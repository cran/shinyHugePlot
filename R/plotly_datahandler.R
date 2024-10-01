#' R6 class for handling plotly data
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table setkey
#' @importFrom dplyr %>% case_when filter if_else bind_rows select left_join
#' @importFrom stringr str_replace str_sub str_subset
#' @importFrom stringr str_extract str_remove str_detect
#' @importFrom purrr map map_chr map_dfr compact pmap
#' @importFrom tibble tibble as_tibble
#' @importFrom bit64 as.integer64
#' @importFrom nanotime as.nanotime
#' @importFrom plotly add_trace subplot
#' @importFrom tidyr unnest pivot_wider
#' @importFrom lazyeval f_eval
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery
#' @importFrom duckdb duckdb
#' @description
#' A class for handling \code{plotly} data,
#' which defines functions used in the \code{downsampler} class

# class decralation -------------------------------------------------------

plotly_datahandler <- R6::R6Class(
  "plotly_datahandler",

# public members ----------------------------------------------------------
  public = list(
    #' @field figure \code{plotly} object.
    figure = NULL,

    # constructor ---------------------------------------------------------
    #' @description
    #' Constructing an instance.
    #' The data contained in a \code{plotly} object (\code{figure} argument)
    #' will be included in the instance (as a reference).
    #' @param figure \code{plotly} object.
    #' The traces of this object will be down-sampled.
    #' @param srcs,srcs_ext,formula Character and formula, optional.
    #' \code{srcs} is the path of the source data (or directory).
    #' When a directory is specified, \code{srcs_ext} is the extension of the source file.
    #' \code{formula} is the formula to extract the data from the source data.
    #' @param legend_options Named list, optional.
    #' Names of the elements are \code{name_prefix},
    #' \code{name_suffix}, \code{xdiff_prefix},
    #' and \code{xdiff_suffix}.
    #' \code{name_prefix} and \code{name_suffix}
    #' will be added to the name of the trace when the down-sampling is applied.
    #' By default, prefix is a bold orange \code{[S]} and suffix is none.
    #' \code{xdiff_prefix} and \code{xdiff_suffix} are employed to show
    #' the mean aggregation size of the down-sampling.
    #' @param tz Character, optional.
    #' Time zone used to display time-series data.
    #' By default \code{Sys.timezone()}.
    #' @param use_light_build Boolean, optional.
    #' Whether \code{plotly_build_light} is used.
    #' It quickly build scatter-type \code{plotly} data.
    #' By default, \code{TRUE}.
    #'
    initialize = function(
      figure = NULL,
      srcs = NULL,
      formula = NULL,
      srcs_ext = NULL,
      legend_options = list(
        name_prefix  = '<b style="color:sandybrown">[S]</b> ',
        name_suffix  = "",
        xdiff_prefix = '<i style="color:#fc9944"> ~',
        xdiff_suffix = "</i>"
      ),
      tz = Sys.timezone(),
      use_light_build = TRUE
    ) {

      # check classes and lengths of the arguments
      assertthat::assert_that(is.null(figure) || inherits(figure, "plotly"))
      assertthat::assert_that(is.null(srcs) || inherits(srcs, "character"))
      assertthat::assert_that(inherits(legend_options, "list"))
      assertthat::assert_that(inherits(tz, "character"))

      assertthat::assert_that(
        all(purrr::map_lgl(legend_options, ~length(.x) == 1))
      )

      for (opt in names(private$legend_options)) {
        if (!is.null(legend_options[[opt]])) {
          private$legend_options[[opt]] <- legend_options[[opt]]
        }
      }

      if (!is.null(figure) & !is.null(srcs)) {
        warning("Both figure and srcs are given. srcs is neglected.")
      }

      private$tz <- tz

      # before registering the data,
      # build the data of the figure
      if (is.null(figure)) {
        figure <- plotly::plot_ly(type = "scatter", mode = "lines") %>%
          plotly_build() %>%
          plotly::subplot()

        figure$x$data <- list()

      } else {
        if (is.null(figure$x$data)) {
          if (use_light_build) {
            figure <- plotly_build_light(figure)
          } else {
            message("building plotly data for the first time may take much time")
            figure <- plotly::plotly_build(figure)
          }
        }

        x_title <- figure$x$layout$xaxis$title
        y_title <- figure$x$layout$yaxis$title

        # note: this subplot may change the layout
        figure <- plotly::subplot(
          figure,
          titleX = !is.null(x_title) && is.character(x_title),
          titleY = !is.null(y_title) && is.character(y_title)
        )
      }


      # convert the trace data to data frame
      # it will be registered after adjusting figure data,
      # because self$figure must be registered before doing that
      private$set_trace_df_default()
      traces_df <- self$plotly_data_to_df(figure$x$data)

      if (nrow(traces_df) == 0 & !is.null(srcs)) {
        assertthat::assert_that(!is.null(formula) && inherits(formula, "formula"))
        traces_df <- self$srcs_to_df(fml = formula, srcs = srcs, srcs_ext = srcs_ext)
      }

      # change figure data according to the traces_df, if necessary
      figure$x$attrs <- NULL
      if (length(figure$x$data) > 0 && nrow(traces_df) > 0 && "data" %in% colnames(traces_df)) {
        figure$x$data <- purrr::map2(
          figure$x$data, traces_df$data,
          ~.x[setdiff(names(.x), colnames(.y))]
          )
      } else if (length(figure$x$data) == 0) {

        figure$x$data <- purrr::pmap(
          traces_df,
          function(xaxis, ...) {
            plotly::plotly_build(
              plotly::plot_ly(
                type = "scatter", mode = "lines", xaxis = xaxis
              )
            )$x$data[[1]]
          }
        )
      } else if (nrow(traces_df) == 0) {
        figure$x$data <- plotly::plotly_build(
          plotly::plot_ly(type = "scatter", mode = "lines")
          )$x$data
      }

      # setting of the show legend
      if (is.null(figure$x$layout$showlegend) ||
          figure$x$layout$showlegend == FALSE) {
        figure$x$layout$showlegend <- TRUE
      }

      # finally register the figure and the data
      self$figure <- figure
      self$set_trace_data(traces_df = traces_df)

    }, #end of initialization

    #' @description
    #' In the instance, data is contained as a data frame
    #' (see \code{self$orig_data} for detailed information).
    #' Using this method, the data can be added or overwritten.
    #' If a data frame (\code{traces_df} argument) is given, it will be
    #' added to \code{self$orig_data} or reassigned as \code{self$orig_data}.
    #' If attributes to construct a \code{plotly} object (\code{...} argument)
    #' are given, a data frame is constructed and used.
    #' @param ... Arguments to constitute a \code{plotly} attributes, optional.
    #' For instance, \code{x}, \code{y}, \code{type}, and \code{mode}
    #' are applicable. See \code{plotly::plot_ly}.
    #' @param traces_df Data frame, optional.
    #' Data frame whose format is agreed with \code{self$orig_data}.
    #' If \code{traces_df} is given, arguments in \code{...} are neglected.
    #' @param append Boolean, optional.
    #' Whether the data is append or overwrite. By default, \code{FALSE}
    #' (the traces are overwritten).
    set_trace_data = function(
      ..., traces_df = NULL, append = FALSE
      ) {

      # make traces_df, if not given
      if (is.null(traces_df)) {
        message("data is built with plotly_build_light")
        traces_df <- plotly::plot_ly(...) %>%
          plotly_build_light() %>%
          .$x %>% .$data %>%
          self$plotly_data_to_df()
      }

      if (append) {
        private$traces_df <- dplyr::bind_rows(
          private$traces_df,
          traces_df
        )
      } else {
        private$traces_df <- traces_df
      }

      # set figure xaxis
      if (nrow(traces_df) > 0) {
        traces_axis <- private$traces_df %>%
          dplyr::mutate(
            xaxis_type = purrr::map_chr(
              data,
              ~if_else(inherits(.x$x, "nanotime"), "date", "linear")
            )
          ) %>%
          dplyr::distinct(xaxis, xaxis_type) %>%
          dplyr::group_by(xaxis) %>%
          dplyr::mutate(cnt = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(xaxis = stringr::str_replace(xaxis, "^x", "xaxis"))

        if (any(traces_axis$cnt > 1)) {
          warning("types of the x values are not consistent")
          traces_axis <- dplyr::distinct(trace_axis, xaxis, .keep_all = TRUE)
        }

        args <- NULL
        for (i in 1:nrow(traces_axis)) {
          args[[traces_axis$xaxis[i]]][["type"]] <- traces_axis$xaxis_type[i]
        }

        self$figure <- do.call(plotly::layout, c(list(self$figure), args))
      }

      invisible()
    },

    #' @description
    #' Covert the data contained in \code{srcs} file(s) to a duck-db.
    #' A minimum data and the path of the database will be returned.
    #' @param fml Formula.
    #' The formula to extract the data from the source data.
    #' @param srcs Character.
    #' The name of the source file (e.g. data.parquet) or the directory can be specified.
    #' @param srcs_ext Character, optional.
    #' The extension of the source file, if \code{srcs} is a directory.
    srcs_to_df = function(fml, srcs, srcs_ext = NULL) {

      assertthat::assert_that(file.exists(srcs) || dir.exists(srcs))
      assertthat::assert_that(
        length(labels(terms(fml))) == 1,
        msg = "only one term is acceptable"
      )

      srcs_name <- basename(srcs) %>% str_remove("\\..*$")

      if (dir.exists(srcs)) {
        srcs_path <- paste0(
          "'", str_remove(srcs, "/?$"),
          "/**/*", srcs_ext, "'"
        )
      } else {
        srcs_path <- paste0("'", srcs, "'")
      }

      db_path <- paste0(tempfile("db"), "/", srcs_name, ".duckdb")
      message("The duck-db is created at ", db_path)

      if (file.exists(db_path)) {
        file.remove(db_path)
      } else if (!dir.exists(dirname(db_path))) {
        dir.create(dirname(db_path))
      }

      x_name <- labels(terms(fml))
      y_name <- all.vars(fml)[1]

      # create a table in the database from the source data
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

        DBI::dbExecute(
          con,
          paste0(
            "create table original_raw_data",
            " as select ", x_name, " as x, ", y_name, " as y from ", srcs_path
          )
        )

        col_info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(original_raw_data)"))

        x_type <- col_info["type"][col_info["name"] == "x"]
        y_type <- col_info["type"][col_info["name"] == "y"]

        DBI::dbExecute(
          con,
          paste0(
            "create table original_data",
            "(x ", x_type, " primary key, y ", y_type, ")"
          )
        )

        DBI::dbExecute(
          con,
          paste0(
            "create table original_grouped_data",
            " as select x, first(y) as y",
            " from original_raw_data ", "group by x order by x"
          )
        )

        DBI::dbExecute(
          con,
          paste0(
            "insert into original_data(x, y) ",
            "select * from original_grouped_data"
          )
        )

      DBI::dbDisconnect(con)

      if (stringr::str_detect(x_type, "^TIME") | stringr::str_detect(x_type, "^DATE")) {
        x_initval <- nanotime::as.nanotime(Sys.time())
      } else {
        x_initval <- 0.0
      }

      y_initval <- 0.0

      data_init <- data.table::data.table(
        x = x_initval, y = y_initval
      )

      traces_df_output <- data.table::data.table(
        uid = stringr::str_replace(basename(tempfile()), "^file", "ar"),
        name = y_name,
        showlegend = TRUE,
        legendgroup = NA,
        customdatasrc = list(db_path),
        xaxis = "x",
        yaxis = "y",
        data = list(data_init)
      )

      return(traces_df_output)
    },


    #' @description
    #' Covert the data contained in \code{plotly} object to a data frame.
    #' A unique id (\code{uid}) is granted to each data.
    #' The data frame will be returned.
    #' @param plotly_data List.
    #' The list whose elements are named list representing \code{plotly} traces.
    #' All elements must have elements named \code{type}.
    #' @param use_datatable Boolean.
    #' If it is \code{TRUE}, data such as \code{x} and \code{y} are nested
    #' in a \code{data.table}, of which key column is \code{x}.
    #' By default, \code{TRUE}.
    plotly_data_to_df = function(plotly_data, use_datatable = TRUE) {

      # check argument
      assertthat::assert_that(inherits(plotly_data, "list"))
      assertthat::assert_that(
        all(purrr::map_lgl(plotly_data, ~!is.null(.x$type))),
        msg = "All traces must have 'type' attribute"
      )
      assertthat::assert_that(
        all(purrr::map_lgl(
          plotly_data, ~.x$type %in% private$trace_type_accept
          )),
        msg = paste0(
          "The types of the all traces must be ",
          paste(private$trace_type_accept, collapse = " or ")
        )
      )

      # check trace members
      purrr::map(
        plotly_data,
        function(trace) {
          purrr::pmap(
            self$trace_df_default,
            function(name, required, class, ...) {
              if (required) {
                assertthat::assert_that(
                  name %in% names(trace),
                  msg = paste0(name, " is required for all traces")
                )
              }

              if (name %in% names(trace)) {
                assertthat::assert_that(inherits(trace[[name]], class))
              }
            }
          )
          invisible()
        }
      )

      # change the class, if necessary
      tryCatch({
        plotly_data <- purrr::map(
          plotly_data,
          function(trace) {
            if (inherits(trace[["x"]], c("integer64", "POSIXt", "Date"))) {
              trace[["x"]] <- nanotime::as.nanotime(trace[["x"]])
            } else if (inherits(trace[["x"]], "character")) {
              trace[["x"]] <- private$plotlytime_to_nanotime(
                trace[["x"]], private$tz
                )
            }

            if (inherits(trace[["y"]], "character")) {
              trace[["y"]] <- factor(trace[["y"]])
            }
            return(trace)
          }
        )
      },
      error = function(e) {
        stop("x is not numeric or datetime")
      }
      )

      vars_hf <- self$trace_df_default$name[self$trace_df_default$data]

      # make data table and set key at initialization,
      # of which cost is relatively high.
      traces_df <- purrr::map(
        plotly_data,
        function(trace) {

          if (use_datatable) {

            hf_data <- trace[intersect(names(trace), vars_hf)] %>%
              data.table::setDT(key = "x") %>%
              unique(by = "x")

            trace_df <- c(
              trace[setdiff(names(trace), vars_hf)] %>%
                purrr::modify_if(~length(.x) > 1 || is.list(.x), list),
              list(data = list(hf_data))
              ) %>%
              data.table::setDT()
          } else {
            trace_df <- trace %>%
              purrr::modify_if(~length(.x) > 1 || is.list(.x), list) %>%
              data.table::setDT()
          }

          return(trace_df)
        }
      ) %>%
        bind_rows() %>%
        bind_rows(
          self$trace_df_default %>%
            dplyr::filter(purrr::map_lgl(default, ~!is.null(.x))) %>%
            dplyr::select(name, default) %>%
            tidyr::pivot_wider(names_from = "name", values_from = "default") %>%
            tidyr::unnest(everything()) %>%
            .[0,]
        )


      if (nrow(traces_df) > 0) {

        traces_df_output <- traces_df %>%
          dplyr::mutate(
            uid = basename(tempfile(rep("", nrow(.)))),
            name = if_else(
              !is.na(name),
              as.character(name),
              paste("trace", dplyr::row_number())
            ),
            showlegend = dplyr::if_else(
              is.na(legendgroup),
              TRUE,
              !duplicated(legendgroup)
            )
          )
      } else {
        traces_df_output <- traces_df %>%
          bind_rows(
            tibble(uid = "", name = "", showlegend = TRUE) %>%
              .[0,]
          )
      }


      if (use_datatable && nrow(traces_df_output) > 0) {
        message(paste0(
          purrr::map_int(traces_df_output$data, nrow),
          " unique data were obtained from ",
          traces_df_output$name
        ) %>%
          paste(collapse = "\n"))
      }

      return(traces_df_output)

    }
  ), # end of the public member

  active = list(
    #' @field orig_data Data frame representing \code{plotly} traces.
    orig_data = function() private$traces_df,
    #' @field trace_df_default Data frame representing default values
    #' of \code{plotly} traces.
    #' \code{name} column represents the names of the attributes.
    #' \code{required} column represents whether the attributes are necessary
    #' to construct a data frame of a trace.
    #' \code{data} column represents whether the attributes are the data.
    #' \code{default} attributes represents default values of the attributes.
    #' When constructing a data frame of a trace, default values are used
    #' if no values are assigned.
    #' \code{class} column represents the acceptable classes of the attributes.
    trace_df_default = function() private$trace_df_def
  ),

# private members ---------------------------------------------------------

  private = list(

    # traces_df and its default structure
    traces_df = NULL,
    trace_df_def = NULL,
    trace_type_accept = c("scatter", "scattergl", "candlestick"),

    set_trace_df_default = function() {
      x_class <- c("numeric", "integer", "integer64", "POSIXt", "character","Date")
      y_class <- c("numeric", "integer", "character", "factor")
      name_class <- c("character", "factor")
      lgrp_class <- c("character", "factor")

      private$trace_df_def <- tibble::tribble(
        ~name,         ~required, ~data, ~default,     ~class,
        "x",           TRUE,      TRUE,  NULL,         x_class,
        "y",           FALSE,     TRUE,  NULL,         y_class,
        "open",        FALSE,     TRUE,  NULL,         c("numeric", "integer"),
        "high",        FALSE,     TRUE,  NULL,         c("numeric", "integer"),
        "low",         FALSE,     TRUE,  NULL,         c("numeric", "integer"),
        "close",       FALSE,     TRUE,  NULL,         c("numeric", "integer"),
        "text",        FALSE,     TRUE,  NULL,         c("character"),
        "hovertext",   FALSE,     TRUE,  NULL,         c("character"),
        "xaxis",       FALSE,     FALSE, "x",          c("character"),
        "name",        FALSE,     FALSE, character(0), name_class,
        "legendgroup", FALSE,     FALSE, character(0), lgrp_class
      )
    },

    # settings for the trace names
    legend_options = list(
      name_prefix  = '<b style="color:sandybrown">[S]</b> ',
      name_suffix  = "",
      xdiff_prefix = '<i style="color:#fc9944"> ~',
      xdiff_suffix = "</i>"
    ),

    # time zone employed to show time-series data
    tz = Sys.timezone(),

    # compute the name of the trace based on the differences of x values
    cmpt_trace_name = function(name, x) {

      assertthat::assert_that(inherits(name, c("character", "factor")))
      assertthat::assert_that(inherits(x, c("numeric","integer", "integer64")))

      if (length(x) < 2) return(name)

      # mean difference of the x
      x_diff_median <- as.numeric(median(diff(x), na.rm = TRUE))

      x_diff_chr <- dplyr::if_else(
        inherits(x, "nanotime"),
        private$nanosecond_to_label(x_diff_median),
        private$numeric_to_label(x_diff_median)
      )

      name <- paste0(
        private$legend_options$name_prefix,
        name,
        private$legend_options$name_suffix,
        private$legend_options$xdiff_prefix,
        x_diff_chr,
        private$legend_options$xdiff_suffix
      )

      return(name)
    },


    # Generate an aggregation label according to the data interval
    numeric_to_label = function(x) {
      thr <- 0.5
      e3_unit <- 1e3
      e6_unit <- 1e6
      e9_unit <- 1e9
      e12_unit <- 1e12

      output_chr <- dplyr::case_when(
        x > thr * e12_unit ~ sprintf("%.1fT",  x / e12_unit),
        x > thr * e9_unit  ~ sprintf("%.1fG",  x / e9_unit),
        x > thr * e6_unit  ~ sprintf("%.1fM",  x / e6_unit),
        x > thr * e3_unit  ~ sprintf("%.1fK",  x / e3_unit),
        TRUE               ~ sprintf("%.1f",   x)
      )
      return(output_chr)
    },


    # Generate an time-based aggregation label according to nanotime interval
    nanosecond_to_label = function(ns) {
      thr     <- 0.5
      us_unit <- 1e3
      ms_unit <- 1e6
      s_unit  <- 1e9
      m_unit  <- 1e9 * 60
      h_unit  <- 1e9 * 60 * 60
      d_unit  <- 1e9 * 60 * 60 * 24
      w_unit  <- 1e9 * 60 * 60 * 24 * 7
      mo_unit <- 1e9 * 60 * 60 * 24 * 30
      yr_unit <- 1e9 * 60 * 60 * 24 * 365

      output_chr <- dplyr::case_when(
        ns > thr * yr_unit ~ sprintf("%.1fy",  ns / yr_unit),
        ns > thr * mo_unit ~ sprintf("%.1fm",  ns / mo_unit),
        # ns > thr * w_unit  ~ sprintf("%.1fw",  ns / w_unit),
        ns > thr * d_unit  ~ sprintf("%.1fd",  ns / d_unit),
        ns > thr * h_unit  ~ sprintf("%.1fh",  ns / h_unit),
        ns > thr * m_unit  ~ sprintf("%.1fm",  ns / m_unit),
        ns > thr * s_unit  ~ sprintf("%.1fs",  ns / s_unit),
        ns > thr * ms_unit ~ sprintf("%.1fms", ns / ms_unit),
        ns > thr * us_unit ~ sprintf("%.1fus", ns / us_unit),
        TRUE               ~ sprintf("%.1fns", ns)
      )
      return(output_chr)
    },

    # Conversion of nanotime to plotly-style time
    nanotime_to_plotlytime = function(time, tz = private$tz) {
      assertthat::assert_that(
        inherits(time, "nanotime"),
        msg = "Only the nanotime class is acceptable"
      )

      tz_hms <- format(as.POSIXct("1970-01-01", tz), "%z") %>%
        stringr::str_replace("^(\\-?)\\+?([0-9]{1,2})([0-9]{2})", "\\1\\2:\\3:00")

      time_plotly <- (time + nanotime::as.nanoduration(tz_hms)) %>%
        format("%Y-%m-%d %H:%M:%E9S")

      return(time_plotly)
    },

    # Conversion of plotly-style time to nanotime
    # as.POSIXct is necessary because as.nanotime does not accept NA
    plotlytime_to_nanotime = function(time, tz = private$tz) {
      if (inherits(time, "nanotime")) return(time)
      return(nanotime::as.nanotime(as.POSIXct(time, tz = tz)))
    }

  ) # end of the private member
) # end of the R6 class
