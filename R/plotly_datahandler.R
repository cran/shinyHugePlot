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
#' @description
#' A class for handling plotly data,
#' which defines functions and be used in the downsampler class

# class decralation -------------------------------------------------------

plotly_datahandler <- R6::R6Class(
  "plotly_datahandler",

# public members ----------------------------------------------------------
  public = list(
    #' @field figure \code{plotly} object.
    figure = NULL,

    # constructor ---------------------------------------------------------
    #' @description
    #' Constructing an abstract down-sampler.
    #' @param figure \code{plotly} object.
    #' The traces of this object will be down-sampled.
    #' @param legend_options Named list, optional.
    #' Names of the elements are \code{name_prefix},
    #' \code{name_suffix}, \code{xdiff_prefix},
    #' and \code{xdiff_suffix}.
    #' \code{name_prefix} and \code{name_suffix}
    #' will be added to the name of the trace when the down-sampling is applied.
    #' By default, prefix is a bold orange \code{[R]} and suffix is none.
    #' \code{xdiff_prefix} and \code{xdiff_suffix} are employed to show
    #' the mean aggregation size of the down-sampling.
    #' @param tz Character, optional.
    #' Time zone used to display time-series data.
    #' By default \code{Sys.timezone()}.
    #'
    initialize = function(
      figure = plotly::plot_ly(),
      legend_options = list(
        name_prefix  = '<b style="color:sandybrown">[R]</b> ',
        name_suffix  = "",
        xdiff_prefix = '<i style="color:#fc9944"> ~',
        xdiff_suffix = "</i>"
      ),
      tz = Sys.timezone()
    ) {
      # check classes and lengths of the arguments
      assertthat::assert_that(inherits(figure, "plotly"))
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

      private$tz <- tz

      # register the figure
      self$figure <- figure

      # build and relayout the figure
      # note that first build may take a lot of time
      if (is.null(figure$x$data)) {
        warning("building plotly data for the first time may take much time")
      }
      x_title <- figure$x$layout$xaxis$title
      y_title <- figure$x$layout$yaxis$title

      figure <- plotly::subplot(
        figure,
        titleX = !is.null(x_title) && is.character(x_title),
        titleY = !is.null(y_title) && is.character(y_title)
      )

      # convert the trace data to tibble
      private$set_trace_df_default()
      traces_df <- self$plotly_data_to_df(figure$x$data)

      # delete the data
      self$figure$x$attrs <- NULL
      self$figure$x$data <- purrr::map2(
        figure$x$data, traces_df$data,
        ~.x[setdiff(names(.x), colnames(.y))]
        )

      # setting of the show legend
      if (is.null(self$figure$x$layout$showlegend) ||
          self$figure$x$layout$showlegend == FALSE) {
        self$figure$x$layout$showlegend <- TRUE
      }

      # add the trace to the self$figure
      self$set_trace_data(traces_df = traces_df)
    }, #end of initialization

    #' @description
    #' Add or overwrite the data of the traces.
    #' @param ... Arguments constitute a plotly attribute, optional.
    #' (e.g., \code{id}, \code{x}, \code{y},
    #' \code{type}, \code{mode}).
    #' Note that un-build data will be built by \code{plotly_build}.
    #' @param traces_df Data frame, optional.
    #' Data frame that contains all the attributes of a plotly trace
    #' in each row. See \code{self$orig_data} field.
    #' If \code{traces_df} is given, arguments in \code{...} are neglected.
    #' @param append Boolean, optional.
    #' Whether the data is append or overwrite. By default, \code{FALSE}
    #' (the traces are overwritten).
    set_trace_data = function(
      ..., traces_df = NULL, append = FALSE
      ) {

      # make traces_df, if not given
      if (is.null(traces_df)) {
        traces_df <- private$plotly_build_simple(list(...)) %>%
          self$plotly_data_to_df()
      }

      if (nrow(traces_df) == 0) {
        message("no data is registered because no data are given")
        return()
      }

      if (append) {
        private$traces_df <- dplyr::bind_rows(
          private$traces_df,
          traces_df
        )
      } else {
        private$traces_df <- traces_df
      }

      invisible()
    },

    #' @description
    #' Covert plotly data to data frame, with generating new uid
    #' @param plotly_data List.
    #' The elements are named list that represents plotly trace,
    #' all of which must have \code{type}.
    #' @param use_datatable Boolean.
    #' If it is \code{TRUE}, data such as \code{x} and \code{y} are nested
    #' in a \code{data.table}, of which key colomn is \code{x}.
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
          plotly_data, ~.x$type %in% c("scatter", "scattergl")
          )),
        msg = paste0(
          "The types of the all traces must be ",
          paste(private$trace_type_accept, collapse = "or")
        )
      )

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

      # build blank data frame
      blank_df <- self$trace_df_default %>%
        dplyr::filter(purrr::map_lgl(default, ~!is.null(.x))) %>%
        dplyr::select(name, default) %>%
        tidyr::pivot_wider(names_from = "name", values_from = "default") %>%
        tidyr::unnest(everything()) %>%
        .[0,]

      # make a data frame to represent traces
      traces_df <- purrr::map_dfr(
        plotly_data,
        ~purrr::modify_if(.x, ~length(.x) > 1 || is.list(.x), list) %>%
          tibble::as_tibble()
      ) %>%
        dplyr::bind_rows(
          blank_df[setdiff(colnames(blank_df), colnames(.))]
        ) %>%
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

      # change the class and get the axis type
      tryCatch({
        traces_df_newclass <- traces_df %>%
          dplyr::mutate(
            x = purrr::modify_if(
              x, ~inherits(.x, c("integer64", "POSIXt")),
              nanotime::as.nanotime
            ) %>%
              purrr::modify_if(
                ~inherits(.x, "character"),
                ~private$plotlytime_to_nanotime(.x, private$tz)
              ) ,
            y = purrr::modify_if(
              y, ~inherits(.x, "character"),
              factor
            )
          )
      },
        error = function(e) {
          stop("x is not numeric or datetime")
        }
      )

      # if not use_datatable, stop here
      if (!use_datatable) return(traces_df_newclass)

      # nest data columns and defined the data as data.table

      # get column names
      colnames_for_data <- intersect(
        colnames(traces_df),
        dplyr::filter(self$trace_df_default, data)$name
      )

      # nest them and change to data.table
      traces_df_data <- traces_df_newclass %>%
        tidyr::nest(data = colnames_for_data) %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            ~tidyr::unnest(.x, everything()) %>%
              data.table::data.table(key = "x")
          )
        )

      return(traces_df_data)

    }
  ), # end of the public member

  active = list(
    #' @field orig_data Data frame of the plotly traces.
    orig_data = function() private$traces_df,
    #' @field trace_df_default Data frame that represents default values
    #' of the plotly traces.
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

    set_trace_df_default = function() {
      x_class <- c("numeric", "integer", "integer64", "POSIXt", "character")
      y_class <- c("numeric", "integer", "character", "factor")
      name_class <- c("character", "factor")
      lgrp_class <- c("character", "factor")

      private$trace_df_def <- tibble::tribble(
        ~name,         ~required, ~data, ~default,     ~class,
        "x",           TRUE,      TRUE,  NULL,         x_class,
        "y",           TRUE,      TRUE,  NULL,         y_class,
        "text",        FALSE,     TRUE,  NULL,         c("character"),
        "hovertext",   FALSE,     TRUE,  NULL,         c("character"),
        "xaxis",       FALSE,     FALSE, "x",          c("character"),
        "name",        FALSE,     FALSE, character(0), name_class,
        "legendgroup", FALSE,     FALSE, character(0), lgrp_class
      )
    },

    # settings for the trace names
    legend_options = list(
      name_prefix  = '<b style="color:sandybrown">[R]</b> ',
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
      x_diff_mean <- as.numeric(mean(diff(x), na.rm = TRUE))

      x_diff_chr <- dplyr::if_else(
        inherits(x, "nanotime"),
        private$nanosecond_to_label(x_diff_mean),
        private$numeric_to_label(x_diff_mean)
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
      thr <- 0.1
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
      thr     <- 0.1
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
