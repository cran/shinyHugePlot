#' R6 class of abstract down-sampler
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
#' @description
#' An abstract class for the down-sampler,
#' which defines the structure of the class and
#' is not available on a stand-alone basis.

# class decralation -------------------------------------------------------

abstract_downsampler <- R6::R6Class(
  "abstract_downsampler",

# public members ----------------------------------------------------------
  public = list(
    #' @field figure plotly object.
    figure = NULL,

    # constructor ---------------------------------------------------------
    #' @description
    #' Constructing an abstract down-sampler.
    #' @param figure Plotly structure that will be down-sampled.
    #' @param is_downsample Boolean.
    #' Whether down-sampling is done. By default \code{TRUE}.
    #' @param n_out Integer or numeric.
    #' The number of samples shown after down-sampling. By default 1000.
    #' @param aggregator An instance of an R6 class for aggregation.
    #' Select one out of
    #' \code{LTTB_aggregator}, \code{min_max_ovlp_aggregator},
    #' \code{min_max_aggregator}, \code{eLTTB_aggregator},
    #' \code{nth_pnt_aggregator}, \code{custom_stat_aggregator},
    #' \code{mean_aggregator}, \code{median_aggregator},
    #' \code{min_aggregator}, \code{max_aggregator},
    #' or \code{custom_func_aggregator}.
    #' By default \code{eLTTB_aggregator}.
    #' @param legend_options Named list, optional.
    #' Names of the elements are \code{prefix_downsample},
    #' \code{suffix_downsample}, \code{is_aggsize_shown}, \code{agg_prefix},
    #' and \code{agg_suffix}.
    #' The \code{prefix_downsample} and \code{suffix_downsample}
    #' will be added to the legend name when the traces are down-sampled.
    #' By default, prefix is a bold orange \code{[R]} and suffix is none.
    #' The \code{is_aggsize_shown} is boolean.
    #' Whether the mean aggregation bin size will be added to the legend name.
    #' By default \code{TRUE}.
    #' The \code{agg_prefix} and \code{agg_suffix} are employed to show
    #' the mean aggregation size
    #' @param tz Character, optional.
    #' Time zone used to display time-series data.
    #' By default \code{Sys.timezone()}.
    #'
    initialize = function(
      figure = plotly::plot_ly(),
      is_downsample = TRUE,
      n_out = 1000L,
      aggregator = eLTTB_aggregator$new(),
      legend_options = list(
        downsample_prefix = '<b style="color:sandybrown">[R]</b> ',
        downsample_suffix = "",
        is_aggsize_shown  = TRUE,
        agg_prefix        = '<i style="color:#fc9944"> ~',
        agg_suffix        = "</i>"
      ),
      tz = Sys.timezone()
    ) {

      # check classes and lengths of the arguments
      assertthat::assert_that(inherits(figure, "plotly"))
      assertthat::assert_that(
        length(is_downsample) == 1 & length(n_out) == 1
        )

      assertthat::assert_that(inherits(is_downsample, "logical"))
      assertthat::assert_that(
        inherits(n_out, "integer") |
          inherits(n_out, "numeric")
        )
      assertthat::assert_that(inherits(aggregator, "R6"))
      assertthat::assert_that(inherits(tz, "character"))

      assertthat::assert_that(
        is.null(legend_options$downsample_prefix) ||
          length(legend_options$downsample_prefix) == 1
        )
      assertthat::assert_that(
        is.null(legend_options$downsample_suffix) ||
          length(legend_options$downsample_suffix) == 1
        )
      assertthat::assert_that(
        is.null(legend_options$is_aggsize_shown)  ||
          length(legend_options$is_aggsize_shown) == 1
        )

      # set private properties
      private$n_out_default      <- n_out
      private$aggregator_default <- aggregator
      private$tz                 <- tz

      for (opt in names(private$legend_options)) {
        if (!is.null(legend_options[[opt]])) {
          private$legend_options[[opt]] <- legend_options[[opt]]
        }
      }

      # count the number of traces and prepare the data
      if (!is.null(figure$x$subplot) && figure$x$subplot) {
        figure <- plotly::subplot(figure) # necessary to reset the axes
        n_series <- length(figure$x$attrs)
        plot_data <- figure$x$data

      } else {

        # delete blank data that is frequently included in the plotly obj
        if (is.null(figure$x$attrs[[1]][["type"]]) &&
            length(figure$x$attrs) > 1) {
          figure$x$attrs[[1]] <- NULL
          blank_data <- figure$x$attrs[[1]]
        }

        # extract attrs to be evaluated
        attrs_to_eval <- figure$x$attrs[
          purrr::map_lgl(figure$x$attrs, ~!inherits(.x, "plotly_eval"))
        ]

        n_series <- length(attrs_to_eval)

        # get the data to be plotted
        if (n_series > 0) {
          plot_data <- purrr::imap(
            attrs_to_eval,
            function(x, y) {
              d <- plotly::plotly_data(figure, y)
              purrr::map(
                x,
                function(x) {
                  if (lazyeval::is_formula(x)) {
                    ex <- lazyeval::f_eval(x, d)
                  } else {
                    ex <- x
                  }
                  return(ex)
                }
              )
            }
          )
        }

      }

      assertthat::assert_that(
        n_series > 0,
        msg = "No traces are found in the plotly figure"
        )

      self$figure <- figure

      # initial downsampling
      if (is_downsample) {

        self$figure$x$attrs <- NULL
        self$figure$x$data <- NULL

        for (i in 1:n_series) { # repeat using unique ids
          do.call(
            self$add_trace, # private add_trace with downsampling
            c(plot_data[[i]], tz = tz)
          )
        }


      } # end of down-sampling
    }, #end of initialization

    #' @description
    #' Adds a trace to the figure (\code{self$figure}) and returns nothing.
    #' @param ... Arguments passed along to the plotly trace.
    #' (e.g., \code{id}, \code{x}, \code{y},
    #' \code{data}, \code{type}, \code{mode}).
    #' @param n_out Integer, optional.
    #' The max number of samples that are shown in the figure.
    #' By default, \code{private$n_out_default}.
    #' @param aggregator An instance of an R6 class for aggregation.
    #' The aggregator used for downsampling.
    #' By default, \code{private$aggregator_default}.
    #' @param hf_x,hf_y,hf_text,hf_hovertext Optional.
    #' The original high frequency data for \code{x}, \code{y},
    #' \code{text} and \code{hovertext}.
    #' @param tz Character, optional.
    #' The timezone used in the plotly. By default, \code{Sys.timezone()}.
    add_trace = function(
      ..., n_out = NULL, aggregator = NULL,
      hf_x = NULL, hf_y = NULL, hf_text = NULL, hf_hovertext = NULL,
      tz = Sys.timezone()
      ) {

      # set the parameters that have default values
      if (is.null(n_out)) n_out <- private$n_out_default
      if (is.null(aggregator)) aggregator <- private$aggregator_default

      # get attributes as trace object
      trace <- list(...)

      # add trace name and unique id, if it is not defined
      trace$uid <- basename(tempfile(pattern = ""))
      # add name if it does not exist
      if (is.null(trace$name)) {
        existing_id_max <- purrr::map_chr(private$orig_datalist, ~.x$name) %>%
          str_subset("^trace\\s[0-9]+$") %>%
          str_extract("[0-9]+") %>%
          as.integer() %>%
          max(-1)
        trace$name <- paste("trace", existing_id_max + 1)
      }

      # stop if there are no y values
      if (is.null(trace[["y"]]) && is.null(hf_y)) {
        message(paste(trace$name, "does not have y values"))
        return()
      }

      # register original data
      private$set_orig_datalist(
        trace = trace,
        hf_x = hf_x, hf_y = hf_y,
        hf_text = hf_text, hf_hovertext = hf_hovertext,
        tz = tz,
        n_out = n_out, aggregator = aggregator
        )

      # conduct down-sampling
      if (trace$type %in% c("scatter", "scattergl")) {
        trace <- private$aggregate_trace(trace)
      } else {
        message(
          paste(
            "Down-sampling is only applied to scatter / scattergl type.",
            trace$name, "(", trace$type, "type) was not down-sampled."
          )
        )

        for (elem in c("x", "y", "text", "hovertext")){
          trace[[elem]] <- private$orig_datalist$data[[elem]]
        }

      }

      # plotly is not compatible with 64-bit nanotime,
      # the datatime is converted to the text compatible with
      # the plotly requirements
      # only the UTC timezone is applicable

      if (inherits(trace$x, "nanotime")) {
        trace$x <- nanotime_to_plotlytime(trace$x, private$tz)
        axis_type <- "date"
      } else {
        axis_type <- "linear"
      }

      if (!is.null(trace[["ylwr"]]) && !is.null(trace[["yupr"]])) {
        range_trace <- private$get_range_trace(trace)

        trace[["ylwr"]] <- NULL
        trace[["yupr"]] <- NULL

        fig <- do.call(
          plotly::add_trace, c(p = list(self$figure), range_trace)
        ) %>%
          plotly::subplot()

        fig$x$attrs <- NULL # necessary to avoid wrond dataset

        self$figure <- fig

      }

      fig <- do.call(plotly::add_trace, c(p = list(self$figure), trace)) %>%
        plotly::subplot()

      fig$x$attrs <- NULL # necessary to avoid wrond dataset

      xaxis_name <- paste0("x", trace$xaxis) %>%
        stringr::str_replace("^xx", "x") %>%
        stringr::str_replace("^x", "xaxis")

      if (axis_type == "date") {
        fig$x$layout[[xaxis_name]]$type <- "date"
      }

      if (is.null(fig$x$layout$showlegend) ||
          fig$x$layout$showlegend == FALSE) {
        fig$x$layout$showlegend <- TRUE
      }

      self$figure <- fig

      invisible()
    },

    #' @description
    #' Returns the list of the data used for the plotly figure.
    get_figure_data = function() {
      return(self$figure$x$data)
    }
  ), # end of the public member

# private members ---------------------------------------------------------

  private = list(
    # @field orig_datalist High-frequency data before down-sampling
    orig_datalist = list(),
    # @field n_out The number of samples shown after down-sampling
    n_out_default = 0L,
    # @field legend_options Settings for the trace names
    legend_options = list(
      downsample_prefix = '<b style="color:sandybrown">[R]</b> ',
      downsample_suffix = "",
      is_aggsize_shown  = TRUE,
      agg_prefix        = '<i style="color:#fc9944"> ~',
      agg_suffix        = "</i>"
    ),
    # @field aggregator An instance created from
    # R6 FigureSeriesAggregator class.
    aggregator_default = NULL,
    # @field tz Time zone employed to show time-series data
    tz = Sys.timezone(),

    # the functions below are set using set method
    # construct_update_data
    # update_fig_env
    # aggregate_trace

    # set orig_datalist ----------------------------------------------------


    set_orig_datalist = function(trace, hf_x = NULL, hf_y = NULL,
             hf_text = NULL, hf_hovertext = NULL,
             tz = Sys.timezone(),
             n_out, aggregator) {
      # load x, y, text and hovertext --------------------------------------

      # high-frequency values
      replace_null_x <- function(x = NULL, y = NULL) {
        if (is.null(x) && !is.null(y)) return(y)
        return(x)
      }

      hf_x         <- replace_null_x(hf_x,         trace$x)
      hf_y         <- replace_null_x(hf_y,         trace$y)
      hf_text      <- replace_null_x(hf_text,      trace$text)
      hf_hovertext <- replace_null_x(hf_hovertext, trace$hovertext)

      # modification regarding x

      # fill hf_x if it is NULL
      if (is.null(hf_x)) hf_x <- seq_along(hf_y)

      # datatype change to nanotime (it may be implicity changed to integer64)
      if (bit64::is.integer64(hf_x)) {
        hf_x <- nanotime::as.nanotime(hf_x)
      } else if (inherits(hf_x, "POSIXt")) {
        hf_x <- nanotime::as.nanotime(hf_x)
      } else if (inherits(hf_x, "character")) {
        tryCatch(
          {
            hf_x <- nanotime::nanotime(hf_x, tz = tz)
            message("X values are converted to datetime.")
          },
          error = function(e) {invisible()},
          silent = TRUE
        )
      }

      # check regarding y

      if (is.null(hf_y)) {
        message(paste("trace(", trace$name, ") has no y values"))
        return(p)
      }


      # check regarding x and y
      assertthat::assert_that(
        is.atomic(hf_x) & is.atomic(hf_y),
        msg = "Vector-datatype data is requred (i.e., hf_x and hf_y)."
      )

      assertthat::assert_that(
        length(hf_x) == length(hf_y),
        msg = "x and y have different length!"
      )


      # create data.table of which key is x
      # and remove NA y values
      hf_d <- data.table::data.table(
        x = hf_x,
        y = hf_y,
        text = hf_text,
        hovertext = hf_hovertext,
        key = "x"
      )
      hf_d <- hf_d[!is.na(hf_d$y), ]

      n_samples <- nrow(hf_d)

      axis_type <- if (inherits(hf_d$x, "nanotime")) "date" else "linear"

      # register orig_datalist
      private$orig_datalist <- c(
        private$orig_datalist,
        list(list(
          name = trace$name,
          n_out = n_out,
          data = hf_d,
          axis_type = axis_type,
          aggregator =  aggregator
        )) %>% setNames(trace$uid)
      )
    },

  # Construct the to-be-updated front-end data, based on the layout change.
  #
  # It takes the \code{relayout_order} from the front-end figure
  # and returns the updated data of the traces.
  construct_update_data = function(relayout_order = list()) {

    if (is.null(relayout_order[[1]])) return()

    message(paste(
      paste(
        "relayout order: {",
        paste(
          paste(names(relayout_order), relayout_order, sep = ":"),
          collapse = " "
          ),
        "}"
      )
    ))

    if (!any(stringr::str_detect(names(relayout_order), "xaxis"))) return()

    df_xaxes <-  tibble(
      trace_idx = seq_along(self$figure$x$data) - 1,
      trace_uid = purrr::map_chr(self$figure$x$data, ~.x$uid),
      xaxis = purrr::map_chr(
        self$figure$x$data,
        ~if (!is.null(.x$xaxis)) {.x$xaxis} else {"x"}
      ) %>%
        stringr::str_replace("^x", "xaxis")
    )


    traces_relayout_order <- tibble::tibble(
      value = relayout_order,
      xaxis = str_extract(names(relayout_order), "^xaxis[0-9]*"),
      type  = str_remove(names(relayout_order), "^xaxis[0-9]*\\.") %>%
        str_replace("range\\[0\\]", "start") %>%
        str_replace("range\\[1\\]", "stop")
    ) %>%
      dplyr::filter(!is.na(xaxis)) %>%
      tidyr::pivot_wider(names_from = type, values_from = value) %>%
      tidyr::unnest(matches("start|stop|autorange|showspikes")) %>%
      dplyr::left_join(
        dplyr::mutate(., start = NA, stop = NA, autorange = NA, showspikes = NA)
        ) %>%
      suppressMessages() %>%
      dplyr::filter(
        (!is.na(start) & !is.na(stop)) |
          (!is.na(autorange) && !is.na(showspikes))
      ) %>%
      dplyr::select(xaxis, start, stop) %>%
      dplyr::left_join(df_xaxes, by = "xaxis") %>%
      tidyr::nest(trace_idx = trace_idx) %>%
      dplyr::mutate(trace_idx = purrr::map(trace_idx, ~.x$trace_idx))

    traces_output <- traces_relayout_order %>%
      tidyr::nest(args = -trace_idx) %>%
      dplyr::mutate(
        trace = purrr::map2(
          args, trace_idx,
          ~private$aggregate_trace(
            trace = list(index = .y, uid = .x$trace_uid),
            start = .x$start, end = .x$stop
            )
        )
      )

    return(traces_output)

  },


  # Aggregation of the passed \code{trace}.
  # Before this function, \code{private$orig_datalist} must be registered.
  # Using \code{trace$uid}, this function finds the corresponding original data,
  # aggregates according to the method registered,
  # and also generates the name that is shown in the legend.
  # It returns aggregated trace.
  aggregate_trace = function(trace, start = NULL, end = NULL) {

    # find the high-freq data using trace and uid
    assertthat::assert_that(
      !is.null(private$orig_datalist[[trace$uid]]),
      msg = paste("High-frequency data was not found (", trace$name, ")")
    )

    orig_datalist  <- private$orig_datalist[[trace$uid]]
    orig_datatable <- orig_datalist$data

    # if start and end parameters are set, data is extracted
    if (!is.null(start) && !is.na(start) &&
        !is.null(end) && !is.na(end)) {
      if (orig_datalist$axis_type == "date") {
        start <- plotlytime_to_nanotime(start, tz = private$tz)
        end   <- plotlytime_to_nanotime(end,   tz = private$tz)
      }
      orig_datatable <- orig_datatable[x >= start & x <= end]
    }

    # Downsample the x-y data
    aggregation_results <- orig_datalist$aggregator$
      aggregate(
        orig_datatable$x, orig_datatable$y, orig_datalist$n_out
      ) %>%
      data.table::as.data.table() %>%
      data.table::setkey(x)

    # Downsample the text, if it exists
    if ("text" %in% colnames(orig_datatable)) {
      aggregation_results <- merge(
        aggregation_results,
        orig_datatable[, c("x", "text")],
        all.x = TRUE)
    }

    # Downsample the hovertext, if it exists
    if ("hovertext" %in% colnames(orig_datatable)) {
      aggregation_results <- merge(
        aggregation_results,
        orig_datatable[, c("x", "hovertext")],
        all.x = TRUE
      )
    }

    for (elem in names(aggregation_results)) {
      trace[[elem]] <- aggregation_results[[elem]]
      if (inherits(trace[[elem]], "nanotime")) {
        trace[[elem]] <- nanotime_to_plotlytime(trace[[elem]], private$tz)
      }
    }


    # Add the mean aggregation bin size to the trace name
    if (nrow(orig_datatable) > orig_datalist$n_out &&
        private$legend_options$is_aggsize_shown) {

      name <- paste0(
        private$legend_options$downsample_prefix,
        orig_datalist$name,
        private$legend_options$downsample_suffix
      )

      agg_mean <- as.numeric(mean(diff(aggregation_results$x), na.rm = TRUE))

      if (inherits(aggregation_results$x, "nanotime")) {
        agg_mean_chr <- nanosecond_to_label(agg_mean)
      } else {
        agg_mean_chr <- numeric_to_label(agg_mean)
      }

      name <- paste0(
        name,
        private$legend_options$agg_prefix,
        agg_mean_chr,
        private$legend_options$agg_suffix
      )
    } else {

      message(
        paste(
          "Down-sampling is not applied to",
          orig_datalist$name,
          "because the length (",
          nrow(orig_datatable),
          ") was less than the threshold."
        )
      )

      name <- orig_datalist$name
    }

    trace$name <- name

    return(trace)
  },

  get_range_trace = function(trace) {
    range_trace <- trace

    range_trace[["x"]] <- c(trace[["x"]], rev(trace[["x"]]))
    range_trace[["y"]] <- c(trace[["ylwr"]], rev(trace[["yupr"]]))
    range_trace[["text"]] <- paste(
      paste0("x: ",    range_trace[["x"]]),
      paste0("y: ",    range_trace[["y"]]),
      paste0("ylwr: ", c(trace[["ylwr"]], rev(trace[["ylwr"]]))),
      paste0("yupr: ", c(trace[["yupr"]], rev(trace[["yupr"]]))),
      sep = "<br>"
      )

    range_trace[["ylwr"]] <- NULL
    range_trace[["yupr"]] <- NULL
    range_trace[["fill"]] <- "toself"
    range_trace[["opacity"]] <- 0.5
    range_trace[["hoveron"]] <- "points"

    return(range_trace)
  }


  ) # end of the private member
) # end of the R6 class
