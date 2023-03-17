#' R6 class for down-sampling data
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
#' A class for down-sampling data with a large number of samples.
#' An instance contains (the reference of) original data, layout of the figure,
#' and options for aggregating the original data.
#' An interactive plot for displaying large-sized data can be obtained using
#' the figure, down-sampler and its options included in the instance,
#' while making the plot using \code{shiny_hugeplot} function is easier (see examples).
#' See the super class (\code{plotly_datahandler}) to find more members
#' to handle the data in \code{plotly}.
#' @examples
#' \donttest{
#' data(noise_fluct)
#'
#' # example 1 : Easy method using shiny_hugeplot
#' shiny_hugeplot(noise_fluct$time, noise_fluct$f500)
#'
#' # example 2 : Manual method using a downsampler object
#' fig <- plot_ly(
#'   x = noise_fluct$time,
#'   y = noise_fluct$f500,
#'   type = "scatter",
#'   mode = "lines"
#'   ) %>%
#'   layout(xaxis = list(type = "date")) %>%
#'   shinyHugePlot::plotly_build_light()
#'
#' ds <- downsampler$new(
#'   figure = fig,
#'   aggregator = min_max_aggregator$new(interleave_gaps = TRUE)
#' )
#'
#' ui <- fluidPage(
#'   plotlyOutput(outputId = "hp", width = "800px", height = "600px")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   output$hp <- renderPlotly(ds$figure)
#'
#'   observeEvent(plotly::event_data("plotly_relayout"),{
#'     updatePlotlyH(session, "hp", plotly::event_data("plotly_relayout"), ds)
#'   })
#'
#' }
#'
#' shinyApp(ui = ui, server = server)
#'
#'
#' # example 3 : Add another series of which aggregator is different
#'
#' noise_events <- tibble(
#'   time = c("2022-11-09 12:25:50", "2022-11-09 12:26:14"),
#'   level = c(60, 60)
#' )
#'
#' ds$add_trace(
#'   x = noise_events$time, y = noise_events$level, name = "event",
#'   type = "scatter", mode = "markers",
#'   aggregator = null_aggregator$new()
#' )
#' ds$update_trace(reset = TRUE)
#'
#' server <- function(input, output, session) {
#'
#'   output$hp <- renderPlotly(ds$figure)
#'
#'   observeEvent(plotly::event_data("plotly_relayout"),{
#'     updatePlotlyH(session, "hp", plotly::event_data("plotly_relayout"), ds)
#'   })
#'
#' }
#'
#' shinyApp(ui = ui, server = server)
#'
#'
#' }

# class decralation -------------------------------------------------------

downsampler <- R6::R6Class(
  "downsampler",
  inherit = plotly_datahandler,

# public members ----------------------------------------------------------
  public = list(
    # constructor ---------------------------------------------------------
    #' @description
    #' To construct an instance, original data, layout of the figure, and options
    #' for aggregating the original data are necessary.
    #' The original data and the layout of the figure can be given by providing
    #' a \code{plotly} object (\code{figure} argument).
    #' The options for aggregating the original data can be given by providing
    #' an aggregator (\code{aggregator} argument) and the number of samples
    #' (\code{n_out} argument).
    #' See the constructor of the \code{plotly_datahandler} class for more
    #' information on other arguments.
    #' @param figure,legend_options,tz,use_light_build
    #' Arguments passed to \code{plotly_datahandler$new}.
    #' @param n_out Integer or numeric.
    #' The number of samples shown after down-sampling. By default 1000.
    #' @param aggregator An instance of an R6 class for aggregation.
    #' Select an aggregation function. The list of the functions are obtained
    #' using \code{list_aggregators}.
    #' By default, \code{min_max_aggregator$new()}.
    #'
    initialize = function(
      figure = NULL,
      n_out = 1000L,
      aggregator = min_max_aggregator$new(),
      tz = Sys.timezone(),
      use_light_build = TRUE,
      legend_options = list(
        name_prefix  = '<b style="color:sandybrown">[S]</b> ',
        name_suffix  = "",
        xdiff_prefix = '<i style="color:#fc9944"> ~',
        xdiff_suffix = "</i>"
      )
    ) {

      # register the data
      super$initialize(
        figure = figure,
        legend_options = legend_options,
        tz = tz,
        use_light_build = use_light_build
      )

      # check classes and lengths of the arguments
      assertthat::assert_that(inherits(n_out, c("numeric", "integer")))
      assertthat::assert_that(inherits(aggregator, "aggregator"))

      # register the values other than figure
      private$n_out_def      <- n_out[1]
      private$aggregator_def <- aggregator

      # register downsample options
      self$set_downsample_options()
      # set the initial data
      self$update_trace(reset = TRUE)

      invisible()

    }, #end of initialization

    #' @description
    #' Add a new series to the data registered in the instance.
    #' If a data frame (\code{traces_df} argument) compliant with
    #' \code{self$orig_data} is given, it will be added to \code{self$orig_data}.
    #' If attributes to construct a \code{plotly} object (\code{...} argument)
    #' are given, a data frame is constructed and added.
    #' Options for aggregating data can be set using
    #' \code{aggregator} and \code{n_out} arguments.
    #' It is a wrapper of \code{self$set_trace_data} and
    #' \code{self$set_downsample_options}. See these methods for more information.
    #' Note that the traces of the figure are not updated with this method and
    #' \code{self$update_trace} is necessary.
    #' @param ...,traces_df Arguments passed to \code{self$set_trace_data}
    #' (see the super class of \code{plotly_datahandler})
    #' @param n_out,aggregator
    #' Arguments passed to \code{self$set_downsample_options}.
    add_trace = function(
      ..., traces_df = NULL,
      n_out = NULL, aggregator = NULL){

      self$set_trace_data(..., traces_df = traces_df, append = TRUE)

      uid <- setdiff(self$orig_data$uid, self$downsample_options$uid)
      self$set_downsample_options(uid, n_out, aggregator)

      invisible()
    },

    #' @description
    #' Update traces of the figure registered in the instance
    #' (\code{self$figure$x$data}) according to
    #' re-layout order (\code{relayout_order} argument).
    #' Using \code{reset} and \code{reload} arguments, traces are updated
    #' without re-layout orders.
    #' It just registers the new traces and returns nothing by default.
    #' It returns the new traces if \code{send_trace} is \code{TRUE}.
    #' @param relayout_order Named list.
    #' A list generated by \code{plotlyjs_relayout},
    #' which is obtained using \code{plotly::event_data}.
    #' e.g.,
    #' If you would like set the range of the 2nd x axis to [10.0, 21.5],
    #' \code{list(`xaxis2.range[0]` = 10.0, `xaxis2.range[1]` = 21.5)}.
    #' If you would like reset the range of the 1st x axis,
    #' \code{list(xaxis.autorange = TRUE, xaxis.showspike = TRUE)}.
    #' @param reset Boolean.
    #' If it is \code{TRUE}, all other arguments are neglected and
    #' the figure will be reset (all the ranges of x axes are initialized).
    #' By default, \code{FALSE}.
    #' @param reload Boolean.
    #' If it is \code{TRUE}, the ranges of the figure are preserved but
    #' the aggregation will be conducted with the current settings.
    #' By default, \code{FALSE}.
    #' @param send_trace Boolean.
    #' If it is \code{TRUE}, a named list will be returned,
    #' which contains the indexes of the traces that will be updated
    #' (\code{trace_idx_update}) and the updated traces (\code{new_trace}).
    #' By default, \code{FALSE}.
    update_trace = function(
        relayout_order = list(NULL),
        reset = FALSE, reload = FALSE, send_trace = FALSE
    ) {

      # check arguments
      if (is.null(relayout_order)) return()
      assertthat::assert_that(inherits(relayout_order, "list"))
      assertthat::assert_that(inherits(reset, "logical"))
      assertthat::assert_that(inherits(reload, "logical"))
      assertthat::assert_that(inherits(send_trace, "logical"))

      # stop if the order is NULL and no reset or reload
      if (is.null(relayout_order[[1]]) && !reset && !reload) return()

      # if there are no x-axis order and no reset or reload, stop here
      if (
        !any(stringr::str_detect(names(relayout_order), "^xaxis")) &&
        !reset && !reload
        ) {
        return()
      }

      # compute relayout_order_df
      relayout_order_df <- private$relayout_order_to_df(
        relayout_order = relayout_order,
        reset = reset, reload = reload
        )

      # if the relayout_order_df is null, stop here
      if (is.null(relayout_order_df) || nrow(relayout_order_df) == 0) return()

      # compute updated data of the traces
      traces_update_df <- private$construct_agg_traces(relayout_order_df)

      # set showlegend to FALSE, if many series are output
      # because of range_stat_aggregator
      if (any(duplicated(traces_update_df$uid))) {
        is_duplicated <- rep(FALSE, nrow(traces_update_df))
        is_uid_rng <- purrr::map_lgl(traces_update_df$trace, ~!is.null(.x[["fill"]]))
        is_duplicated[is_uid_rng] <- duplicated(traces_update_df$uid[is_uid_rng])

        traces_update_df$trace[is_duplicated] <- purrr::map(
          traces_update_df$trace[is_duplicated],
          function(trace){
            trace$showlegend <- FALSE
            return(trace)
          }
        )
      }

      # detect the index of the trace to be updated
      if (is.null(self$figure$x$data) ||
          any(purrr::map_lgl(self$figure$x$data, ~is.null(.$uid)))
      ) {
        trace_idx_update <- integer()
        self$figure$x$data <- NULL
      } else {
        trace_idx_update <- purrr::map(
          unique(traces_update_df$uid),
          ~which(.x == purrr::map_chr(self$figure$x$data, ~.x$uid))
        ) %>%
          unlist()

        # delete the traces to be updated
        self$figure$x$data <- self$figure$x$data[-trace_idx_update]
      }

      new_trace <- traces_update_df$trace %>%
        purrr::keep(~"x" %in% names(.x))

      # register the new data
      self$figure$x$data <- c(
        self$figure$x$data,
        new_trace
      )

      if (send_trace) {
        return(
          list(trace_idx_update = trace_idx_update,
               new_trace = new_trace
              )
        )

      } else {
        invisible()
      }
    },

    #' @description
    #' In the instance, options for aggregating data are registered as data frame.
    #' (see \code{self$downsample_options}.)
    #' Using this method, the options can be set.
    #' @param uid Character, optional.
    #' The unique id of the trace.
    #' If \code{NULL}, all the options registered in this instance are updated.
    #' By default, \code{NULL}.
    #' @param n_out Numeric or integer, optional.
    #' The number of samples output by the aggregator.
    #' If \code{NULL}, the default value registered in this instance is used.
    #' By default, \code{NULL}.
    #' @param aggregator \code{aggregator} object, optional.
    #' An instance that aggregate the data.
    #' If \code{NULL}, the default value registered in this instance is used.
    set_downsample_options = function(
      uid = NULL, n_out = NULL, aggregator = NULL
    ){

      if (is.null(uid)) uid <- private$traces_df$uid
      if (is.null(n_out)) n_out <- private$n_out_def
      if (is.null(aggregator)) aggregator <- private$aggregator_def
      assertthat::assert_that(inherits(uid, "character"))
      assertthat::assert_that(inherits(n_out, c("numeric", "integer")))
      assertthat::assert_that(inherits(aggregator, "aggregator"))

      private$ds_options <- dplyr::bind_rows(
        private$ds_options[private$ds_options[["uid"]] != uid, ],
        tibble(
          uid = uid,
          aggregator_inst = list(aggregator),
          aggregator_name = class(aggregator)[1],
          n_out = n_out,
          interleave_gaps = aggregator$parameters$interleave_gaps,
          NA_position = aggregator$parameters$NA_position
        )
      )
      invisible()
    }
  ), # end of the public member

  active = list(
    #' @field downsample_options
    #' Options for aggregating (down-sampling) data
    #' registered in this instance.
    downsample_options = function() private$ds_options,
    #' @field n_out_default Default sample size.
    n_out_default = function() private$n_out_def,
    #' @field aggregator_default Default aggregator instance.
    aggregator_default = function() private$aggregator_def

  ),


# private members ---------------------------------------------------------

  private = list(

    # downsample options such as aggregator and n_out
    ds_options = NULL,
    # the number of samples shown after down-sampling
    n_out_def = 0L,
    # the dafault aggregator
    aggregator_def = NULL,


    # change the relayout_order to data frame that contains uid and range
    relayout_order_to_df = function(
      relayout_order, reset = FALSE, reload = FALSE
      ) {

      assertthat::assert_that(inherits(relayout_order, "list"))

      # show the relayout order or RESET or RELOAD notification
      if (!reset && !reload) {
        message(paste(
          paste(
            "Re-layout order: {",
            paste(
              paste(names(relayout_order), relayout_order, sep = ":"),
              collapse = " "
            ),
            "}"
          )
        ))
      } else if (reset) {
        message("Initialize the samples")
      } else if (reload) {
        message("Reload the samples")
      }

      # prepare blank data frame for the type of the relayout order
      order_type_blank <- tibble::tribble(
        ~`range[0]`, ~`range[1]`, ~autorange, ~showspikes,
        list(NULL),  list(NULL),  NA,         NA
      ) %>% .[0,]

      ###
      # First, convert the order to data frame
      # The necessary data is xaxis, start and end values
      ###

      # if the update is resetd, all the xaxis is reset
      if (reset) {
        x_order_df <- tibble(
          xaxis = purrr::map_chr(
            self$figure$x$data,
            ~stringr::str_replace(.x$xaxis, "^x", "xaxis")
          ),
          start = list(NA),
          end = list(NA)
        ) %>%
          dplyr::distinct(xaxis, .keep_all = TRUE)

      # else if the update is caused by the change of the aggregation method,
      # keep ranges but update values
      } else if (reload) {
        x_order_df <- self$plotly_data_to_df(
          self$figure$x$data, use_datatable = FALSE
          ) %>%
          dplyr::mutate(
            xaxis = stringr::str_replace(xaxis, "^x", "xaxis"),
            start = purrr::map(x, ~min(.x, na.rm = TRUE)),
            end   = purrr::map(x, ~max(.x, na.rm = TRUE))
          ) %>%
          tidyr::nest(data = -xaxis) %>%
          dplyr::mutate(
            data = purrr::map(
              data,
              ~tidyr::unnest(.x, c(start, end)) %>%
                dplyr::summarise(
                  start = list(min(start, na.rm = TRUE)),
                  end = list(max(end, na.rm = TRUE))
                )
            )
          ) %>%
          tidyr::unnest(data)

      # else, update according to the order
      } else {
        x_order_df <- as_tibble(relayout_order) %>%
          dplyr::mutate(across(.fns = list)) %>%
          tidyr::pivot_longer(
            everything(),
            names_sep = "\\.", names_to = c("xaxis", "type")
          ) %>%
          dplyr::filter(stringr::str_detect(xaxis, "^xaxis")) %>%
          tidyr::pivot_wider(names_from = type, values_from = value) %>%
          tidyr::unnest(matches("autorange|showspikes")) %>%
          bind_rows(
            order_type_blank[setdiff(colnames(order_type_blank), colnames(.))]
          ) %>%
          dplyr::filter(
            (!purrr::map_lgl(`range[0]`, is.null) &
               !purrr::map_lgl(`range[0]`, is.null)) |
              (!is.na(autorange) & !is.na(showspikes))
          ) %>%
          dplyr::select(xaxis, start = `range[0]`, end = `range[1]`) %>%
          dplyr::mutate(
            across(
              c(start, end),
              ~dplyr::if_else(
                purrr::map_lgl(.x, is.null),
                list(NA),
                .x
              )
            )
          )
      }

      # return NULL if the data frame is virtually NULL
      if (nrow(x_order_df) == 0) return()

      ###
      # Then, link the order to the trace uid
      ###
      relayout_order_df <- private$traces_df %>%
        dplyr::select(uid, xaxis) %>%
        dplyr::inner_join(
          x_order_df %>%
            dplyr::mutate(xaxis = stringr::str_replace(xaxis, "^xaxis", "x")),
          by = "xaxis"
        ) %>%
        dplyr::mutate(
          across(
            c(start, end),
            ~modify_if(
              .x, ~inherits(.x, c("POSIXt", "character")),
              ~private$plotlytime_to_nanotime(.x, private$tz)
            )
          )
        )

      return(relayout_order_df)
    },


    # construct a data frame of aggregated traces,
    # by employing the data frame representing relayout_order.
    construct_agg_traces = function(relayout_order_df = NULL) {

      # check arguments
      if (is.null(relayout_order_df)) return()
      assertthat::assert_that(inherits(relayout_order_df, "data.frame"))
      assertthat::assert_that(
        length(
          setdiff(c("uid", "start", "end"), colnames(relayout_order_df))
          ) == 0,
        msg = "uid, start and end columns must be included in the data frame"
      )

      # select columns
      traces_update_df <-
        relayout_order_df[, c("uid", "start", "end"), with = FALSE]

      # MAIN aggregation
      traces_update_df$agg_result <- purrr::pmap(
        traces_update_df %>%
          dplyr::left_join(private$traces_df, by = "uid") %>%
          dplyr::left_join(private$ds_options, by = "uid") %>%
          dplyr::rename(aggregator = aggregator_inst)
        ,
        private$aggregate_trace,
      )

      # construct a list representing a trace
      traces_update_agg_df <- traces_update_df %>%
        tidyr::unnest(agg_result) %>%
        dplyr::select(-start, -end) %>%
        dplyr::mutate(
          data = purrr::modify_if(
            data,
            ~inherits(.x, "data.frame"),
            ~dplyr::summarise(.x, across(.fns = list))
            ) %>%
            purrr::modify_if(
              ~inherits(.x, "list"),
              ~purrr::modify_if(.x, ~length(.x) > 1, list) %>% as_tibble(.x)
            )
        ) %>%
        tidyr::unnest(data)


      # join with the current data
      colname_from_current <- c(
        "uid",
        setdiff(
          colnames(private$traces_df),
          c(colnames(traces_update_agg_df), "data")
        )
      )

      traces_update_list <- left_join(
        traces_update_agg_df,
        private$traces_df[, colname_from_current, with = FALSE],
        by = "uid"
      ) %>%
        as.list() %>%
        purrr::transpose() %>%
        purrr::map(~purrr::discard(.x, ~all(is.na(.x))))

      # return a data frame including the traces and its uids.
      return(
        tibble(
          uid = purrr::map_chr(traces_update_list, ~.x$uid),
          trace = traces_update_list
        )
      )
    },

    # MAIN aggregation process
    # it returns the aggregated trace and the name in data frame
    aggregate_trace = function(
      data, start, end, name, legendgroup, aggregator, n_out, ...
      ) {

      # extract data
      if (!is.na(start) && length(start) > 0 &&
          !is.na(end)   && length(end)   > 0) {
        data <- data[x >= start & x <= end]
      } else {
        data <- data
      }

      # number of the extracted data
      nrow_orig <- nrow(data)
      # down-sample x and y
      data_agg <- aggregator$
        aggregate(
          x = data$x, y = data$y, n_out = n_out
        )%>%
        data.table::setDT() %>%
        merge(
          data[,
               intersect(colnames(data), c("x", "text", "hovertext")),
               with = FALSE],
          all.x = TRUE, sort = FALSE
          )

      # number of the aggregated data
      nrow_agg <- nrow(data_agg)


      # generate a message about the down-sampling
      msg <- paste0(
        name,
        if_else(is.na(legendgroup), "", paste0("/", legendgroup)),
        ": ",
        if_else(
          nrow_orig <= nrow_agg,
          paste0("no down-sample (n: ", nrow_orig, ")"),
          paste0("applied down-sample (n: ", nrow_orig, " -> ", nrow_agg, ")")
        )
      )
      message(msg)

      # if no data, stop here
      if (nrow_agg == 0) {
        return(tibble(name = as.character(name), data = list(data_agg)))
      }

      # generate a name for aggregation
      name <- if_else(
        nrow_orig <= nrow_agg || inherits(aggregator, "null_aggregator"),
        as.character(name),
        private$cmpt_trace_name(name, data_agg$x)
      )

      # format x values if the x is nanotime
      if (inherits(data_agg$x, "nanotime")) {
        data_agg$x <- private$nanotime_to_plotlytime(data_agg$x, private$tz)
      }

      # add the range of the data if the aggregator is rng_aggregator
      if (inherits(aggregator, "rng_aggregator")) {
        list_attr_rng <- aggregator$as_plotly_range(
          x = data_agg$x, y = data_agg$y,
          ylwr = data_agg$ylwr, yupr = data_agg$yupr
          )

        data_agg <- data_agg[, c("x", "y"), with = FALSE]
        data_agg <- c(list(data_agg), list_attr_rng)
      } else {
        data_agg <- list(data_agg)
      }

      # finally constitute the result tibble
      agg_result <- tibble(
        name = name,
        data = data_agg
      )

      return(agg_result)
    }
  ) # end of the private member
) # end of the R6 class
