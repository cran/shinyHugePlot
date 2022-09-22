#' R6 Class implementing down-sampling in dash app
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @importFrom R6 R6Class
#' @importFrom plotly plot_ly
#' @importFrom dash dash_app dccGraph set_layout
#' @importFrom dplyr %>%
#' @importFrom dplyr %>%
#' @importFrom dash add_callback
#' @description
#' This class includes high-frequency original data, plotly figure, and
#' dash app.
#' The plotly figure will be made by initializing the instance and using
#' \code{add_trace} method.
#' (note that the method is different from \code{plotly::add_trace}).
#' The easiest way to run dash app is using \code{show_dash} method.
#' Or, you can register the figures using \code{register_figures} method
#' and the trace updater using \code{register_traceupdater},
#' then you can run the app using \code{run_server} method.
#' @examples
#'
#'\donttest{
#' data(noise_fluct)
#' p <- plotly::plot_ly(noise_fluct) %>%
#'   plotly::add_trace(x = ~sec, y = ~level, type = "scatter", mode = "lines")
#' d_app <- dash_downsampler$new(p)
#' d_app$show_dash()
#'}
#'
#'\donttest{
#' data(noise_fluct)
#' d_app <- dash_downsampler$new(p)
#' d_app$add_trace(
#'   x = noise_fluct$sec, y = noise_fluct$level,
#'   type = "scatter", mode = "lines", name = "fluct",
#'   aggregator = mean_aggregator$new()
#'   )
#' d_app$add_trace(
#'   x = noise_fluct$sec, y = noise_fluct$level 10,
#'   type = "scatter", mode = "lines", name = "fluct2",
#'   aggregator = mean_aggregator$new()
#'   )
#' d_app$register_figures(is_separated_panels = TRUE)
#' d_app$register_traceupdater(graph_id = "fluct")
#' d_app$run_server(use_viewer = TRUE)
#'}


# class decralation -------------------------------------------------------

dash_downsampler <- R6::R6Class(
  "dash_downsampler",
  inherit = abstract_downsampler,

# public members ----------------------------------------------------------

  public = list(
    #' @field app Dash app
    app = dash::Dash$new(),
    #' @description
    #' Create a new downsampler
    #' @param figure,is_downsample,n_out,aggregator,legend_options,tz
    #' Arguments pass to the constructor of
    #' the \code{abstract_downsampler} class.
    initialize = function(
      figure = plotly::plot_ly(),
      is_downsample = TRUE,
      n_out = 200L,
      aggregator = eLTTB_aggregator$new(),
      legend_options = list(
        downsample_prefix = '<b style="color:sandybrown">[R]</b> ',
        downsample_suffix = "",
        is_aggsize_shown = TRUE,
        agg_prefix = ' <i style="color:#fc9944">~',
        agg_suffix = "</i>"
      ),
      tz = Sys.timezone()
    ) {

      if (is.null(figure)) self$figure <- plotly::plot_ly()
      super$initialize(
          figure,
          is_downsample,
          n_out,
          aggregator,
          legend_options,
          tz
        )
    },

    #' @description
    #' Construct an simple layout of the dash app
    #' and show the figure.
    #' Note that all the traces are registered as the same id
    #' @param dash_runserver_options Named list, optional.
    #' Arguments passed to \code{Dash$run_server}.
    #' @param traceupdater_options Named list, optional.
    #' Arguments passed to \code{dcTraceUpdater}.
    show_dash = function(
      dash_runserver_options = list(use_viewer = TRUE),
      traceupdater_options = list()
      ) {

      # Construct a simple Dash app layout
      self$register_figures()

      do.call(
        self$register_traceupdater,
        traceupdater_options
      )

      do.call(self$run_server, dash_runserver_options)

    },


    #' @description
    #' Register respective \code{self$figure} in \code{self$app}.
    #' Every figure is shown in a different panel.
    #' Trace name will be employed as the component id.
    #' @param graph_id Character.
    #' The character set as the id of the dash graph component.
    #' By default, "downsample-figure".
    #' @param is_in_separated_panels Boolean.
    #' Whether the respective plotly figures are registered
    #' in the dash app separately.
    #' When \code{TRUE}, each figure is included in a different panel,
    #' of which id is set to the trace name.
    #' By default, FALSE.
    register_figures = function(
      graph_id = "downsample-figure",
      is_in_separated_panels = FALSE
      ) {

      if (is_in_separated_panels) {
        null_fig <- self$figure
        null_fig$x$attrs <- NULL
        null_fig$x$data <- NULL

        dash_components <- purrr::map(
          self$get_figure_data(),
          ~ do.call(
            plotly::add_trace, # private add_trace with downsampling
            c(list(p = null_fig), .x)
          ) %>%
            plotly::subplot()
        ) %>%
          purrr::map2(
            private$orig_datalist,
            ~dash::dccGraph(id = .y$name, figure = .x)
          )

        # Construct a simple Dash app layout
        self$app <- dash::dash_app()

        self$app <- do.call(
          dash::set_layout,
          c(list(app = self$app), dash_components)
        )
      } else {
        # Construct a simple Dash app layout
        self$app <- dash::dash_app() %>%
          dash::set_layout(
            dash::dccGraph(
              id = graph_id,
              figure = self$figure
            )
          )
      }

    },


    #' @description
    #' Register the trace updater.
    #' If there are no components, generate a new trace updater.
    #' Using it, interactive down-sampling will be implemented
    #' @param graph_id Character. The id of the \code{dash::dccGraph}
    #' component that include a plotly figure to which down-sampling
    #' will be applied.
    #' @param trace_updater_id Character.
    #' The id of the \code{trace_updater} component.
    #' @param ...
    #' Arguments passed to \code{trace_updater}.
    register_traceupdater = function(
      graph_id = "downsample-figure",
      trace_updater_id = basename(tempfile(pattern = "tu")),
      ...
      ) {

      # get the app layout
      layout <- self$app$layout_get(FALSE)

      # get the figure to which the downsampling is applied
      if (is.null(layout$props$children)) {
        dash_components <- list(layout)
      } else {
        dash_components <- layout$props$children
      }

      ids <- purrr::map_chr(dash_components, ~.x$props$id)

      # graph id must exist
      assert_that(
        graph_id %in% ids,
        msg = paste("The graph (id:", graphid,") does not exist.")
        )

      # if the trace updater does not exist, add this
      if (!(trace_updater_id %in% ids)) {

        dc_trace_updater <- trace_updater(
          id = trace_updater_id,
          gdID = graph_id,
          ...
        )

        self$app <- do.call(
          dash::set_layout,
          c(app = list(self$app), dash_components, list(dc_trace_updater))
        )
      }

      self$app <- self$app %>%
        dash::add_callback(
          output(trace_updater_id, "updateData"),
          input(graph_id, "relayoutData"),
          private$construct_update_dash
        )
    },


    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$server_route}.
    #' @param ... Arguments passed to \code{Dash$server_route}
    server_route = function(...) self$app$server_route(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$redirect}
    #' @param ... Arguments passed to \code{Dash$redirect}.
    redirect = function(...) self$app$redirect(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$layout_get}
    #' @param ... Arguments passed to \code{Dash$layout_get}
    layout_get = function(...) self$app$layout_get(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$layout}
    #' @param ... Arguments passed to \code{Dash$layout}
    layout = function(...) self$app$layout(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$react_version_set}
    #' @param ... Arguments passed to \code{Dash$react_version_set}
    react_version_set = function(...) self$app$react_version_set(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$callback}
    #' @param ... Arguments passed to \code{Dash$callback}
    callback = function(...) self$app$callback(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$callback_context}
    #' @param ... Arguments passed to \code{Dash$callback_context}
    callback_context = function(...) self$app$callback_context(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$callback_context.record_timing}
    #' @param ... Arguments passed to \code{Dash$callback_context.record_timing}
    callback_context.record_timing = function(...) self$app$callback_context.record_timing(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$get_asset_url}
    #' @param ... Arguments passed to \code{Dash$get_asset_url}
    get_asset_url = function(...) self$app$get_asset_url(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$get_relative_path}
    #' @param ... Arguments passed to \code{Dash$get_relative_path}
    get_relative_path = function(...) self$app$get_relative_path(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$strip_relative_path}
    #' @param ... Arguments passed to \code{Dash$strip_relative_path}
    strip_relative_path = function(...) self$app$strip_relative_path(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$index_string}
    #' @param ... Arguments passed to \code{Dash$index_string}
    index_string = function(...) self$app$index_string(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$interpolate_index}
    #' @param ... Arguments passed to \code{Dash$interpolate_index}
    interpolate_index = function(...) self$app$interpolate_index(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$title}
    #' @param ... Arguments passed to \code{Dash$title}
    title = function(...) self$app$title(...),

    #' @description
    #' Wrapper of the R6 Dash class method of \code{Dash$run_server}
    #' @param ... Arguments passed to \code{Dash$run_server}
    run_server = function(...) self$app$run_server(...)

  ), # end of the public
 private = list(
   construct_update_dash <- function(relayout_order) {

     traces_updates <- contruct_update_data(relayout_order)
     if (is.null(traces_updates)) return(dash::dashNoUpdate())

     # create an order to redraw the figure ----------------------------------

     # autorange axis update
     # note that data.frame structure is necessary
     # so as to convert it to a list of dict in JS

     axis_relayout <- traces_updates$xaxis %>% unique()
     axis_rerange <- traces_updates %>%
       dplyr::filter(!is.na(start) & !is.na(stop)) %>%
       .$xaxis %>%
       unique()

     layout_updates_dash <- c(
       rep(TRUE, length(setdiff(axis_relayout, axis_rerange))),
       rep(FALSE, length(axis_rerange))
     ) %>%
       as.list() %>%
       setNames(c(setdiff(axis_relayout, axis_rerange), axis_rerange)) %>%
       tibble::as_tibble()

     # trace update
     # note that data.frame structure is necessary so as to convert it
     # to a list of dict in JS
     traces_updates_dash <- traces_updates %>%
       dplyr::mutate(
         trace = purrr::map(
           trace,
           ~purrr::map(.x, ~list(.x)) %>% as_tibble()
         )
       ) %>%
       tidyr::unnest(trace) %>%
       tidyr::unnest(name) %>%
       dplyr::select(
         index = trace_idx, name,
         matches("(^x$)|(y)|(text)|(hovertext)")
       )

     return(dplyr::bind_rows(layout_updates_dash, traces_updates_dash))

   }
 )
) # end of the class definition
