
#' @param dash_runserver_options Named list, optional.
#' Arguments passed to \code{run_server} method of the Dash app.
#' @param traceupdater_options Named list, optional.
#' Arguments passed to \code{trace_updater} function
#' other than \code{id} and \code{gdID}.
#' @param run_dash Boolean, optional.
#' When the \code{obj} is a plotly structure, this indicates
#' whether a generated dash app will be run immediately.
#' @param graph_id Character.
#' When the \code{obj} is an instance of Dash, this indicates
#' the id of the graph to which the down-sampling is applied.
#' @rdname apply_downsampler
#' @export
apply_downsampler.Dash <- function(
    obj,
    n_out = 1000L,
    aggregator = eLTTB_aggregator,
    graph_id,
    downsampler_options = list(),
    traceupdater_options = list(sequentialUpdate = FALSE),
    ...
) {

  # check data type
  assertthat::assert_that(inherits(obj, "Dash"))

  # get the app layout
  layout <- obj$layout_get(FALSE)

  # get the figure to which the downsampling is applied
  if (is.null(layout$props$children)) {
    dash_components <- list(layout)
  } else {
    dash_components <- layout$props$children
  }

  ids <- purrr::map_chr(dash_components, ~.x$props$id)

  # check that the graph_id exists and the type of the component is Graph
  assertthat::assert_that(
    graph_id %in% ids,
    msg = "The given graph_id does not exist in the app."
  )
  assertthat::assert_that(
    dash_components[[which(ids == graph_id)]]$type == "Graph",
    msg = "The type of the graph_id component is NOT 'Graph'."
  )

  downsampler_options[["n_out"]] <- n_out
  downsampler_options[["aggregator"]] <- aggregator$new()

  # create dash_downsampler instance and
  fd <- do.call(
    dash_downsampler$new,
    c(
      list(figure = dash_components[[which(ids == graph_id)]]$props$figure),
      downsampler_options
    )
  )

  # substitute the dash components layout with the dash_downsampler figure
  dash_components[[which(ids == graph_id)]]$props$figure <- fd$figure

  # set layout
  fd$app <- do.call(
    dash::set_layout,
    c(app = list(dash::dash_app()), dash_components)
  )

  # register trace updater
  fd$register_traceupdater(graph_id)

  return(fd)
}
