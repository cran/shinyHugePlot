#' A TraceUpdater component.
#'
#' @description
#' This is provided by predict-idlab.
#' See https://github.com/predict-idlab/trace-updater .
#'
#' @note
#' The TraceUpdater type must be registered to use this component.
#' The scripts are found in `inst/deps/tu`.
#'
#' @param id String, optional.
#' The ID used to identify this component in Dash callbacks.
#' @param gdID String, required.
#' The id of the graph component whose traces should be updated.
#' @param sequentialUpdate Boolean.
#' Whether the figure should be redrawn sequentially. By default `FALSE`.
#' @param updateData Tibble, optional.
#' The data to update the graph with.
#' Column named `index` must be included.
#' The classes of other columns are `list` and each row represents
#' the properties of a single trace (e.g., `x`, `y`, `type`).
trace_updater <- function(
    id = NULL, gdID = NULL, sequentialUpdate = NULL, updateData = NULL
    ) {

  props <- list(
    id = id,
    gdID = gdID,
    sequentialUpdate = sequentialUpdate,
    updateData = updateData
    )

  if (length(props) > 0) {
    props <- props[!vapply(props, is.null, logical(1))]
  }

  component <- list(
    props = props,
    type = "TraceUpdater",
    namespace = "trace_updater",
    propNames = c("id", "gdID", "sequentialUpdate", "updateData"),
    package = "traceUpdater"
  )

  structure(component, class = c("dash_component", "list"))
}

#' Find the javascript metadeta for trace updater
#'
#' @description
#' This function will be used to register the trace updater in Dash app.
#' The function name is essentially important because 
#' the \code{dash:::getDashMetadata} function finds this function using
#' regular expression of \code{^\\.dash.+_js_metadata$}.
.dashTraceUpdater_js_metadata <- function() {
  deps_metadata <- list(
    `trace_updater` = structure(
      list(
        name = "trace_updater",
        version = "0.0.8",
        src = list(href = NULL, file = "deps"),
        # src = list(href = NULL, file = "inst/deps"), # when using load_all
        meta = NULL,
        script = "tu/trace_updater.min.js",
        stylesheet = NULL,
        head = NULL,
        attachment = NULL,
        package = "plotlyHugeData",
        all_files = FALSE
      ),
      class = "html_dependency"
    ),
    `trace_updater` = structure(
      list(
        name = "trace_updater",
        version = "0.0.8",
        src = list(href = NULL, file = "deps"),
        # src = list(href = NULL, file = "inst/deps"), # when using load_all
        meta = NULL,
        script = "tu/trace_updater.min.js.map",
        stylesheet = NULL,
        head = NULL,
        attachment = NULL,
        package = "plotlyHugeData",
        all_files = FALSE,
        dynamic = TRUE
      ),
      class = "html_dependency"
    )
  )
  return(deps_metadata)
}
