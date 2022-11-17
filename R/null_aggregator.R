#' NULL aggregator.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' It does not aggregate the data but returns the full samples within the range.
#' @examples
#' data(noise_fluct)
#' agg <- null_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500)
#' plotly::plot_ly(
#'   x = d_agg$x[1:100], y = d_agg$y[1:100], type = "scatter", mode = "lines"
#' )
null_aggregator <- R6::R6Class(
  "null_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,accepted_datatype,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position, accepted_datatype
      ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
    },
    #' @description
    #' A function that does nothing other than inserting NAs.
    #' @param ... Arguments passed to \code{super$aggregate}.
    aggregate = function(...) {
      args <- list(...)
      if (is.null(args$n_out)) args$n_out <- Inf
      do.call(super$aggregate, args)
    }
  ),
  private = list(
    aggregate_exec = function(x, y, ...) list(x = x, y = y)
  )
)
