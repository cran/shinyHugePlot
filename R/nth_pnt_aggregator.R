#' Aggregation which returns every Nth point.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Aggregation by extracting every Nth data.
#' @examples
#' data(noise_fluct)
#' agg <- nth_pnt_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
nth_pnt_aggregator <- R6::R6Class(
  "nth_pnt_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to \code{aggregator$new}.
    #' @param ... Not used.
    initialize = function(interleave_gaps = FALSE, nan_position = "end", ...) {
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      idx <- seq(1, length(x), max(1, ceiling(length(x) / n_out)))
      return(list(x = x[idx], y = y[idx]))
    }
  )
)
