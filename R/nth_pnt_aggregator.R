#' Aggregation which returns every Nth point.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Aggregation by extracting every Nth data.
#' @examples
#' data(noise_fluct)
#' agg <- nth_pnt_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
nth_pnt_aggregator <- R6::R6Class(
  "nth_pnt_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
    }
  ),
  private = list(
    accepted_datatype = c("numeric", "integer", "character", "factor", "logical"),
    aggregate_exec = function(x, y, n_out) {
      idx <- seq(1, length(x), max(1, ceiling(length(x) / n_out)))
      return(list(x = x[idx], y = y[idx]))
    }
  )
)
