#' NULL aggregator.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' It does not aggregate the data but returns the full samples within the range.
#' @examples
#' data(noise_fluct)
#' agg <- null_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level)
#' plot(d_agg$x[1:100], d_agg$y[1:100], type = "l")
null_aggregator <- R6::R6Class(
  "null_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor that changes nothing.
    #' @param ... not used
    initialize = function(...) {},
    #' @description
    #' Function where no aggregation will be executed.
    #' @param x,y Vectors.
    #' @param n_out Integer, omitted.
    aggregate = function(x, y, n_out = -1){
      return(list(x = x, y = y))
    }
  )
)
