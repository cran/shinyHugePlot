#' Aggregation using a user-defined function.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Arbitrary function can be applied using this aggregation class.
#' @examples
#' custom_agg_func <- function(x, y, n_out) {
#'   bin_width <- floor(length(x)/n_out)
#'   x_idx <- seq(floor(bin_width / 2), bin_width * n_out, bin_width)
#'   y_mat <- y[1:(bin_width * n_out)] %>%
#'     matrix(nrow = bin_width)
#'   y_agg <- apply(y_mat, 2, quantile, probs = 0.25)
#'   return(list(x = x[x_idx], y = y_agg))
#' }
#' data(noise_fluct)
#' agg <- custom_func_aggregator$new(custom_agg_func)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
custom_func_aggregator <- R6::R6Class(
  "custom_func_aggregator",
  inherit = aggregator,
  public = list(
    #' @field aggregation_func
    #' User-defined function to aggregate data,
    #' of which arguments are \code{x}, \code{y} and \code{n_out}.
    aggregation_func = NULL,
    #' @description
    #' Constructor of the Aggregator.
    #' @param aggregation_func Function.
    #' User-defined function to aggregate data,
    #' of which arguments are \code{x}, \code{y} and \code{n_out}.
    #' @param interleave_gaps,nan_position,accepted_datatype
    #' Arguments pass to \code{aggregator$new}.
    #' @param ... Not used.
    initialize = function(
    aggregation_func, interleave_gaps = FALSE,
    nan_position = "end", accepted_datatype = NULL) {

      super$initialize(interleave_gaps, nan_position, accepted_datatype)
      self$aggregation_func <- aggregation_func
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      return(self$aggregation_func(x, y, n_out))
    }
  )
)
