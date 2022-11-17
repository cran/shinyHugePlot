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
#' agg <- custom_func_aggregator$new(
#'   aggregation_func = custom_agg_func, interleave_gaps = TRUE
#'   )
#' d_agg <- agg$aggregate(
#'   x = noise_fluct$time, y = noise_fluct$f500, n_out = 1000
#'   )
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
custom_func_aggregator <- R6::R6Class(
  "custom_func_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param aggregation_func Function.
    #' User-defined function to aggregate data,
    #' of which arguments are \code{x}, \code{y} and \code{n_out}.
    #' @param interleave_gaps,coef_gap,NA_position,accepted_datatype,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    initialize = function(
      ..., aggregation_func,
      interleave_gaps, coef_gap, NA_position, accepted_datatype
      ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
      private$aggregation_func <- aggregation_func
    },

    #' @description
    #' Set a function to aggregate the data
    #' @param aggregation_func Function.
    #' User-defined function to aggregate data,
    #' of which arguments are \code{x}, \code{y} and \code{n_out}.
    #'
    set_aggregation_func = function(aggregation_func) {
      assertthat::assert_that(aggregation_func, "function")
      private$aggregation_func <- aggregation_func
    }

  ),
  private = list(
    aggregation_func = NULL,
    aggregate_exec = function(x, y, n_out) {
      return(private$aggregation_func(x, y, n_out))
    }
  )
)
