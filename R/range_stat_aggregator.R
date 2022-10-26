#' Aggregation which returns the ranges and nominal values
#' within small data ranges
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' This aggregator divides the data into no-overlapping intervals
#' and calculate specific statistics that represents the range and nominal
#' values of the data, such as the max, min and mean.
#' @examples
#' data(noise_fluct)
#' agg <- range_stat_aggregator$new(ylwr = min, y = mean, yupr = max)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$ylwr, type = "l")
#' plot(d_agg$x, d_agg$y, type = "l")
#' plot(d_agg$x, d_agg$yupr, type = "l")
#'
range_stat_aggregator <- R6::R6Class(
  "range_stat_aggregator",
  inherit = rng_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to \code{aggregator$new}.
    #' @param yupr,y,ylwr Functions.
    #' Statistical values are calculated using this function.
    #' By default, \code{max, mean, min}, respectively.
    #' Note that the NA values are omitted automatically.
    #' @param ... Not used.
    initialize = function(
    ylwr = min,
    y = mean,
    yupr = max,
    interleave_gaps = FALSE, nan_position = "end",
    ...
    ) {
      assertthat::assert_that(
        all(
          c(
            inherits(ylwr, "function"),
            (inherits(y, "function") || is.null(y)),
            inherits(yupr, "function")
          )
        ),
        msg = "ylwr and yupr must be functions / y must be a function or NULL"
      )
      private$ylwr <- function(x) ylwr(na.omit(x))
      private$yupr <- function(x) yupr(na.omit(x))
      if (!is.null(y)) {
        private$y    <- function(x) y(na.omit(x))
      }
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    ylwr = NULL,
    y    = NULL,
    yupr = NULL,

    aggregate_exec = function(x, y, n_out) {

      x_mat <- private$generate_matrix(x, n_out, remove_first_last = FALSE)
      if (inherits(x, "integer64")) {
        x_agg <- private$apply_nano64(
          x_mat, 2, function(x) mean(x, na.rm = TRUE)
        )
      } else {
        x_agg <- apply(x_mat, 2, mean, na.rm = TRUE)
      }

      y_mat <- private$generate_matrix(y, n_out, remove_first_last = FALSE)

      yupr_agg <- apply(y_mat, 2, private$yupr)
      ylwr_agg <- apply(y_mat, 2, private$ylwr)
      if (!is.null(private$y)) {
        y_agg    <- apply(y_mat, 2, private$y)
      } else {
        y_agg <- NA
      }

      return(list(x = x_agg, ylwr = ylwr_agg, y = y_agg, yupr = yupr_agg))
    }
  )
)
