#' Aggregation using local minimum and maximum values.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into small data ranges
#' and find the maximum and minimum values of each.
#' \code{n_out} must be even number.
#' @examples
#' data(noise_fluct)
#' agg <- min_max_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
min_max_aggregator <- R6::R6Class(
  "min_max_aggregator",
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
      n_minmax <- n_out / 2 - 1

      y_mat <- private$generate_matrix(
        y[2:(length(x) - 1)], n_minmax, remove_first_last = FALSE
      )
      y_mat_values <- apply(y_mat, 2, function(x) sum(!is.na(x)))

      idx_min <- 1 +
        purrr::map_int(1:n_minmax, ~which.min(y_mat[, .x])) +
        c(0, cumsum(y_mat_values)[1:(n_minmax - 1)])

      idx_max <- 1 +
        purrr::map_int(1:n_minmax, ~which.max(y_mat[, .x])) +
        c(0, cumsum(y_mat_values)[1:(n_minmax - 1)])

      idx <- c(1, idx_min, idx_max, length(x)) %>% sort()

      return(list(x = x[idx], y = y[idx]))

    }
  )
)
