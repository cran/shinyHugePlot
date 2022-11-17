#' Aggregation using local minimum and maximum values.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into small data ranges
#' and find the maximum and minimum values of each.
#' Note that many samples may be replaced with \code{NA},
#' if \code{interleave_gaps = TRUE} and the original data is increased or decreased
#' monotonically. Use \code{min_max_ovlp_aggregator} instead of this class.
#' \code{n_out} must be even number.
#' @examples
#' data(noise_fluct)
#' agg <- min_max_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
#'
min_max_aggregator <- R6::R6Class(
  "min_max_aggregator",
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
