#' Aggregation using local maximum (absolute) values.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into small data ranges
#' and find the maximum (absolute) value of each.
#' It may be useful for the waveform data.
#' @examples
#' data(noise_fluct)
#' agg <- max_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
#'
max_aggregator <- R6::R6Class(
  "max_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    #' @param use_abs Logical. If \code{TRUE}, the absolute value is used.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position, use_abs = TRUE
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
      private$use_abs <- use_abs
    }
  ),
  private = list(
    accepted_datatype = c("numeric", "integer", "character", "factor", "logical"),
    use_abs = NULL,
    aggregate_exec = function(x, y, n_out) {
      if (private$use_abs) {
        y_use <- abs(y)
      } else {
        y_use <- y
      }

      y_mat <- private$generate_matrix(
        y_use, n_out, remove_first_last = FALSE
      )
      y_mat_values <- apply(y_mat, 2, function(x) sum(!is.na(x)))

      idx_max <- c(0, cumsum(y_mat_values)[1:(n_out - 1)]) +
        purrr::map_int(1:n_out, ~which.max(y_mat[, .x]))
        c(0, cumsum(y_mat_values)[1:(n_out - 1)])

      return(list(x = x[idx_max], y = y_use[idx_max]))

    }
  )
)
