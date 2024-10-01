#' Aggregation using local minimum and maximum values
#' of which small data ranges have 50\% overlaps.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into 50\% overlapping intervals
#' and find the maximum and minimum values of each.
#' \code{n_out} must be even number.
#' @examples
#' data(noise_fluct)
#' agg <- min_max_ovlp_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
#'
min_max_ovlp_aggregator <- R6::R6Class(
  "min_max_ovlp_aggregator",
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

      n_minmax <- n_out / 2 - 1
      block_size <- floor((length(x) - 2) / (n_minmax + 0.5))

      y_mat_1 <- private$generate_matrix(
        y[2:(length(x) - 1 - block_size %/% 2 )],
        n_minmax, remove_first_last = FALSE
      )

      y_mat_1_values <- apply(y_mat_1, 2, function(x) sum(!is.na(x)))

      y_mat_2 <- private$generate_matrix(
        y[(2 + block_size %/% 2):(length(x) - 1)],
        n_minmax, remove_first_last = FALSE
      )

      y_mat_2_values <- apply(y_mat_2, 2, function(x) sum(!is.na(x)))

      idx_min <- 1 +
        purrr::map_int(1:n_minmax, ~max(0, which.min(y_mat_1[, .x]))) +
        c(0, cumsum(y_mat_1_values)[1:(n_minmax - 1)])

      idx_max <- block_size %/% 2 +
        purrr::map_int(1:n_minmax, ~max(0, which.max(y_mat_2[, .x]))) +
        c(0, cumsum(y_mat_2_values)[1:(n_minmax - 1)])

      idx <- c(1, idx_min, idx_max, length(x)) %>% sort()

      return(list(x = x[idx], y = y[idx]))
    }
  )
)
