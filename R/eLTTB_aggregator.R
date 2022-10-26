#' Aggregation using local minimum and maximum values,
#' and Largest Triangle Three Buckets (LTTB) method.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Efficient version off LTTB
#' by first reducing really large data with the \code{min_max_ovlp_aggregator}
#' and then further aggregating the reduced result with \code{LTTB_aggregator}.
#' @examples
#' data(noise_fluct)
#' agg <- eLTTB_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
eLTTB_aggregator <- R6::R6Class(
  "eLTTB_aggregator",
  inherit = aggregator,
  public = list(
    #' @field LTTB An R6 LTTB_aggregator instance
    LTTB = NULL,

    #' @field minmax An R6 \code{min_max_ovlp_aggregator} instance
    minmax = NULL,

    #' @description
    #' Constructor of the aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to \code{aggregator$new}.
    #' @param ... Not used.
    initialize = function(interleave_gaps = FALSE, nan_position = "end", ...) {
      self$LTTB   <- LTTB_aggregator$new(interleave_gaps = FALSE)
      self$minmax <- min_max_ovlp_aggregator$new(interleave_gaps = FALSE)

      super$initialize(
        interleave_gaps,
        nan_position,
        accepted_datatype <- c(
          "numeric", "integer", "character", "factor", "logical"
        )
      )
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      if (length(x) > n_out * 1e3) {
        result <- self$minmax$aggregate(x, y, n_out * 50)
        x <- result$x
        y <- result$y
      }
      result <- self$LTTB$aggregate(x, y, n_out)
    }
  )
)
