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
#' agg <- eLTTB_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
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
    #' @param interleave_gaps,coef_gap,NA_position,accepted_datatype
    #' Arguments pass to the constructor of \code{aggregator} object.
    #' @param ...
    #' Arguments pass to the constructor of \code{aggregator},
    #' \code{LTTB_aggregator} and \code{min_max_oblp_aggregator} objects.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position,
      accepted_datatype = c("numeric", "integer", "character", "factor", "logical")
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)

      args$interleave_gaps <- FALSE

      self$LTTB   <- do.call(LTTB_aggregator$new, args)
      self$minmax <- do.call(min_max_ovlp_aggregator$new, args)


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
