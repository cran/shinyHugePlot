#' Aggregation which returns the rms and maximum values
#' within small data ranges
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' This aggregator divides the data into no-overlapping intervals
#' and calculate the root-mean-square and the maximum absolute
#' values of the data, which may be helpful to understand the waveforms.
#' @examples
#' data(noise_fluct)
#' agg <- rms_max_aggregator$new(
#'   ylwr = min, y = mean, yupr = max, interleave_gaps = TRUE
#' )
#' d_agg <- agg$aggregate(nanotime::as.nanotime(noise_fluct$time), noise_fluct$f500, 100)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines") %>%
#'   plotly::add_trace(x = d_agg$x, y = d_agg$ylwr, type = "scatter", mode = "lines")%>%
#'   plotly::add_trace(x = d_agg$x, y = d_agg$yupr, type = "scatter", mode = "lines")
#'
rms_max_aggregator <- R6::R6Class(
  "rms_max_aggregator",
  inherit = rng_aggregator,
  public = list(
    #' @description
    #' Constructor of the aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    #' @param use_abs
    #' Boolean. If `TRUE`, the maximum absolute values are calculated.
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
    use_abs = TRUE,

    aggregate_exec = function(x, y, n_out) {

      x_mat <- private$generate_matrix(x, n_out, remove_first_last = FALSE)
      if (inherits(x, "integer64")) {
        x_agg <- private$apply_nano64(
          x_mat, 2, function(x) mean(x, na.rm = TRUE)
        )
      } else {
        x_agg <- apply(x_mat, 2, mean, na.rm = TRUE)
      }

      if (private$use_abs) {
        y_use <- abs(y)
      } else {
        y_use <- y
      }
      y_mat <- private$generate_matrix(y_use, n_out, remove_first_last = FALSE)

      y_rms <- apply(y_mat, 2, function(x) sqrt(mean(x^2, na.rm=T)))
      y_max <- apply(y_mat, 2, function(x) max(x, na.rm=T))


      return(list(x = x_agg, ylwr = y_rms, y = y_rms, yupr = y_max))
    }
  )
)
