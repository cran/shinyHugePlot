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
#' agg <- range_stat_aggregator$new(
#'   ylwr = min, y = mean, yupr = max, interleave_gaps = TRUE
#' )
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 100)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines") %>%
#'   plotly::add_trace(x = d_agg$x, y = d_agg$ylwr, type = "scatter", mode = "lines")%>%
#'   plotly::add_trace(x = d_agg$x, y = d_agg$yupr, type = "scatter", mode = "lines")
#'
range_stat_aggregator <- R6::R6Class(
  "range_stat_aggregator",
  inherit = rng_aggregator,
  public = list(
    #' @description
    #' Constructor of the aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,accepted_datatype,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    #' @param yupr,y,ylwr Functions.
    #' Statistical values are calculated using this function.
    #' By default, \code{max, mean, min}, respectively.
    #' Note that the NA values are omitted automatically.
    initialize = function(
      ...,
      ylwr = min, y = mean, yupr = max,
      interleave_gaps, coef_gap, NA_position, accepted_datatype
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)

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
