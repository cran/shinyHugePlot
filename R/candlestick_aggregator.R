#' Aggregation which returns the candle sticks
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' This aggregator divides the data into no-overlapping intervals
#' and calculate the first, maximum, minimum, and last values of the data,
#' which represents candle sticks.
#' @examples
#' data(noise_fluct)
#' agg <- candlestick_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(nanotime::as.nanotime(noise_fluct$time), noise_fluct$f500, 100)
#' fig <- plotly::plot_ly(
#'   x = d_agg$x, open = d_agg$open, high = d_agg$high, low = d_agg$low, close = d_agg$close,
#'   type = "candlestick"
#'   )
#'
candlestick_aggregator <- R6::R6Class(
  "candlestick_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    #' @param yupr,y,ylwr Functions.
    #' Statistical values are calculated using this function.
    #' By default, \code{max, mean, min}, respectively.
    #' Note that the NA values are omitted automatically.
    initialize = function(
      ...,
      interleave_gaps, coef_gap, NA_position
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)

    }
  ),
  private = list(
    accepted_datatype = c("numeric", "integer", "character", "factor", "logical", "list"),

    aggregate_exec = function(x, y, n_out) {

      if (length(x) <= n_out) {
        if (inherits(y, "list")){
          return(list(x = x, open = y$open, high = y$high, low = y$low, close = y$close))
        } else {
          return(list(x = x, open = y, high = y, low = y, close = y))
        }
      }

      x_mat <- private$generate_matrix(x, n_out, remove_first_last = FALSE)
      if (inherits(x, "integer64")) {
        x_agg <- private$apply_nano64(
          x_mat, 2, function(x) mean(x, na.rm = TRUE)
        )
      } else {
        x_agg <- apply(x_mat, 2, mean, na.rm = TRUE)
      }

      if (inherits(y, "list")){
        open_mat <- private$generate_matrix(y$open, n_out, remove_first_last = FALSE)
        close_mat <- private$generate_matrix(y$close, n_out, remove_first_last = FALSE)
        high_mat <- private$generate_matrix(y$high, n_out, remove_first_last = FALSE)
        low_mat <- private$generate_matrix(y$low, n_out, remove_first_last = FALSE)
      } else {
        y_mat <- private$generate_matrix(y, n_out, remove_first_last = FALSE)
        open_mat <- close_mat <- high_mat <- low_mat <- y_mat
      }


      y_open  <- open_mat[1,]
      y_close <- purrr::map2_dbl(
        close_mat[nrow(close_mat),], close_mat[nrow(close_mat)-1,],
        ~if_else(!is.na(.x), .x, .y)
      )

      y_high  <- apply(high_mat, 2, function(x) max(x, na.rm = T))
      y_low   <- apply(low_mat, 2, function(x) min(x, na.rm = T))

      return(list(x = x_agg, open = y_open, high = y_high, low = y_low, close = y_close))
    }
  )
)
