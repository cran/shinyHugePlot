#' Aggregation which returns arbitrary statistics
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' This aggregator divides the data into no-overlapping intervals
#' and calculate specific statistical values such as the mean.
#' @examples
#' data(noise_fluct)
#' agg <- custom_stat_aggregator$new(y_func = mean, interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(noise_fluct$time, noise_fluct$f500, 1000)
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
#'
custom_stat_aggregator <- R6::R6Class(
  "custom_stat_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param y_func Function.
    #' Statistical values are calculated using this function.
    #' By default, \code{mean}.
    #' @param x_mean Boolean.
    #' Whether using the mean values or not for the x values.
    #' If not, the x values that give the specific y values are used.
    #' E.g., if you use \code{max} as the \code{aggregation_func} and
    #' set this argument to \code{FALSE}, x values that give the maximum
    #' y values are used.
    #' By default, \code{TRUE}.
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,coef_gap,NA_position,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    initialize = function(
      ...,
      y_func = mean, x_mean = TRUE,
      interleave_gaps, coef_gap, NA_position
      ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
      private$y_func <- function(x) y_func(na.omit(x))
      private$x_mean <- x_mean
    }

  ),
  private = list(
    accepted_datatype = c("numeric", "integer", "character", "factor", "logical"),
    y_func = NULL,
    x_mean = NULL,
    aggregate_exec = function(x, y, n_out) {

      y_mat <- private$generate_matrix(y, n_out, remove_first_last = FALSE)
      y_agg <- apply(y_mat, 2, private$y_func)

      x_mat <- private$generate_matrix(x, n_out, remove_first_last = FALSE)
      if (private$x_mean && inherits(x, "integer64")) {
        x_agg <- private$apply_nano64(
          x_mat, 2, function(x) mean(x, na.rm = TRUE)
        )
      } else if (private$x_mean) {
        x_agg <- apply(x_mat, 2, mean, na.rm = TRUE)
      } else {
        x_idx <- purrr::map_int(
          seq_along(y_agg),
          ~if_else(
            y_agg[.x] %in% y_mat[, .x],
            which(y_mat[, .x] == y_agg[.x])[1],
            NA_integer_
            )
        )
        x_agg <- purrr::map(seq_along(y_agg), ~x_mat[x_idx[.x], .x]) %>%
          unlist()
      }

      return(list(x = x_agg, y = y_agg))
    }
  )
)
