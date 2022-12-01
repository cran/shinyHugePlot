#' Aggregation using Largest Triangle Three Buckets (LTTB) method.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @importFrom purrr map map_int
#' @description
#' The LTTB method aggregates the huge samples using the areas
#' of the triangles formed by the samples.
#' Numerical distances are employed in this class,
#' which requires the ratio between x and y values.
#' When the x is datetime, nanosecond is a unit.
#' When the x is factor or character, it will be encoded into numeric codes.
#' @examples
#' data(noise_fluct)
#' agg <- LTTB_aggregator$new(interleave_gaps = TRUE)
#' d_agg <- agg$aggregate(
#'   x = noise_fluct$time, y = noise_fluct$f500, n_out = 1000
#'   )
#' plotly::plot_ly(x = d_agg$x, y = d_agg$y, type = "scatter", mode = "lines")
LTTB_aggregator <- R6::R6Class(
  "LTTB_aggregator",
  inherit = aggregator,
  public = list(
    #' @description
    #' Constructor of the aggregator.
    #' @param x_y_ratio,nt_y_ratio Numeric.
    #' These parameters set the unit length of the numeric \code{x}
    #' and \code{nanotime} x.
    #' For example, setting \code{x_y_ratio} to 2 is equivalent to
    #' assuming 2 is the unit length of \code{x}
    #' (and 1 is always the unit length of \code{y}).
    #' The unit length is employed to calculate the area of the triangles.
    #' @param interleave_gaps,coef_gap,NA_position,accepted_datatype,...
    #' Arguments pass to the constructor of \code{aggregator} object.
    #' Note that \code{accepted_datatype} has default value.
    initialize = function(
      ...,
      nt_y_ratio = 1e9, x_y_ratio = 1.0,
      interleave_gaps, coef_gap, NA_position,
      accepted_datatype = c("numeric", "integer", "character", "factor", "logical")
    ) {
      args <- c(as.list(environment()), list(...))
      do.call(super$initialize, args)
      private$nt_y_ratio <- nt_y_ratio
      private$x_y_ratio <- x_y_ratio
    }

  ),

  private = list(
    nt_y_ratio = 1,
    x_y_ratio = 1,
    aggregate_exec = function(x, y, n_out) {

      # if x is given as POSIX time, convert it to nanotime
      if (inherits(x, "POSIXt")) {
        convert_x <- TRUE
        if (inherits(x, "POSIXct")) {
          fun_convert_x <- as.POSIXct
        } else {
          fun_convert_x <- as.POSIXlt
        }
        x <- nanotime::as.nanotime(x)
      } else {
        convert_x <- FALSE
      }


      # if x is time,
      # convert it to integer64 and normalized using `nt_y_ratio`
      if (inherits(x, "nanotime")) {
        result <- private$LTTB(
          x = bit64::as.integer64(x - min(x, na.rm = TRUE)) / private$nt_y_ratio,
          y = dplyr::case_when(
            inherits(y, "character") ~ as.numeric(as.factor(y)),
            inherits(y, "factor")    ~ as.numeric(y),
            TRUE                     ~ as.numeric(y)
          ),
          n_out
        )

        # convert integer64 to nanotime
        result$x <- nanotime::as.nanotime(
          bit64::as.integer64(result$x * private$nt_y_ratio + min(x))
        )

      } else {
        # if x is NOT time,
        # the numeric distances are calculated based on the `x_y_ratio`.
        result <- private$LTTB(
          x = x / private$x_y_ratio,
          y = dplyr::case_when(
            inherits(y, "character") ~ as.numeric(as.factor(y)),
            inherits(y, "factor")    ~ as.numeric(y),
            TRUE                     ~ as.numeric(y)
          ),
          n_out
        )

        result$x <- result$x * private$x_y_ratio
      }

      if (convert_x) {
        result$x <- fun_convert_x(result$x)
      }

      if (inherits(y, "factor")) {
        result$y <- as.integer(result$y) %>%
          factor(labels = levels(y))
      }

      return(list(x = result$x, y = result$y))
    },

    #' Downsample with the Largest Triangle Three Buckets (LTTB) aggregation method
    LTTB = function(x, y, n_out) {

      # calculation of a triangle
      # @param A,B,C numeric vectors of which length is 2
      # @return number
      calcTriArea <- function(A, B, C) {
        return(
          0.5 * abs((A[1] - C[1]) * (B[2] - A[2]) - (A[1] - B[1]) * (C[2] - A[2]))
        )
      }

      assertthat::assert_that(
        length(x) == length(y),
        msg = "x and y must be the same-length vectors"
      )

      assertthat::assert_that(
        n_out > 2 & n_out < length(x),
        msg = "n_out is too small or too large"
      )

      N <- length(x)

      x_bins <- private$generate_matrix(x, n_out, remove_first_last = TRUE)
      y_bins <- private$generate_matrix(y, n_out, remove_first_last = TRUE)

      out <- matrix(NA, nrow = n_out, ncol = 2)
      out[1, ] <- c(x[1], y[1])
      out[n_out, ] <- c(x[N], y[N])

      for (i in 1:(n_out - 2)) {
        this_bin <- cbind(x_bins[, i], y_bins[, i])
        if (i < n_out - 2) {
          next_bin <- cbind(x_bins[, i + 1], y_bins[, i + 1])
        } else {
          next_bin <- matrix(c(x[N], y[N]), nrow = 1)
        }

        A  <- out[i, ]
        Bs <- this_bin
        C  <- apply(next_bin, 2, mean, na.rm = TRUE)

        areas <- apply(Bs, 1, calcTriArea, A = A, C = C)

        out[i + 1, ] <- Bs[which.max(areas), ]

      }

      return(list(x = out[, 1], y = out[, 2]))
    }

  )
)
