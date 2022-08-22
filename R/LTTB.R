#' Downsample with the Largest Triangle Three Buckets (LTTB) aggregation method
#'
#' @importFrom assertthat assert_that
#' @param x,y numeric vectors.
#' @param n_out length of the output.
#' This must be larger than 2 and lesser than the number of the rows of `data`
#' @return named list of `x` and `y`
LTTB <- function(x, y, n_out) {

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

  x_bins <- generate_matrix(x, n_out)
  y_bins <- generate_matrix(y, n_out)

  out <- matrix(NA, nrow = n_out, ncol = 2)
  out[1, ] <- c(x[1], y[1])
  out[n_out, ] <- c(x[N], y[N])

  for (i in 1:(n_out - 2)) {
    this_bin <- cbind(x_bins[i, ], y_bins[i, ])
    if (i < n_out - 2) {
      next_bin <- cbind(x_bins[i + 1, ], y_bins[i + 1, ])
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
