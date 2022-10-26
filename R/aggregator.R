#' R6 base class for the aggregation
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @importFrom R6 R6Class
#' @description
#' A base class for the aggregation,
#' which defines the structure of the class and
#' is not available on a stand-alone basis.
aggregator <- R6::R6Class(
  "aggregator",
  public = list(
    #' @field interleave_gaps Boolean.
    #' Whether \code{NA} values should be added
    #' when there are gaps / irregularly sampled data
    interleave_gaps  = TRUE,

    #' @field accepted_datatype Character.
    #' Classes that can be handled with the aggregator.
    accepted_datatype = NULL,

    #' @field nan_position Character.
    #' Where \code{NA}s are placed when gaps are detected.
    nan_position     = "end",

    #' @description
    #' Constructor of abstract_aggregator
    #' @param interleave_gaps Boolean, optional.
    #' Whether \code{NA} values should be added
    #' when there are gaps / irregularly sampled data.
    #' Gaps and irregular samples are detected with a quantile-based approach.
    #' By default, \code{FALSE}.
    #' @param nan_position Character, optional.
    #' Indicates where \code{NA}s are placed when gaps are detected.
    #' If \code{"end"}, the first point after a gap will be replaced.
    #' If \code{"begin"}, the last point before a gap will be replaced.
    #' If \code{"both"}, both the encompassing gap data points are replaced.
    #' This parameter is only effective
    #' when \code{interleave_gaps == TRUE}.
    #' @param accepted_datatype Character, optional.
    #' This parameter indicates the supported data classes.
    #' If all data classes are accepted, set it to \code{NULL}.
    initialize = function(
      interleave_gaps = FALSE, nan_position = "end", accepted_datatype = NULL
      ) {
      self$interleave_gaps   <- interleave_gaps
      self$accepted_datatype <- accepted_datatype
      self$nan_position      <- nan_position
    },

    #' @description
    #' Aggregates the given input and returns samples.
    #' @param x,y Indexes and values that has to be aggregated.
    #' @param n_out Integer or numeric.
    #' The number of samples that the aggregated data contains.
    aggregate = function(x, y, n_out) {
      assertthat::assert_that(
        length(x) == length(y),
        msg = "x and y must be the same length"
       )

      assertthat::assert_that(inherits(n_out, c("integer", "numeric")))

      # base case: the passed series is empty
      if (length(y) == 0) return(list(x = x, y = y))

      # assert that the datatype of y is acceptable
      if (!is.null(self$accepted_datatype)) {
        assertthat::assert_that(
          inherits(y, self$accepted_datatype),
          msg = paste(
            "Data type of the y is", paste(class(y), collapse = "/"),
            ", which doesn't match with the accepted data classes (",
            paste(self$accepted_datatype, collapse = "/"),
            ")"
          )
        )
      }

      # More samples that n_out -> perform data aggregation
      if (length(x) > n_out) {
        result <- private$aggregate_exec(x, y, n_out)

        # Replace the end of gap periods (i.e. the first non-gap sample) with NA
        if (self$interleave_gaps) {
          resultxy <- private$replace_gap_end_none(result$x, result$y)
          result$x <- resultxy$x
          result$y <- resultxy$y
        }
      } else { # Less samples than n_out -> no data aggregation

        result <- list(x = x, y = y)

        # gaps are inserted instead of replaced
        if (self$interleave_gaps) {
          resultxy <- private$insert_gap_none(result$x, result$y)
          result$x <- resultxy$x
          result$y <- resultxy$y
        }
      }

      return(result)
    }

  ),
  private = list(

    # method to aggregate x and y, which is defined in the sub class
    aggregate_exec = function(x, y, n_out) {},

    # divide and conquer heuristic to calculate the median diff
    calc_key_diff = function(x) {

      if (inherits(x, "integer64")) {
        all_diff <- as.numeric(x - dplyr::lag(x))
      } else {
        all_diff <- c(0, diff(x))
      }
      # To do so - use a quantile-based (median) approach
      # where we reshape the data
      # into `n_blocks` blocks and calculate the min
      n_blcks <- 128
      if (length(x) > 5 * n_blcks) {
        blck_size <- length(x) %/% n_blcks

        # calculate the min and max and calculate the median on that
        med_diff <- matrix(
          all_diff[1:(blck_size * n_blcks)], nrow = n_blcks, byrow = TRUE
         ) %>%
          apply(1, mean, na.rm = TRUE) %>%
          median()
      } else {
        med_diff <- median(all_diff, na.rm = TRUE)
      }

      return(list(median = med_diff, all = all_diff))
    },


    # Insert NA between gaps / irregularly sampled data
    insert_gap_none = function(x, y) {

      x_diff <- private$calc_key_diff(x)

      # add None data-points in-between the gaps
      if (!is.null(x_diff$median)) {
        gap_key_idx <- which(x_diff$all > 3 * x_diff$median)
        if (length(gap_key_idx) > 0) {
          gap_key_idx <- gap_key_idx + seq(0, (length(gap_key_idx) - 1))
          for (idx in gap_key_idx) {
            x <- c(x[1:(idx - 1)], NA, x[idx:length(x)])
            y <- c(y[1:(idx - 1)], NA, y[idx:length(y)])
          }
        }
      }
      return(list(x = x, y = y))
    },

    # Replace NA where a gap ends
    replace_gap_end_none = function(x, y) {

      x_diff <- private$calc_key_diff(x)
      if (!is.null(x_diff$median)) {
        # Replace data-points with NA where the gaps occur
        # The default is the end of a gap
        nan_mask <- x_diff$all > 4 * x_diff$median
        if (self$nan_position == "begin") {
          # Replace the last non-gap datapoint (begin of gap) with NA
          nan_mask <- c(nan_mask[2:length(nan_mask)], TRUE)
        } else if (self$nan_position == "both") {
          # Replace the encompassing gap datapoints with NA
          nan_mask  <- nan_mask | c(nan_mask[2:length(nan_mask)], TRUE)
        }
        x[nan_mask] <- NA
        y[nan_mask] <- NA
      }
      return(list(x = x, y = y))
    },

    #' Generate a matrix using x and n_out
    generate_matrix = function(x, n_out, remove_first_last = TRUE) {

      if (remove_first_last) {
        N <- length(x) - 2
        n_out <- n_out - 2
      } else {
        N <- length(x)
      }

      bin_width <- c(
        rep(ceiling(N / n_out), N %% n_out),
        rep(floor(N / n_out), n_out - N %% n_out)
      )

      idx <- purrr::map2(
        bin_width, dplyr::lag(cumsum(bin_width), default = 0),
        ~c(.y + seq(1, .x), rep(NA, max(bin_width) - .x))
      ) %>%
        unlist()

      if (remove_first_last) idx <- idx + 1

      if (inherits(x, "integer64")) {
        x <- bit64::as.integer64(x)
        m <- x[idx]
        dim(m) <- c(length(idx) %/% n_out, n_out)
      } else {
        m <- matrix(x[idx], ncol = n_out, byrow = FALSE)
      }

      return(m)
    },

    #' Apply function for nanotime
    apply_nano64 = function(mat, margin, func){
      assertthat::assert_that(inherits(mat, "integer64"))
      assertthat::assert_that(inherits(margin, c("numeric", "integer")))
      assertthat::assert_that(inherits(func, "function"))

      margin <- as.integer(margin)
      assertthat::assert_that(margin %in% c(1L, 2L))

      nm <- seq(1, dim(mat)[margin])
      v <- as.nanotime(nm)

      if (margin == 1L) {
        for (i in nm) {
          v[i] <- as.nanotime(func(mat[i, ]))
        }
      } else {
        for (i in nm) {
          v[i] <- as.nanotime(func(mat[, i]))
        }
      }
      return(v)
    }
  )
)
