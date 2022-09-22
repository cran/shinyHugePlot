#' R6 abstract class for the aggregation
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @importFrom R6 R6Class
#' @description
#' An abstract class for the aggregation,
#' which defines the structure of the class and
#' is not available on a stand-alone basis.
abstract_aggregator <- R6::R6Class(
  "abstract_aggregator",
  public = list(
    #' @field interleave_gaps Whether \code{NA} values should be added
    #' when there are gaps / irregularly sampled data
    interleave_gaps  = TRUE,

    #' @field accepted_datatype Vector of supported data classes
    accepted_datatype = NULL,

    #' @field nan_position Character that indicates
    #' where \code{NA}s are placed when gaps are detected
    nan_position     = "end",

    #' @description
    #' Constructor of abstract_aggregator
    #' @param interleave_gaps Boolean, optional.
    #' Whether \code{NA} values should be added
    #' when there are gaps / irregularly sampled data.
    #' A quantile-based approach is employed.
    #' By default, \code{FALSE}.
    #' @param nan_position Character, optional.
    #' Indicates where \code{NA}s are placed when gaps are detected.
    #' If \code{"end"}, the first point after a gap will be replaced.
    #' If \code{"begin"}, the last point before a gap will be replaced.
    #' If \code{"both"}, both the encompassing gap data points are replaced.
    #' This parameter is only effective
    #' when \code{interleave_gaps == TRUE}.
    #' @param accepted_datatype Character vector, optional.
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
    #' @param n_out Integer.
    #' The number of samples that the aggregated data contains.
    aggregate = function(x, y, n_out) {
      assertthat::assert_that(
        length(x) == length(y),
        msg = "x and y must be the same length!"
       )

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
          resultxy <- private$insert_gap_none(x, y)
          result$x <- resultxy$x
          result$y <- resultxy$y
        }
      }

      return(result)
    }

 ),
  private = list(
    # abstract method that must be defined in the sub class
    aggregate_exec = function(x, y, n_out) {
      return(list(x = x[1:n_out], y = y[1:n_out]))
      },

    # @description
    # divide and conquer heuristic to calculate the median diff
    # @param x vectors of indexes
    # @returns named list contains `median` and `all` of
    # the differences of indexes
    calc_key_diff = function(x) {

      # remark: thanks to the prepend -> s_idx_diff.shape === len(s)
      all_diff <- c(0, diff(x))

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


    # @description
    # Insert NA between gaps / irregularly sampled data
    # @param x,y vectors of indexes and values
    # @returns named list contains `x` and `y` vectors where NAs are inserted
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


    # @description
    # Replace NA where a gap ends
    # @param x,y vectors of indexes and values
    # @returns named list contains `x` and `y` vectors
    # where some keys and values are replaced with NAs
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
    }

 )
)
