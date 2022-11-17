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

    #' @description
    #' Constructor of abstract_aggregator
    #' @param ... Not used.
    #' @param interleave_gaps,NA_position,coef_gap,accepted_datatype
    #' Arguments passed to \code{self$set_parameters}, optional.
    initialize = function(
      ...,
      interleave_gaps = FALSE, NA_position = "begin", coef_gap = 3.0,
      accepted_datatype = NULL
      ) {
      self$set_parameters(
        interleave_gaps   = interleave_gaps,
        coef_gap          = coef_gap,
        NA_position       = NA_position,
        accepted_datatype = accepted_datatype
      )
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
      if (!is.null(private$accepted_datatype)) {
        assertthat::assert_that(
          inherits(y, private$accepted_datatype),
          msg = paste(
            "Data type of the y is", paste(class(y), collapse = "/"),
            ", which doesn't match with the applicable data classes (",
            paste(private$accepted_datatype, collapse = "/"),
            ")"
          )
        )
      }

      # if the samples are less than n_out, return input
      if (length(x) < n_out) {

        result <- list(x = x, y = y)

      # if not, perform aggregation
      } else {

        result <- private$aggregate_exec(x, y, n_out)

      }

      # if interleve_gaps is TRUE, procedure for irregular gaps is conducted
      if (private$interleave_gaps) {

        # compute where the irregular gaps are
        gap_idx <- private$cmpt_gap_idx(result$x)

        # if no aggregation, insert NAs at the irregular gaps
        # note that the length might be longer than n_out
        if (length(x) < n_out) {

          result_grp <- cumsum(is.element(seq_along(x), gap_idx + 1))

          insertNA <- function(out, input) return(c(out, NA, input))

          result <- purrr::map(
            result,
            ~split(.x, result_grp) %>%
              purrr::reduce(insertNA)
          )

        # if aggregation is performed, replace the gaps with NA
        # the length of the results is not changed
        } else if(length(gap_idx) > 0) {

          if (private$NA_position == "begin") {
            gap_idx <- gap_idx
          } else if (private$NA_position == "both") {
            gap_idx <- c(gap_idx, gap_idx + 1)
          } else {
            gap_idx <- gap_idx + 1
          }

          gap_idx <- gap_idx[gap_idx >= 1 & gap_idx <= length(result$x)]

          result <- purrr::map(
            result,
            function(x) {
              x[gap_idx] <- NA
              return(x)
            }
          )
        }
      }



      return(result)
    },

    #' @description
    #' Setting of the parameters for the aggregation
    #' @param ... Not used.
    #' @param interleave_gaps Boolean, optional.
    #' Whether \code{NA} values should be added
    #' when there are gaps / irregularly sampled data.
    #' Irregular gaps between samples are determined whether the gap is larger than
    #' the median of the sample gaps times the coefficient for detecting irregular gaps.
    #' By default, \code{FALSE}.
    #' @param NA_position Character, optional.
    #' Indicates where \code{NA}s are placed when gaps are detected.
    #' If \code{"end"}, the first point after a gap will be replaced.
    #' If \code{"begin"}, the last point before a gap will be replaced.
    #' If \code{"both"}, both the encompassing gap data points are replaced.
    #' This parameter is only effective when \code{interleave_gaps == TRUE}.
    #' By default, \code{"begin"}.
    #' @param coef_gap Numeric, optional.
    #' The coefficient to detect irregular gaps.
    #' By default, 3.0.
    #' @param accepted_datatype Character, optional.
    #' This parameter indicates the supported data classes.
    #' If all data classes are accepted, set it to \code{NULL}.
    set_parameters = function(
      ...,
      interleave_gaps, NA_position, coef_gap, accepted_datatype
    ) {
      if (!missing(interleave_gaps)) {
        assertthat::assert_that(inherits(interleave_gaps, "logical"))
        private$interleave_gaps <- interleave_gaps
      }

      if (!missing(NA_position)) {
        assertthat::assert_that(inherits(NA_position, "character"))
        private$NA_position <- NA_position
      }

      if (!missing(coef_gap)) {
        assertthat::assert_that(inherits(coef_gap, c("numeric", "integer")))
        private$coef_gap <- as.numeric(coef_gap)
      }

      if (!missing(accepted_datatype)) {
        assertthat::assert_that(
          inherits(accepted_datatype, "character") || is.null(accepted_datatype)
        )
        private$accepted_datatype <- accepted_datatype
      }

      invisible()
    }

  ),

  active = list(
    #' @field parameters Parameters for the aggregation, returned as a named list.
    parameters = function() {
      list(
        interleave_gaps   = private$interleave_gaps,
        NA_position       = private$NA_position,
        coef_gap          = private$coef_gap,
        accepted_datatype = private$accepted_datatype
      )
    }
  ),

  private = list(

    # Whether the procedure for the large gaps is performed
    interleave_gaps  = TRUE,

    # accepted class
    accepted_datatype = NULL,

    # whether the (begin/both/end) of the gaps are replaced with NA
    NA_position     = "end",

    # how large the "large gap" compared to the median of the gap
    coef_gap = 3.0,

    # method to aggregate x and y, which is defined in the sub class
    aggregate_exec = function(x, y, n_out) {},

    # find where the gap is large and returns the index
    cmpt_gap_idx = function(x) {
      x_diff <- as.numeric(x - dplyr::lag(x)) %>% na.omit()
      x_diff_med <- median(x_diff, na.rm = TRUE)

      if (is.na(x_diff_med)) return(integer())

      # compute index of which gap with the next value is large
      gap_idx <- which(x_diff > private$coef_gap * x_diff_med)
      return(gap_idx)
    },

    #' Generate a matrix using x and n_out
    generate_matrix = function(x, n_out, remove_first_last = TRUE) {

      if (remove_first_last) {
        N <- length(x) - 2
        n_out <- n_out - 2
      } else {
        N <- length(x)
      }

      if (N < 1e6) {
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
        mx <- x[idx]
      } else {
        if (remove_first_last) x <- x[2:N]
        mx <- c(x, rep(NA, ceiling(N / n_out) * n_out - N))
      }


      if (inherits(x, "integer64")) {
        m <- bit64::as.integer64(mx)
      } else {
        m <- mx
      }
      dim(m) <- c(length(mx) %/% n_out, n_out)

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
