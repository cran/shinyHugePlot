#' Aggregation using Largest Triangle Three Buckets (LTTB) method.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' The LTTB method aggregates the huge samples using the areas
#' of the triangles formed by the samples.
#' Numerical distances are employed in this class,
#' which requires the ratio between x and y values.
#' When the x is datetime, nanosecond is a unit.
#' When the x is factor or character, it will be encoded into numeric codes.
#' @examples
#' data(noise_fluct)
#' agg <- LTTB_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
LTTB_aggregator <- R6::R6Class(
  "LTTB_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    #' @param x_y_ratio,nt_y_ratio Numeric.
    #' These parameters set the unit length of the numeric \code{x}
    #' and \code{nanotime} x.
    #' For example, setting \code{x_y_ratio} to 2 is equivalent to
    #' assuming 2 is the unit length of \code{x}
    #' (and 1 is always the unit length of \code{y}).
    #' The unit length is employed to calculate the area of the triangles.
    initialize = function(
      interleave_gaps = FALSE, nan_position = "end",
      nt_y_ratio = 1e9, x_y_ratio = 1.0
      ) {

      super$initialize(
        interleave_gaps,
        nan_position,
        accepted_datatype = c(
          "numeric", "integer", "character", "factor", "logical"
          )
      )
      private$nt_y_ratio <- nt_y_ratio
      private$x_y_ratio <- x_y_ratio
    }
  ),
  private = list(
    nt_y_ratio = 1,
    x_y_ratio = 1,
    aggregate_exec = function(x, y, n_out) {

      # if x is time,
      # convert it to integer64 and normalized using `nt_y_ratio`
      if (inherits(x, "nanotime")) {
        result <- LTTB(
          x = bit64::as.integer64(x - min(x)) / private$nt_y_ratio,
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
        result <- LTTB(
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

      if (inherits(y, "factor")) {
        result$y <- as.integer(result$y) %>%
          factor(labels = levels(y))
      }

      return(list(x = result$x, y = result$y))
    }
  )
)


#' R6 Class for Min-Max Aggregation that has 50\% overlapping windows.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into 50\% overlapping intervals
#' and find the maximum and minimum values of each.
#' @examples
#' data(noise_fluct)
#' agg <- min_max_ovlp_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
min_max_ovlp_aggregator <- R6::R6Class(
  "min_max_ovlp_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(interleave_gaps = FALSE, nan_position = "end") {
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      # The block size 2x the bin size we also perform the ceiling
      block_size <- floor(length(x) / (n_out - 1) * 2)
      argmax_offset <- block_size %/% 2

      # Calculate the offset range,
      # which will be added to the argmin and argmax pos
      offset <- seq(1, length(x) - block_size - argmax_offset, block_size)

      # Calculate the argmin & argmax

      argmin <- y[2:(1 + block_size * length(offset))] %>%
        matrix(nrow = block_size) %>%
        apply(2, which.min) + offset

      argmax <- y[argmax_offset + 2:(1 + block_size * length(offset))] %>%
        matrix(nrow = block_size) %>%
        apply(2, which.max) + offset + argmax_offset


      # Sort the argmin & argmax
      # (where we append the first and last index item)
      # and then slice the original series on these indexes.
      idx <- sort(c(1, argmin, argmax, length(x)))
      return(list(x = x[idx], y = y[idx]))
    }
  )
)

#' R6 Class for Min-Max Aggregation with fully overlapping windows
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into fully overlapping intervals
#' and find the maximum and minimum values of each.
#' @examples
#' data(noise_fluct)
#' agg <- min_max_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
min_max_aggregator <- R6::R6Class(
  "min_max_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(interleave_gaps = FALSE, nan_position = "end") {
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {

      block_size <- floor(length(x) / (n_out - 2) * 2)

      # Offset range which will be added to the argmin and argmax pos
      offset <- seq(1, length(x) - block_size, block_size)

      # Calculate the argmin & argmax

      argmin <- y[2:(1 + block_size * length(offset))] %>%
        matrix(nrow = block_size) %>%
        apply(2, which.min) + offset

      argmax <- y[2:(1 + block_size * length(offset))] %>%
        matrix(nrow = block_size) %>%
        apply(2, which.max) + offset

      # Sort the argmin & argmax (where we append the first and last index item)
      # and then slice the original series on these indexes.
      idx <- sort(c(1, argmin, argmax, length(x)))
      return(list(x = x[idx], y = y[idx]))
    }
  )
)


#' R6 Class for Efficient LTTB aggregation
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
#' agg <- eLTTB_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
eLTTB_aggregator <- R6::R6Class(
  "eLTTB_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @field LTTB An R6 LTTB_aggregator instance
    LTTB = NULL,

    #' @field minmax An R6 \code{min_max_ovlp_aggregator} instance
    minmax = NULL,

    #' @description
    #' Constructor of the aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(interleave_gaps = FALSE, nan_position = "end") {
      self$LTTB   <- LTTB_aggregator$new(interleave_gaps = FALSE)
      self$minmax <- min_max_ovlp_aggregator$new(interleave_gaps = FALSE)

      super$initialize(
        interleave_gaps,
        nan_position,
        accepted_datatype <- c(
          "numeric", "integer", "character", "factor", "logical"
          )
      )
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


#' R6 Class for Naive (but fast) aggregation which returns every Nth point.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Aggregation by extracting every Nth data.
#' @examples
#' data(noise_fluct)
#' agg <- nth_pnt_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
nth_pnt_aggregator <- R6::R6Class(
  "nth_pnt_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(interleave_gaps = FALSE, nan_position = "end") {
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      idx <- seq(1, length(x), max(1, ceiling(length(x) / n_out)))
        return(list(x = x[idx], y = y[idx]))
    }
  )
)


#' R6 Class for aggregation which returns the custom statistical values
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' This aggregator divides the data into no-overlapping intervals
#' and calculate specific statistical values such as the mean.
#' @examples
#' data(noise_fluct)
#' agg <- custom_stat_aggregator$new(y_func = function(x) mean(x, na.rm = TRUE))
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
custom_stat_aggregator <- R6::R6Class(
  "custom_stat_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    #' @param y_func Function.
    #' Statistical values are calculated using this function.
    #' By default, \code{mean}.
    initialize = function(
      y_func = function(x) mean(x, na.rm = TRUE),
      interleave_gaps = FALSE, nan_position = "end"
      ) {
      private$y_func <- y_func
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    y_func = NULL,
    aggregate_exec = function(x, y, n_out) {
      x_agg <- apply(
        generate_matrix(x, n_out, remove_first_last = FALSE),
        1, mean, na.rm = TRUE
      )
      y_mat <- generate_matrix(y, n_out, remove_first_last = FALSE)

      y_agg <- apply(y_mat, 1, private$y_func)

      return(list(x = x_agg, y = y_agg))
    }
  )
)



#' R6 Class for aggregation which returns the 3 types of the statistical values
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' This aggregator divides the data into no-overlapping intervals
#' and calculate specific statistical values such as the mean.
#' @examples
#' data(noise_fluct)
#' agg <- range_stat_aggregator$new(ylwr = min, y = mean, yupr = max)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$ylwr, type = "l")
#' plot(d_agg$x, d_agg$y, type = "l")
#' plot(d_agg$x, d_agg$yupr, type = "l")
#'
range_stat_aggregator <- R6::R6Class(
  "custom_stat_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    #' @param yupr,y,ylwr Functions.
    #' Statistical values are calculated using this function.
    #' By default, \code{max, mean, min}, respectively.
    #' Note that the functions need to deal with NA values.
    initialize = function(
      ylwr = function(x) min(x, na.rm = TRUE),
      y = function(x) mean(x, na.rm = TRUE),
      yupr = function(x) max(x, na.rm = TRUE),
      interleave_gaps = FALSE, nan_position = "end"
    ) {
      private$ylwr <- ylwr
      private$y    <- y
      private$yupr <- yupr
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    ylwr = NULL,
    y    = NULL,
    yupr = NULL,

    aggregate_exec = function(x, y, n_out) {

      x_agg <- apply(
        generate_matrix(x, n_out, remove_first_last = FALSE),
        1, mean, na.rm = TRUE
        )
      y_mat <- generate_matrix(y, n_out, remove_first_last = FALSE)

      y_agg <- apply(y_mat, 1, private$y)
      yupr_agg <- apply(y_mat, 1, private$yupr)
      ylwr_agg <- apply(y_mat, 1, private$ylwr)

      return(list(x = x_agg, ylwr = ylwr_agg, y = y_agg, yupr = yupr_agg))
    }
  )
)


#' R6 Class for aggregation which returns the mean values
#'
#' @export
#' @format An \code{R6::R6Class} object
#' @description
#' The mean value for each interval is calculated.
mean_aggregator <- R6::R6Class(
  "mean_aggregator",
  inherit = custom_stat_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(interleave_gaps = FALSE, nan_position = "end"
    ) {
      super$initialize(
        y_func = function(x) mean(x, na.rm = TRUE),
        interleave_gaps, nan_position
        )
    }
  )
)



#' R6 Class for aggregation which returns the median values
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' The median value for each interval is calculated.
median_aggregator <- R6::R6Class(
  "mean_aggregator",
  inherit = custom_stat_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(
      interleave_gaps = FALSE, nan_position = "end"
    ) {
      super$initialize(
        y_func = function(x) median(x, na.rm = TRUE),
        interleave_gaps, nan_position
      )
    }
  )
)


#' R6 Class for aggregation which returns the max values
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' The maximum value for each interval is calculated.
max_aggregator <- R6::R6Class(
  "mean_aggregator",
  inherit = custom_stat_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(
      interleave_gaps = FALSE, nan_position = "end"
    ) {
      super$initialize(
        y_func = function(x) max(x, na.rm = TRUE),
        interleave_gaps, nan_position
      )
    }
  )
)



#' R6 Class for aggregation which returns the minimum values
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' The minimum value for each interval is calculated.
min_aggregator <- R6::R6Class(
  "mean_aggregator",
  inherit = custom_stat_aggregator,
  public = list(
    #' @description
    #' Constructor of the Aggregator.
    #' @param interleave_gaps,nan_position
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(
    interleave_gaps = FALSE, nan_position = "end"
    ) {
      super$initialize(
        y_func = function(x) min(x, na.rm = TRUE),
        interleave_gaps, nan_position
      )
    }
  )
)

#' R6 Class for Aggregation using a user-defined function.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Arbitrary function can be applied using this aggregation class.
#' @examples
#' custom_agg_func <- function(x, y, n_out) {
#'   bin_width <- floor(length(x)/n_out)
#'   x_idx <- seq(floor(bin_width / 2), bin_width * n_out, bin_width)
#'   y_mat <- y[1:(bin_width * n_out)] %>%
#'     matrix(nrow = bin_width)
#'   y_agg <- apply(y_mat, 2, quantile, probs = 0.25)
#'   return(list(x = x[x_idx], y = y_agg))
#' }
#' data(noise_fluct)
#' agg <- custom_func_aggregator$new(custom_agg_func)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
custom_func_aggregator <- R6::R6Class(
  "custom_func_aggregator",
  inherit = abstract_aggregator,
  public = list(
    #' @field aggregation_func
    #' User-defined function to aggregate data,
    #' of which arguments are \code{x}, \code{y} and \code{n_out}.
    aggregation_func = NULL,
    #' @description
    #' Constructor of the Aggregator.
    #' @param aggregation_func
    #' User-defined function to aggregate data,
    #' of which arguments are \code{x}, \code{y} and \code{n_out}.
    #' @param interleave_gaps,nan_position,accepted_datatype
    #' Arguments pass to the constructor of
    #' the \code{abstract_aggregator} class.
    initialize = function(
      aggregation_func, interleave_gaps = FALSE,
      nan_position = "end", accepted_datatype = NULL) {

      super$initialize(interleave_gaps, nan_position, accepted_datatype)
      self$aggregation_func <- aggregation_func
    }
  ),
  private = list(
    aggregate_exec = function(x, y, n_out) {
      return(self$aggregation_func(x, y, n_out))
    }
  )
)
