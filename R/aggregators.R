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


#' Aggregation using local minimum and maximum values
#' of which small data ranges have 50\% overlaps.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into 50\% overlapping intervals
#' and find the maximum and minimum values of each.
#' \code{n_out} must be even number.
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

      n_minmax <- n_out / 2 - 1
      # block_size * (n_minmax + 0.5) = data_num
      block_size <- floor((length(x) - 2) / (n_minmax + 0.5))

      y_mat_1 <- generate_matrix(
        y[2:(length(x) - 1 - block_size %/% 2 )],
        n_minmax, remove_first_last = FALSE
      )

      y_mat_1_values <- apply(y_mat_1, 1, function(x) sum(!is.na(x)))

      y_mat_2 <- generate_matrix(
        y[(2 + block_size %/% 2):(length(x) - 1)],
        n_minmax, remove_first_last = FALSE
      )

      y_mat_2_values <- apply(y_mat_2, 1, function(x) sum(!is.na(x)))

      idx_min <- 1 +
        purrr::map_int(1:n_minmax, ~which.min(y_mat_1[.x,])) +
        c(0, cumsum(y_mat_1_values)[1:(n_minmax - 1)])

      idx_max <- block_size %/% 2 +
        purrr::map_int(1:n_minmax, ~which.max(y_mat_2[.x,])) +
        c(0, cumsum(y_mat_2_values)[1:(n_minmax - 1)])

      idx <- c(1, idx_min, idx_max, length(x)) %>% sort()

      return(list(x = x[idx], y = y[idx]))
    }
  )
)

#' Aggregation using local minimum and maximum values.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' Divide the data into small data ranges
#' and find the maximum and minimum values of each.
#' \code{n_out} must be even number.
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
      n_minmax <- n_out / 2 - 1

      y_mat <- generate_matrix(
        y[2:(length(x) - 1)], n_minmax, remove_first_last = FALSE
        )
      y_mat_values <- apply(y_mat, 1, function(x) sum(!is.na(x)))

      idx_min <- 1 +
        purrr::map_int(1:n_minmax, ~which.min(y_mat[.x,])) +
        c(0, cumsum(y_mat_values)[1:(n_minmax - 1)])

      idx_max <- 1 +
        purrr::map_int(1:n_minmax, ~which.max(y_mat[.x,])) +
        c(0, cumsum(y_mat_values)[1:(n_minmax - 1)])

      idx <- c(1, idx_min, idx_max, length(x)) %>% sort()

      return(list(x = x[idx], y = y[idx]))

    }
  )
)


#' Aggregation using local minimum and maximum values,
#' and Largest Triangle Three Buckets (LTTB) method.
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


#' Aggregation which returns every Nth point.
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


#' NULL aggregator.
#'
#' @export
#' @docType class
#' @format An \code{R6::R6Class} object
#' @description
#' It does not aggregate the data but returns the full samples within the range.
#' @examples
#' data(noise_fluct)
#' agg <- null_aggregator$new()
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level)
#' plot(d_agg$x[1:100], d_agg$y[1:100], type = "l")
null_aggregator <- R6::R6Class(
  "null_aggregator",
  public = list(
    #' @description
    #' Constructor that changes nothing.
    initialize = function() {},
    #' @description
    #' Function where no aggregation will be executed.
    #' @param x,y Vectors.
    #' @param n_out Integer, omitted.
    aggregate = function(x, y, n_out = -1){
      return(list(x = x, y = y))
    }
  )
)




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
#' agg <- range_stat_aggregator$new(ylwr = min, y = mean, yupr = max)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$ylwr, type = "l")
#' plot(d_agg$x, d_agg$y, type = "l")
#' plot(d_agg$x, d_agg$yupr, type = "l")
#'
range_stat_aggregator <- R6::R6Class(
  "range_stat_aggregator",
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
    #' Note that the NA values are omitted automatically.
    initialize = function(
      ylwr = min,
      y = mean,
      yupr = max,
      interleave_gaps = FALSE, nan_position = "end"
    ) {
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
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)
    }
  ),
  private = list(
    ylwr = NULL,
    y    = NULL,
    yupr = NULL,

    aggregate_exec = function(x, y, n_out) {

      x_mat <- generate_matrix(x, n_out, remove_first_last = FALSE)
      x_agg <- apply(x_mat, 1, mean, na.rm = TRUE)

      y_mat <- generate_matrix(y, n_out, remove_first_last = FALSE)

      yupr_agg <- apply(y_mat, 1, private$yupr)
      ylwr_agg <- apply(y_mat, 1, private$ylwr)
      if (!is.null(private$y)) {
        y_agg    <- apply(y_mat, 1, private$y)
      } else {
        y_agg <- NA
      }

      return(list(x = x_agg, ylwr = ylwr_agg, y = y_agg, yupr = yupr_agg))
    }
  )
)


#' Aggregation using a user-defined function.
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
    #' @param aggregation_func Function.
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
#' agg <- custom_stat_aggregator$new(y_func = mean)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
#'
#' agg <- custom_stat_aggregator$new(y_func = max, x_mean = FALSE)
#' d_agg <- agg$aggregate(noise_fluct$sec, noise_fluct$level, 1000)
#' plot(d_agg$x, d_agg$y, type = "l")
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
    #' @param x_mean Boolean.
    #' Whether using the mean values or not for the x values.
    #' If not, the x values that give the specific y values are used.
    #' E.g., if you use \code{max} as the \code{aggregation_func} and
    #' set this argument to \code{FALSE}, x values that give the maximum
    #' y values are used.
    #' By default, \code{TRUE}.
    initialize = function(
    y_func = mean,
    x_mean = TRUE,
    interleave_gaps = FALSE, nan_position = "end"
    ) {
      private$y_func <- function(x) y_func(na.omit(x))
      private$x_mean <- x_mean
      super$initialize(interleave_gaps, nan_position, accepted_datatype = NULL)

    }
  ),
  private = list(
    y_func = NULL,
    x_mean = NULL,
    aggregate_exec = function(x, y, n_out) {

      y_mat <- generate_matrix(y, n_out, remove_first_last = FALSE)
      y_agg <- apply(y_mat, 1, private$y_func)

      x_mat <- generate_matrix(x, n_out, remove_first_last = FALSE)
      if (private$x_mean) {
        x_agg <- apply(x_mat, 1, mean, na.rm = TRUE)
      } else {
        x_idx <- purrr::map_int(
          seq_along(y_agg),
          ~if_else(
            y_agg[.x] %in% y_mat[.x,],
            which(y_mat[.x,] == y_agg[.x])[1],
            NA_integer_
            )
        )
        x_agg <- purrr::map(seq_along(y_agg), ~x_mat[.x, x_idx[.x]]) %>%
          unlist()
      }

      return(list(x = x_agg, y = y_agg))
    }
  )
)
