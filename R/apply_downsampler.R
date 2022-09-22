#' Wrapper for applying down-sampling using aggregation function
#'
#' @description
#' This function is a wrapper that employs an aggregation function.
#' @param obj Numeric vector or data.frame.
#' If a numeric vector is given, a specific down-sampling method will be
#' employed and a down-sampled vector will be returned.
#' If a data.frame is given, a specific down-sampling method will be employed
#' using \code{x} and \code{y} columns and a down-sampled data.frame
#' will be returned.
#' @param n_out Integer, optional.
#' Number of samples get by the down-sampling. By default, 1000.
#' @param aggregator R6 class for the aggregation, optional.
#' Select an aggregation function. The list of the functions are obtained
#' using \code{list_aggregators}.
#' By default, \code{eLTTB_aggregator}.
#' @param ... Not used.
#' @export
#' @examples
#' data(noise_fluct)
#'
#' y_agg <- apply_downsampler(noise_fluct$level)
#'
#' d_agg <- noise_fluct %>%
#'   dplyr::select(x = sec, y = level) %>%
#'   apply_downsampler()
#'
apply_downsampler <- function(obj, ...) {
  UseMethod("apply_downsampler", obj)
}


#' @rdname apply_downsampler
#' @export
apply_downsampler.numeric <- function(
    obj,
    n_out = 1000L,
    aggregator = eLTTB_aggregator,
    ...
) {
  agg <- aggregator$new()
  y_agg <- agg$aggregate(
    seq_along(obj), obj, n_out
  )

  return(y_agg$y)
}

#' @rdname apply_downsampler
#' @export
apply_downsampler.data.frame <- function(
  obj,
  n_out = 1000L,
  aggregator = eLTTB_aggregator,
  ...
  ) {
  agg <- aggregator$new()
  d_agg <- agg$aggregate(
    obj$x, obj$y, n_out
    ) %>%
    tibble::as_tibble()
  return(d_agg)
}
