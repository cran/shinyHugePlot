#' Conversion of nanotime to plotly-style time
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace
#' @importFrom assertthat assert_that
#' @importFrom nanotime as.nanoduration
#' @param time nanotime class time.
#' @param tz time zone (e.g. "Asia/Tokyo")
#' @return local time (strings) with the format of %Y-%m-%d %H:%M:%E9S
#' @description
#' Plotly does not accept 64-bit nanotime format,
#' so the conversion is necessary to import the time to plotly.
#'
nanotime_to_plotlytime <- function(time, tz) {
  assertthat::assert_that(
    inherits(time, "nanotime"),
    msg = "Only the nanotime class is acceptable"
  )

  tz_hms <- format(as.POSIXct("1970-01-01", tz), "%z") %>%
    stringr::str_replace("^(\\-?)\\+?([0-9]{1,2})([0-9]{2})", "\\1\\2:\\3:00")

  time_plotly <- (time + nanotime::as.nanoduration(tz_hms)) %>%
    format("%Y-%m-%d %H:%M:%E9S")

  return(time_plotly)
}

#' Conversion of plotly-style time to nanotime
#'
#' @importFrom nanotime as.nanotime
#' @param time local time (strings) with the format of
#' `\%Y-\%m-\%d \%H:\%M:\%S.s`
#' @param tz time zone (e.g. "Asia/Tokyo")
#' @return nanotime
#' @description
#' Datetime obtained from plotly are converted to 64-bit nanotime format.
#'
plotlytime_to_nanotime <- function(time, tz) {
  nanotime::as.nanotime(time, tz = tz)
}

#' Generate an appropriate time-based label using nanotime value
#'
#' @description
#' Nano-second time is converted to an appropriate label.
#' e.g., 1e9 nano seconds will be converted to \code{"1.0s"}
#' @importFrom dplyr case_when
#' @param ns Numeric value(s) representing nano second.
#' @return Character.
#'
nanosecond_to_label <- function(ns) {
  thr     <- 0.1
  us_unit <- 1e3
  ms_unit <- 1e6
  s_unit  <- 1e9
  m_unit  <- 1e9 * 60
  h_unit  <- 1e9 * 60 * 60
  d_unit  <- 1e9 * 60 * 60 * 24
  w_unit  <- 1e9 * 60 * 60 * 24 * 7
  mo_unit <- 1e9 * 60 * 60 * 24 * 30
  yr_unit <- 1e9 * 60 * 60 * 24 * 365

  output_chr <- dplyr::case_when(
    ns > thr * yr_unit ~ sprintf("%.1fy",  ns / yr_unit),
    ns > thr * mo_unit ~ sprintf("%.1fm",  ns / mo_unit),
    # ns > thr * w_unit  ~ sprintf("%.1fw",  ns / w_unit),
    ns > thr * d_unit  ~ sprintf("%.1fd",  ns / d_unit),
    ns > thr * h_unit  ~ sprintf("%.1fh",  ns / h_unit),
    ns > thr * m_unit  ~ sprintf("%.1fm",  ns / m_unit),
    ns > thr * s_unit  ~ sprintf("%.1fs",  ns / s_unit),
    ns > thr * ms_unit ~ sprintf("%.1fms", ns / ms_unit),
    ns > thr * us_unit ~ sprintf("%.1fus", ns / us_unit),
    TRUE               ~ sprintf("%.1fns", ns)
  )
  return(output_chr)
}


#' Generate an appropriate label
#'
#' @description
#' Numeric value is converted to an appropriate label.
#' e.g., 1e9 will be converted to \code{"1.0G"}
#' @importFrom dplyr case_when
#' @param x Numeric value(s).
#' @return Character.
#'
numeric_to_label <- function(x) {
  thr <- 0.1
  e3_unit <- 1e3
  e6_unit <- 1e6
  e9_unit <- 1e9
  e12_unit <- 1e12

  output_chr <- dplyr::case_when(
    x > thr * e12_unit ~ sprintf("%.1fT",  x / e12_unit),
    x > thr * e9_unit  ~ sprintf("%.1fG",  x / e9_unit),
    x > thr * e6_unit  ~ sprintf("%.1fM",  x / e6_unit),
    x > thr * e3_unit  ~ sprintf("%.1fK",  x / e3_unit),
    TRUE               ~ sprintf("%.1f",   x)
  )
  return(output_chr)
}


generate_matrix <- function(x, n_out, remove_first_last = TRUE) {

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

  return(matrix(x[idx], nrow = n_out, byrow = TRUE))
}
