#' Build plotly data with low computation cost
#'
#' @importFrom dplyr %>% mutate filter across summarise bind_cols across
#' @importFrom assertthat assert_that
#' @importFrom purrr imap_dfr modify_if map2 pmap map_int map discard transpose
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest unnest
#' @importFrom tidyselect vars_select_helpers matches
#' @importFrom rlang .data
#' @param fig \code{plotly} object.
#' Note that \code{fig$x$attrs} is not \code{NULL} and
#' each \code{fig$x$attrs} element has an element named \code{x}.
#' This function generates \code{fig$x$data} using \code{fig$x$attrs}.
#' @param vars_hf Character, optional.
#' Variable names where high frequency data is included.
#' It must include \code{x}.
#' @return built \code{plotly} object
#' @export
#' @description
#' Before illustrating data using \code{plotly}, it must be built
#' (\code{figure$x$data} are need to be made using \code{figure$x$attrs}).
#' However, because a lot of procedures are necessary,
#' the computation cost is relatively high.
#' With this function, the data is built in quite short time by omitting
#' several procedures for high-frequency data.
#' Note that this function is not universally applicable to all \code{plotly}
#' objects but made for high-frequency scatter data.
#' \code{plotly::plotly_build} function may return better results in
#' specific cases although it takes more time.
#' @examples
#' data(noise_fluct)
#' plotly_build_light(
#'   plotly::plot_ly(
#'     x = noise_fluct$time,
#'     y = noise_fluct$f500,
#'     name = "level",
#'     type = "scatter"
#'   )
#' )
#'
#' plotly_build_light(
#'   plotly::plot_ly(
#'     data = noise_fluct,
#'     x = ~time,
#'     y = ~f500,
#'     name = "level",
#'     type = "scatter"
#'   )
#' )
#'
plotly_build_light <- function(
    fig, vars_hf = c("x", "y", "text", "hovertext")
    ) {

  # check_arguments
  assertthat::assert_that(inherits(fig, "plotly"))
  assertthat::assert_that(inherits(fig$x$attrs, "list"))
  assertthat::assert_that(!is.null(names(fig$x$attrs)))

  # evaluate the trace, if necessary
  traces_div <- fig$x$attrs %>%
    purrr::discard(~is.null(.x$type) || is.na(.x$type)) %>%
    purrr::imodify(
      function(trace, uid) {
        trace_eval <- purrr::modify_if(
          trace,
          lazyeval::is_formula,
          ~lazyeval::f_eval(.x, plotly::plotly_data(fig, uid))
        )

        attrs_length <- purrr::map_int(trace_eval, length)
        vars_long   <- names(trace_eval[attrs_length == attrs_length["x"]])

        data_long <- trace_eval[vars_long] %>%
          data.table::setDT() %>%
          .[,lapply(.SD, list), by = setdiff(vars_long, vars_hf)]

        trace_data <- purrr::pmap(
          data_long,
          function(...) {
            c(trace[setdiff(names(trace), vars_long)], list(...))
          }
        )

        return(trace_data)
      }
    ) %>%
    unlist(recursive = FALSE)

  # replace attributes with the ones without high frequency data
  # then build it
  fig$x$attrs <- purrr::map(
    traces_div,
    ~.x[setdiff(names(.x), vars_hf)]
  )
  fig_built <- plotly::plotly_build(fig)

  # directly input the high frequency data to the plotly data
  fig_built$x$data <- purrr::map2(
    fig_built$x$data, traces_div,
    ~c(.x, .y[intersect(names(.y), vars_hf)])
  )

  return(fig_built)
}
