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
#'     x = noise_fluct$t,
#'     y = noise_fluct$level,
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
  assertthat::assert_that(
    all(purrr::map_lgl(fig$x$attrs, ~"type" %in% names(.x)))
  )

  # convert to tibble
  traces_df <- purrr::imap_dfr(
    fig$x$attrs,
    ~purrr::modify_if(.x, ~length(.x) > 1, list) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(uid = .y)
  )

  # evaluate the data, if necessary
  traces_df_eval <- traces_df %>%
    dplyr::filter(!is.na(.data$type)) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::vars_select_helpers$where(
          ~any(purrr::map_lgl(.x, lazyeval::is_formula))
        ),
        ~purrr::map2(
          .x, uid,
          ~lazyeval::f_eval(.x, plotly::plotly_data(fig, .y))
        )
      )
    )

  # divide high frequency variables and other attributes
  traces_df_div <- purrr::pmap(
    traces_df_eval,
    function(...) {
      args <- list(...)
      args_length <- purrr::map_int(args, length)
      hf_data_raw <- NULL # for binding a variable
      hf_data <- NULL # for binding a variable

      trace_df <- bind_cols(
        args[args_length < args_length["x"]] %>%
          purrr::modify_if(~length(.x) > 1, list) %>%
          tibble::as_tibble(),
        tibble::as_tibble(args[args_length == args_length["x"]]) %>%
          tidyr::nest(
            hf_data_raw = tidyselect::matches(paste0("^", vars_hf, "$"))
          ) %>%
          dplyr::mutate(
            hf_data_raw = purrr::map(
              .data$hf_data_raw,
              ~dplyr::summarise(.x, across(.fns = list))
            )
          ) %>%
          tidyr::unnest(hf_data_raw)
      ) %>%
        tidyr::nest(
          hf_data = tidyselect::matches(paste0("^", vars_hf, "$"))
        )

      return(trace_df)
    }
  ) %>%
    dplyr::bind_rows()

  # obtain attributes of high frequency variables
  hf_attrs <- traces_df_div$hf_data %>%
    purrr::map(~purrr::map(.x, ~.x[[1]])) %>%
    purrr::map(~purrr::discard(.x, ~all(is.na(.x))))

  # also obtaine other attributees
  other_attrs <- traces_df_div %>%
    dplyr::select(-matches("^hf_data$")) %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::map(~purrr::discard(.x, ~all(is.na(.x))))

  # replace attributes with the ones without high frequency data
  # then build it
  fig$x$attrs <- other_attrs
  fig_built <- plotly::plotly_build(fig)

  # delete attributes
  fig$x$attrs <- NULL

  # directly input the high frequency data to the plotly data
  fig_built$x$data <- purrr::map2(
    fig_built$x$data, hf_attrs,
    ~c(.x, .y)
  )

  return(fig_built)
}
