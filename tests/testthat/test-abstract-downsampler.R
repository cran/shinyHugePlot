d <- tibble::tibble(
  x = seq(0, 1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000,
  y2 = (2 - sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)

test_that("Check constructing abstract-downsampler (x and y are given)", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines")
  bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)

  expect_is(bf_agg$figure, "plotly")
  expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)

})


test_that("Check constructing abstract-downsampler (data is given)", {
  base_figure <- plotly::plot_ly(data = d) %>%
    add_trace(x = ~x, y = ~y, type = "scatter", mode = "lines")
  bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)

  expect_is(bf_agg$figure, "plotly")
  expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)

})

test_that("Check constructing abstract-downsampler
          (nanotime t and y are given)", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines")
  bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)

  expect_is(bf_agg$figure, "plotly")
  expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)

})


test_that("Check constructing abstract-downsampler (POSIX t and y are given)", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$tp, y = d$y, type = "scatter", mode = "lines")
  bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)

  expect_is(bf_agg$figure, "plotly")
  expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)

})


test_that(
  "Check constructing abstract-downsampler
  (character datetime t and y are given)", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$tch, y = d$y, type = "scatter", mode = "lines")
  bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)

  expect_is(bf_agg$figure, "plotly")
  expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)

})


test_that(
  "Check constructing abstract-downsampler
  (x, y and y2 are given)", {
    base_figure <- plotly::plot_ly() %>%
      add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
      add_trace(x = d$x, y = d$y2, type = "scatter", mode = "lines")
    bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)

    expect_is(bf_agg$figure, "plotly")
    expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)
    expect_equal(length(bf_agg$figure$x$data[[1]]$y), 500)
    expect_equal(length(bf_agg$figure$x$data[[2]]$y), 500)

  })


test_that(
  "Check adding a trace to an abstract-downsampler", {
    base_figure <- plotly::plot_ly() %>%
      add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines")
    bf_agg <- abstract_downsampler$new(base_figure, n_out = 500)
    bf_agg$add_trace(x = d$x, y = d$y2, type = "scatter", mode = "lines")

    expect_is(bf_agg$figure, "plotly")
    expect_equal(length(bf_agg$figure$x$data[[1]]$x), 500)
    expect_equal(length(bf_agg$figure$x$data[[1]]$y), 500)
    expect_equal(length(bf_agg$figure$x$data[[2]]$y), 500)

  })
