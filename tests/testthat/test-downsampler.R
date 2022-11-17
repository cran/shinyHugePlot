d <- tibble::tibble(
  x = seq(0, 1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000,
  y2 = (2 - sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)

test_that("Check constructing plotly-datahandler", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines")

  pd <- plotly_datahandler$new(base_figure)

  expect_is(pd$figure, "plotly")
  expect_is(pd$orig_data, "data.table")

  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines")

  pd <- plotly_datahandler$new(base_figure)

  expect_is(pd$figure, "plotly")
  expect_is(pd$orig_data, "data.table")


})


test_that("Check updating plotly-datahandler", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines")

  pd <- plotly_datahandler$new(base_figure)

  pd$set_trace_data(x = d$x, y = d$y2, type = "scatter", mode = "lines", append = TRUE)

  expect_is(pd$figure, "plotly")
  expect_is(pd$orig_data, "data.table")

})


test_that("Check constructing downsampler", {
  base_figure <- plotly::plot_ly(data = d) %>%
    add_trace(x = ~x, y = ~y, type = "scatter", mode = "lines")
  ds <- downsampler$new(base_figure, n_out = 500)

  expect_is(ds$figure, "plotly")
  expect_is(ds$orig_data, "data.table")
  expect_is(ds$downsample_options, "data.frame")

  expect_equal(ds$figure$x$data, ds$update_trace(reset=TRUE, send_trace = TRUE)$new_trace)

})

test_that("Check constructing downsampler with nanotime", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines")
  ds <- downsampler$new(base_figure, n_out = 500)

  expect_is(ds$figure, "plotly")
  #ds$figure

})


test_that("Check constructing downsampler with POSIX time", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$tp, y = d$y, type = "scatter", mode = "lines") %>%
    layout(xaxis = list(type = "date"))
  ds <- downsampler$new(base_figure, n_out = 500)

  expect_is(ds$figure, "plotly")
})


test_that(
  "Check constructing downsampler with character datetime", {
  base_figure <- plotly::plot_ly() %>%
    add_trace(x = d$tch, y = d$y, type = "scatter", mode = "lines") %>%
    layout(xaxis = list(type = "date"))
  ds <- downsampler$new(base_figure, n_out = 500)

  expect_is(ds$figure, "plotly")

})


test_that(
  "Check constructing downsampler with x, y and y2", {
    base_figure <- plotly::plot_ly() %>%
      add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
      add_trace(x = d$x, y = d$y2, type = "scatter", mode = "lines")
    ds <- downsampler$new(base_figure, n_out = 500)

    expect_is(ds$figure, "plotly")

  })


test_that(
  "Check adding a trace to an downsampler", {
    base_figure <- plotly::plot_ly() %>%
      add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines")
    ds <- downsampler$new(base_figure, n_out = 500)
    ds$add_trace(x = d$x, y = d$y2, type = "scatter", mode = "lines", name = "new", color = "orange")

    ds$update_trace(reset = TRUE)
    # ds$figure
    expect_is(ds$figure, "plotly")

  })
