# sample data
d <- tibble::tibble(
  x = seq(0, 1e5),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e5) * 7e4,
  tp = Sys.time() + seq(0, 1e5) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e5 + 1) / 10) * x / 1000,
  y2 = (8 + sin(x / 200) + runif(1e5 + 1) / 10) * x / (-5000)
)

fig <- plotly::plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines")


test_that("Check plotly_build_lght", {

  fig_b <- plotly_build_light(fig)

  expect_is(fig_b, "plotly")
  expect_is(fig_b$x$data, "list")
  expect_equal(length(fig_b$x$data[[1]]$x), nrow(d))

})


test_that("Check plotly-datahandler", {

  pd <- plotly_datahandler$new(fig)

  expect_is(pd$figure, "plotly")
  expect_is(pd$orig_data, "data.table")
  expect_equal(nrow(pd$orig_data$data[[1]]), nrow(d))

  traces_df <- pd$plotly_data_to_df(plotly_build_light(fig)$x$data)
  expect_is(traces_df, "data.table")
  expect_equal(nrow(traces_df$data[[1]]), nrow(d))

  pd$set_trace_data(x = d$x, y = d$y2, type = "scatter", mode = "lines", append = TRUE)
  expect_equal(nrow(pd$orig_data), 2)
  expect_equal(nrow(pd$orig_data$data[[2]]), nrow(d))
})


test_that("Check constructing downsampler", {
  ds <- downsampler$new(fig, n_out = 500)
  expect_is(ds$figure, "plotly")
  expect_equal(length(ds$figure$x$data), 1)
  expect_equal(length(ds$figure$x$data[[1]]$x), 500)

  ds$add_trace(x = d$x, y = d$y2, type = "scatter", mode = "lines")
  expect_equal(length(ds$figure$x$data), 1)

  ds$update_trace(reset = TRUE)
  expect_equal(length(ds$figure$x$data), 2)

  ds$set_downsample_options(uid = ds$orig_data$uid[2], n_out = 100)
  ds$update_trace(reset = TRUE)
  expect_equal(length(ds$figure$x$data[[2]]$x), 100)

})

test_that("Check constructing downsampler with nanotime", {
  fig_n <- plotly::plot_ly() %>%
    add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines")
  ds <- downsampler$new(fig_n, n_out = 500)

  expect_is(ds$figure, "plotly")

})


test_that("Check constructing downsampler with POSIX time", {
  fig_tp <- plotly::plot_ly() %>%
    add_trace(x = d$tp, y = d$y, type = "scatter", mode = "lines") %>%
    layout(xaxis = list(type = "date"))
  ds <- downsampler$new(fig_tp, n_out = 500)

  expect_is(ds$figure, "plotly")
})


test_that(
  "Check constructing downsampler with character datetime", {
  fig_tc <- plotly::plot_ly() %>%
    add_trace(x = d$tch, y = d$y, type = "scatter", mode = "lines") %>%
    layout(xaxis = list(type = "date"))
  ds <- downsampler$new(fig_tc, n_out = 500)

  expect_is(ds$figure, "plotly")

})


test_that(
  "Check constructing downsampler with x, y and y2", {
    fig2 <- plotly::plot_ly() %>%
      add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
      add_trace(x = d$x, y = d$y2, type = "scatter", mode = "lines")
    ds <- downsampler$new(fig2, n_out = 500)

    expect_is(ds$figure, "plotly")

  })

