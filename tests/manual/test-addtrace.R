# Note that settings of `trace updater` should be changed
# if you test the package using `pkgload::load_all`

d <- tibble::tibble(
  x = seq(0, 1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)

# plotly downsampler
fig <- plotly::plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines")
ds <- downsampler$new()

ds$figure

ds$add_trace(
  x = d$x[1:10000], y = d$y[1:10000],
  type = "scatter", mode = "lines",
  name = "trace new", color = I("orange")
  )

ds$add_trace(
  x = d$x[10001:20000], y = d$y[10001:20000],
  type = "scatter", mode = "lines",
  name = "trace new2", color = I("red"),
  aggregator = range_stat_aggregator$new(), n_out = 100
  )

ds$figure
ds$update_trace(reset = TRUE)

ds$figure

