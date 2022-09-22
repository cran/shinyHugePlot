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

app <- shiny_downsampler$new()
app$add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines")
app$show_shiny()


app <- shiny_downsampler$new()
app$add_trace(
  x = d$x, y = d$y, type = "scatter", mode = "lines",
  aggregator = range_stat_aggregator$new(y = NULL), n_out = 100
)
app$add_trace(
  x = d$x, y = d$y, type = "scatter", mode = "lines",
  aggregator = min_max_aggregator$new(), n_out = 100
  )
app$show_shiny()
