# Note that settings of `trace updater` should be changed
# if you test the package using `pkgload::load_all`

d <- tibble::tibble(
  x = seq(0, 1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)

# data downsampler

apply_downsampler(d$y)
apply_downsampler(d)
apply_downsampler(d, n_out = 100, aggregator = min_max_aggregator)
apply_downsampler(d, n_out = 100, aggregator = range_stat_aggregator)

# plotly downsampler

plotly::plot_ly() %>%
  add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
  apply_downsampler()


plotly::plot_ly() %>%
  add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
  apply_downsampler(n_out = 100, aggregator = range_stat_aggregator)

plotly::plot_ly() %>%
  add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines") %>%
  layout(xaxis = list(type = "date")) %>%
  apply_downsampler()

# subplot downsampler

p1 <- plotly::plot_ly(
  data = d[1:1e5, ], x = ~x, y = ~y,
  name = "only this is named", type = "scatter", mode = "lines"
)

p2 <- plotly::plot_ly(
  data = d[1e3 + 1:1e5, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)

p3 <- plotly::plot_ly(
  data = d[2e3 + 1:1e5, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)

p4 <- plotly::plot_ly(
  data = d[3e3 + 1:1e5, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)

ps <- subplot(p1, p2, p3, p4, nrows = 2)

apply_downsampler(ps)


# subplot downsampler (x axis is shared)

ps <- subplot(p1, p2, p3, p4, nrows = 2, shareX = TRUE) %>%
  apply_downsampler()

# subplot downsampler (x axis is shared using `matches`)

ps <- subplot(p1, p2, p3, p4, nrows = 2) %>%
  layout(
    xaxis = list(matches = "x4")
  ) %>%
  apply_downsampler()




# dash downsampler
# app <- dash::dash_app() %>%
#   dash::set_layout(
#     dash::dccGraph(
#       id = "downsample-figure",
#       figure = plotly::plot_ly(
#         data = d[1:1e4, ], x = ~x, y = ~y, type = "scatter", mode = "lines"
#       )
#     ),
#     dash::dccGraph(
#       id = "other-figure",
#       figure = plotly::plot_ly(
#         data = d[1e2 + 1:1e2, ], x = ~x, y = ~y,
#         type = "scatter", mode = "lines"
#       )
#     )
#   ) %>%
#   apply_downsampler(graph_id = "downsample-figure")
#
# app$run_server(host = "127.0.0.1", port = "8050", use_viewer = TRUE)
