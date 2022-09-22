# Note that settings of `trace updater` should be changed
# if you test the package using `pkgload::load_all`

d <- tibble::tibble(
  x = seq(0, 1e8),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)

# data downsampler

shiny_hugeplot(d$y)
shiny_hugeplot(d)
shiny_hugeplot(d, aggregator = range_stat_aggregator$new(), n_out = 100)
shiny_hugeplot(d, aggregator = range_stat_aggregator$new(y = NULL), n_out = 100)

# plotly downsampler

plotly::plot_ly() %>%
  add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
  shiny_hugeplot()


plotly::plot_ly() %>%
  add_trace(x = d$x, y = d$y, type = "scatter", mode = "lines") %>%
  shiny_hugeplot(n_out = 100, aggregator = range_stat_aggregator)

plotly::plot_ly() %>%
  add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines") %>%
  layout(xaxis = list(type = "date")) %>%
  shiny_hugeplot()

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

shiny_hugeplot(ps, n_out = 100)


# subplot downsampler (x axis is shared)

ps <- subplot(p1, p2, p3, p4, nrows = 2, shareX = TRUE) %>%
  shiny_hugeplot()

# subplot downsampler (x axis is shared using `matches`)

ps <- subplot(p1, p2, p3, p4, nrows = 2) %>%
  layout(
    xaxis = list(matches = "x4")
  ) %>%
  shiny_hugeplot()


