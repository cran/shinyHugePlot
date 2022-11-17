# Note that settings of `trace updater` should be changed
# if you test the package using `pkgload::load_all`

d <- tibble::tibble(
  x = seq(0, 1e6),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  tp = Sys.time() + seq(0, 1e6) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)


d_sep <- d[c(1, 50:1e4, 3e4:1e5, (2e5+3e4):(8e5), 9e5:1e6),]

# data downsampler

shiny_hugeplot(d$y)
shiny_hugeplot(d)
shiny_hugeplot(d_sep, aggregator = min_max_aggregator$new(interleave_gaps = T))
shiny_hugeplot(d, aggregator = range_stat_aggregator$new(), n_out = 100)
shiny_hugeplot(d, aggregator = range_stat_aggregator$new(y = NULL), n_out = 100)
shiny_hugeplot(d_sep, aggregator = range_stat_aggregator$new(interleave_gaps = T), n_out = 100)

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

fig <- plotly::plot_ly() %>%
  add_trace(x = d_sep$x, y = d_sep$y, type = "scatter", mode = "lines")

ds <- downsampler$new(fig, aggregator = range_stat_aggregator$new(interleave_gaps = T), n_out = 100)
shiny_hugeplot(ds)


# subplot downsampler

p1 <- plotly::plot_ly(
  data = d[1:1e5, ], x = ~x, y = ~y,
  name = "only this is named", type = "scatter", mode = "lines"
) %>%
  plotly_build_light()

p2 <- plotly::plot_ly(
  data = d[1e3 + 1:1e5, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)%>%
  plotly_build_light()

p3 <- plotly::plot_ly(
  data = d[2e3 + 1:1e5, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)%>%
  plotly_build_light()

p4 <- plotly::plot_ly(
  data = d[3e3 + 1:1e5, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)%>%
  plotly_build_light()

ps <- subplot(p1, p2, p3, p4, nrows = 2)

shiny_hugeplot(ps, n_out = 100)


ds <- downsampler$new(ps)
ds$orig_data
ds$downsample_options
ds$set_downsample_options(uid = ds$orig_data$uid[1], aggregator = range_stat_aggregator$new(), n_out = 100)
ds$update_trace(reset = T)
shiny_hugeplot(ds)

# subplot downsampler (x axis is shared)

ps <- subplot(p1, p2, p3, p4, nrows = 2, shareX = TRUE) %>%
  shiny_hugeplot()

# subplot downsampler (x axis is shared using `matches`)

ps <- subplot(p1, p2, p3, p4, nrows = 2) %>%
  layout(
    xaxis = list(matches = "x4")
  ) %>%
  shiny_hugeplot()


