# sample data
d <- tibble::tibble(
  x = seq(0, 1e5),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e5) * 7e4,
  tp = Sys.time() + seq(0, 1e5) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e5 + 1) / 10) * x / 1000
)

# sample data with irregular gaps
d_sep <- d[c(1, 5e3:1e4, 3e4:1e5),]

# shiny app generation using different argument
shiny_hugeplot(d$y)
shiny_hugeplot(d)
shiny_hugeplot(plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines"))
shiny_hugeplot(plot_ly(x = d$t, y = d$y, type = "scatter", mode = "lines")) # datetime layout will be automatically selected
shiny_hugeplot(plot_ly(x = d$tp, y = d$y, type = "scatter", mode = "lines")) # datetime layout will be automatically selected
shiny_hugeplot(plot_ly(x = d$tch, y = d$y, type = "scatter", mode = "lines")) # datetime layout will be automatically selected
shiny_hugeplot(
  downsampler$new(plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines"))
  )

# shiny app generation using different down-sampler
shiny_hugeplot(d$y, aggregator = min_max_aggregator$new())
shiny_hugeplot(d$y, aggregator = min_max_ovlp_aggregator$new())
shiny_hugeplot(d$y, aggregator = LTTB_aggregator$new())
shiny_hugeplot(d$y, aggregator = eLTTB_aggregator$new())
shiny_hugeplot(d$y, aggregator = nth_pnt_aggregator$new())
shiny_hugeplot(d$y, aggregator = range_stat_aggregator$new())
shiny_hugeplot(d$y[1:1e4], aggregator = null_aggregator$new())

# shiny app generation with irregular gaps
agg <- min_max_aggregator$new(interleave_gaps = T)
d <- agg$aggregate(x = d_sep$x, y = d_sep$y, n_out = 1000)
shiny_hugeplot(d_sep, aggregator = agg)

# shiny app generation with subplots
p1 <- plotly::plot_ly(
  data = d[1:5e4, ], x = ~x, y = ~y,
  name = "only this is named", type = "scatter", mode = "lines"
) %>%
  plotly_build_light()

p2 <- plotly::plot_ly(
  data = d[1e4 + 1:5e4, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)%>%
  plotly_build_light()

p3 <- plotly::plot_ly(
  data = d[2e4 + 1:5e4, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)%>%
  plotly_build_light()

p4 <- plotly::plot_ly(
  data = d[3e4 + 1:5e4, ], x = ~x, y = ~y,
  type = "scatter", mode = "lines"
)%>%
  plotly_build_light()

ps1 <- subplot(p1, p2, p3, p4, nrows = 2)
ps2 <- subplot(p1, p2, p3, p4, nrows = 2, shareX = TRUE)
ps3 <- subplot(p1, p2, p3, p4, nrows = 2) %>%
  layout(xaxis = list(matches = "x4"))

shiny_hugeplot(ps1)
shiny_hugeplot(ps2)
shiny_hugeplot(ps3)
