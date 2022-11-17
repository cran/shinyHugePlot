
# shinyHugePlot

[![CRAN
status](https://www.r-pkg.org/badges/version/shinyHugePlot)](https://CRAN.R-project.org/package=shinyHugePlot)

The goal of `shinyHugePlot` is to efficiently visualize the data with a
large sample size, such as long time-series data. Using this package, a
small number of samples are obtained automatically from large-sized
data. Moreover, it can interactively change the samples according to the
data range defined by the user using `plotly` interfaces.

For instance, assume that there is a data with a sample size of 1e8.

Without this package, many charts are required: one for illustrating the
overall trend and ones for illustrating small parts of the data. To
display the overall trend is necessary; however, it requires a large
amount of time. It may be difficult to correctly illustrate the entire
data because of the graphical resolution. Dividing the data into
intervals and calculating statistical values, such as mean values, may
be a good approach for avoiding such problems; however, it also requires
a great amount of effort and slight fluctuations in the data will be
lost. It is frequently necessary to extract a specific part of data to
study slight fluctuations; however, it also requires a great amount of
effort.

Using this package, data with a large sample size are visualized easily,
quickly, and without errors. Small number of samples are automatically
obtained on a basis of specific algorithms, which helps in understanding
the overall trend of the data. Zooming up the data (using `plotly`
interfaces) automatically provides new samples and slight fluctuations
in the data are displayed. Both the overall and slight fluctuations of
the data can be accurately understood.

## Installation

You can install shinyHugePlot from CRAN like so:

``` r
install.packages("shinyHugePlot")
```

Or you can install the developing version of the package from gitlab
like so:

``` r
install.packages("remotes")
remotes::install_gitlab("jtagusari/shinyHugePlot")
```

## Example

### Data

Before showing the example, the data with large sample size is prepared
as follows:

``` r
library(tidyverse)
library(nanotime)

d <- tibble::tibble(
    x = seq(0, 1e6),
    t = nanotime::nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
    y1 = rnorm(1) * 1000 + (runif(1) * sin(x / 200) + sin(runif(1) * x / 200) + runif(1e6 + 1) / 10) * x / 1e3,
    y2 = rnorm(1) * 1000 + (runif(1) * sin(x / 200) + sin(runif(1) * x / 200) + runif(1e6 + 1) / 10) * x / 1e3,
    y3 = rnorm(1) * 1000 + (runif(1) * sin(x / 200) + sin(runif(1) * x / 200) + runif(1e6 + 1) / 10) * x / 1e3,
    y4 = rnorm(1) * 1000 + (runif(1) * sin(x / 200) + sin(runif(1) * x / 200) + runif(1e6 + 1) / 10) * x / 1e3,
    y5 = rnorm(1) * 1000 + (runif(1) * sin(x / 200) + sin(runif(1) * x / 200) + runif(1e6 + 1) / 10) * x / 1e3
  )

d_long <- tidyr::pivot_longer(d, cols = -c(x, t))
```

### Easy example

Here is the easiest example.

``` r
library(shinyHugePlot)
shiny_hugeplot(d$x, d$y1)
```

Time-series data can also be employed.

``` r
shiny_hugeplot(d$t, d$y1)
```

A shiny app will be running in the R-studio viewer. The original data
has 1e6 samples while only 1e3 samples (default) are shown to reduce the
processing time. Try zooming up the plot and confirm that more samples
are shown according to the zoom.

Note that how much the samples are down-sampled are shown in the
legends. For example, the legend name of `[R] trace 1 ~1.0K` means that
the name of the displayed series is “trace 1” and that one sample is
generated for every 1.0K (= 1000) samples using a particular
down-sampling algorithm.

### Plotly example

The plot is based on `plotly`, so creating a plotly object and using it
is a good option to use this package:

``` r
library(plotly)
fig <- plot_ly() %>%
  add_trace(x = d$t, y = d$y1, type = "scatter", mode = "lines") %>% 
  layout(xaxis = list(type = "date"))

shiny_hugeplot(fig)
```

It is noted that building plotly data indicates high computation cost.
For instance, try an example below (in the author’s environment, it
takes approx 30 secs):

``` r
system.time({
  fig <- plot_ly(
    x = d_long$t, y = d_long$value, name = d_long$name,
    type = "scatter", mode = "lines"
    ) %>% 
    layout(xaxis = list(type = "date"))
  
  fig_b <- plotly::plotly_build(fig)
  })
```

it is improved by the following function (in the author’s environment,
it takes approx 0.3 secs):

``` r
system.time({
  fig <- plot_ly(
    x = d_long$t, y = d_long$value, name = d_long$name,
    type = "scatter", mode = "lines"
    ) %>% 
    layout(xaxis = list(type = "date"))
  fig_b <- plotly_build_light(fig)
  })
```

Or formula can be employed,

``` r
system.time({
  fig_ev <- plot_ly(
    x = ~t, y = ~value, name = ~name, data = d_long,
    type = "scatter", mode = "lines"
    ) %>% 
    layout(xaxis = list(type = "date"))
  fig_ev_b <- plotly_build_light(fig)
  })
```

Then, run shiny app with the built plotly figure.

``` r
shiny_hugeplot(fig_b)
```

In addition to a normal plot, subplot is also supported. Note that
applying `plotly_build_light` before subplot reduces time for
computation.

``` r
d1 <- dplyr::filter(d_long, name %in% c("y1", "y2"))
d2 <- dplyr::filter(d_long, name %in% c("y3", "y4", "y5"))

fig_1 <- plot_ly(
  x = ~x, y = ~value, name = ~name, data = d1,
  type = "scatter", mode = "lines"
  ) 

fig_2 <- plot_ly(
  x = ~x, y = ~value, name = ~name, data = d2,
  type = "scatter", mode = "lines"
  ) 

fig_merged <- subplot(
  plotly_build_light(fig_1),
  plotly_build_light(fig_2),
  nrows = 2, shareX = TRUE
)

shiny_hugeplot(fig_merged)
```

### Shiny example

The layout of the plot(s) is controlled by `shiny`. You can customize
the layout.

``` r
fig <- plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines") 

ds <- downsampler$new(figure = fig)

ui <- fluidPage(
  plotlyOutput(outputId = "hp", width = "800px", height = "600px")
)

server <- function(input, output, session) {
  output$hp <- renderPlotly(ds$figure)
  observeEvent(plotly::event_data("plotly_relayout"),{
    updatePlotlyH(session, "hp", plotly::event_data("plotly_relayout"), ds)
  })
}

shinyApp(ui = ui, server = server)
```

### Example for changing the aggregator

The down-sampling is, by default, done with the local minimum and
maximum values (see `min_max_aggregator`). You can select several
another down-sample method with the user interface given by
`shiny_hugeplot`, and moreover, can explicitly select the one as
follows:

``` r
fig <- plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines") 
shiny_hugeplot(fig, n_out = 100, aggregator = range_stat_aggregator)
```

## LICENSE

This package is distributed with the MIT license.

## ACKNOWLEDGMENT

Development of this package was inspired by the python package of
`plotly_resampler`
(<https://github.com/predict-idlab/plotly-resampler>).
