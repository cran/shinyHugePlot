
# shinyHugePlot

[![CRAN
status](https://www.r-pkg.org/badges/version/shinyHugePlot)](https://CRAN.R-project.org/package=shinyHugePlot)

The goal of shinyHugePlot is to efficiently plot the data of which
sample size is very large, such as long time-series data. Using this
package, small number of samples are obtained automatically from
huge-sized data. Moreover, it can interactively change the samples
according to the plot range which is defined by the user.

For instance, assume that there is a data of which sample size is 1e8.

Without this package, many charts are required: one for illustrating the
overall data and ones for illustrating small parts of the data. To plot
the overall data is necessary; however, it takes much time due to
computational cost. It may be difficult to correctly illustrate the data
because of the graphical resolution. Dividing the data into intervals
and calculate statistical values such as mean may be a good approach for
avoiding the problem due to the graphical resolution and for
understanding the trend of the data; however, it requires repeated trial
and error and the small fluctuations of the data will be lost. It is
frequently necessary to extract a specific part of the data to study the
small fluctuations; however, it also requires repeated trial and error.

With this package, necessary charts are obtained easily and quickly.
Small number of samples are automatically obtained on a basis of
specific algorithms, which helps understand the overall trend of the
data. Zooming up the data provides new samples and illustrates the small
fluctuations of the data. Both the overall and small fluctuations of the
data can be easily and accurately understood.

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
  t = nanotime(Sys.time()) + seq(0, 1e6) * 7e4,
  y = (3 + sin(x / 200) + runif(1e6 + 1) / 10) * x / 1000
)
```

### Easy example

Here is the easiest example.

``` r
library(shinyHugePlot)
shiny_hugeplot(d$x, d$y)
```

Time-series data is also applicable.

``` r
shiny_hugeplot(d$t, d$y)
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
  add_trace(x = d$t, y = d$y, type = "scatter", mode = "lines") %>% 
  layout(xaxis = list(type = "date"))

shiny_hugeplot(fig)
```

The subplot is also supported:

``` r
p1 <- plot_ly(
  data = d[1:1e5, ], x = ~x, y = ~y, type = "scatter", mode = "lines"
)

p2 <- plot_ly(
  data = d[1e3 + 1:1e5, ], x = ~x, y = ~y, type = "scatter", mode = "lines"
)

ps <- subplot(p1, p2, nrows = 2, shareX = TRUE)
shiny_hugeplot(ps)
```

### Shiny example

The layout of the plot(s) is controlled by `shiny`. You can customize
the layout using it.

``` r
fig <- plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines") 

shd <- shiny_downsampler$new(figure = fig)

ui <- fluidPage(
  plotlyOutput(outputId = "hp", width = "800px", height = "600px")
)

server <- function(input, output, session) {

  output$hp <- renderPlotly(shd$figure)

  observeEvent(plotly::event_data("plotly_relayout"),{
    updatePlotlyH(session, "hp", plotly::event_data("plotly_relayout"), shd)
  })

}

shinyApp(ui = ui, server = server)
```

### Example for changing the aggregator

The down-sampling is, by default, done using Largest Triangle Three
Buckets (LTTB) algorithm, which decreases the samples based on the
visual aspect of the plot. You can select several another down-sample
method, such as just showing minimum, mean and max values, as follows.

``` r
fig <- plot_ly(x = d$x, y = d$y, type = "scatter", mode = "lines") 
shiny_hugeplot(fig, n_out = 100, aggregator = range_stat_aggregator)
```

## NOTE

This package was originally developed for displaying the data using
`dash` app. See `plotlyHugeData` at
<https://gitlab.com/jtagusari/plotlyHugeData> if you would like to use
`dash` app.

## LICENSE

This package is distributed with the MIT license.

## ACKNOWLEDGMENT

This package was developed inspired by the python package of
`plotly_resampler`
(<https://github.com/predict-idlab/plotly-resampler>).
