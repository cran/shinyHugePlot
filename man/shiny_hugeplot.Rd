% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_hugeplot.R
\name{shiny_hugeplot}
\alias{shiny_hugeplot}
\alias{shiny_hugeplot.default}
\alias{shiny_hugeplot.formula}
\alias{shiny_hugeplot.character}
\alias{shiny_hugeplot.matrix}
\alias{shiny_hugeplot.data.frame}
\alias{shiny_hugeplot.plotly}
\alias{shiny_hugeplot.downsampler}
\title{Wrapper for plotting large-sized data using \code{shinyHugePlot}}
\usage{
shiny_hugeplot(obj, ...)

\method{shiny_hugeplot}{default}(
  obj = NULL,
  y = NULL,
  tz = Sys.timezone(),
  use_light_build = TRUE,
  plotly_options = list(type = "scatter", mode = "lines"),
  plotly_layout_options = list(),
  aggregator = min_max_aggregator$new(),
  n_out = 1000L,
  run_shiny = TRUE,
  downsampler_options = list(),
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)

\method{shiny_hugeplot}{formula}(
  obj = NULL,
  srcs = NULL,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  use_light_build = TRUE,
  fread_options = list(),
  downsampler_options = list(),
  plotly_options = list(type = "scatter", mode = "lines"),
  plotly_layout_options = list(),
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)

\method{shiny_hugeplot}{character}(
  obj = NULL,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  use_light_build = TRUE,
  fread_options = list(),
  downsampler_options = list(),
  plotly_options = list(type = "scatter", mode = "lines"),
  plotly_layout_options = list(),
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)

\method{shiny_hugeplot}{matrix}(
  obj = NULL,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  use_light_build = TRUE,
  downsampler_options = list(),
  plotly_options = list(type = "scatter", mode = "lines"),
  plotly_layout_options = list(),
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)

\method{shiny_hugeplot}{data.frame}(
  obj = NULL,
  tz = Sys.timezone(),
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  use_light_build = TRUE,
  downsampler_options = list(),
  plotly_options = list(type = "scatter", mode = "lines"),
  plotly_layout_options = list(),
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)

\method{shiny_hugeplot}{plotly}(
  obj,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  use_light_build = TRUE,
  downsampler_options = list(),
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)

\method{shiny_hugeplot}{downsampler}(
  obj,
  run_shiny = TRUE,
  shiny_options = list(),
  width = "100\%",
  height = "600px",
  verbose = FALSE,
  ...
)
}
\arguments{
\item{obj}{Numeric/\code{nanotime}/\code{POSIXt} vector,
numeric matrix, data.frame, single character string,
\code{plotly} object, or \code{downsampler} object.
If a numeric vector is given, it will be used as y values of the figure
of the \code{shiny} application
(the x values are calculated by \code{seq_along(obj)}).
It will be interpreted as the x values if you use \code{y} argument together.
If a \code{nanotime} (see \code{nanotime} package) vector is given,
it will be used as the x values (\code{y} argument is mandatory).
If a numeric matrix is given, which must have more than 2 columns,
the first and second column values will be used as the x and y values.
If a data frame is given,
which must have columns named \code{x} and \code{y},
these columns will be used as the x and y values.
If a single character string is given, it will be used as a file path
to obtain a data frame
(data frame will be loaded using \code{data.table::fread}).
If a \code{plotly} object is given, the data and layout of it will be used
for constructing the figure of the \code{shiny} application.
If a \code{downsampler} object is given, the data, layout, and
down-sampling options for aggregating original data of it will be used for
constructing \code{shiny} application.
If a \code{formula} is given, the data will be saved and loaded using duck-db.
The sources of the data (\code{srcs}) must be given as a character string}

\item{...}{Not used.}

\item{y}{Numeric vector, optional.
y values of the figure of \code{shiny} application,
which is required if the \code{obj} argument is used as the x values.}

\item{tz}{Timezone, optional.
It is used to convert the \code{nanotime} to the time displayed in the figure.
By default, \code{Sys.timezone()}.}

\item{use_light_build}{Boolean, optional.
Whether \code{shinyHugePlot::plotly_build_light} will be used.
(if \code{FALSE}, \code{plotly::plotly_build} will be used)
By default, \code{TRUE}.}

\item{plotly_options}{Named list, optional.
Arguments passed to \code{plotly::plot_ly}.}

\item{plotly_layout_options}{Named list, optional.
Arguments passed to \code{plotly::layout}.}

\item{aggregator}{Instance of R6 classes for aggregating data, optional.
The classes can be listed using \code{list_aggregators}.
By default, \code{min_max_aggregator$new()}.}

\item{n_out}{Integer, optional.
Number of samples get by the down-sampling. By default, 1000.}

\item{run_shiny}{Boolean, optional.
whether a generated \code{shiny} application will be launched.
By default, \code{TRUE}.}

\item{downsampler_options}{Named list, optional.
Arguments passed to \code{downsampler$new}.
Note that use \code{aggregator} and \code{n_out} arguments
if you want to set these arguments.}

\item{shiny_options}{Named list, optional.
Arguments passed to \code{shinyApp} function.}

\item{width, height}{Character, optional.
Arguments passed to \code{plotlyOutput}.
By default, \code{100\%} and \code{600px}.}

\item{verbose}{Boolean.
Whether detailed messages to check the procedures are shown. By default, \code{FALSE}.}

\item{srcs}{Character, used when a \code{formula} is given as the \code{obj}.}

\item{fread_options}{Named list, optional.
Arguments passed to \code{data.table::fread},
which is used if a single character string is given as the \code{obj}.}
}
\description{
This is a S3 class function to easily plot large-sized data using
\code{downsampler} object including \code{plotly} and
\code{shiny} application.
Using data that is given as a first argument,
\code{shiny} application will be constructed and (by default,) executed.
As the first argument, many classes are applicable,
ranging from a numeric vector representing y values
to a \code{downsampler} object containing
original data, layout of the figure and
options for aggregating the original data.
}
\examples{
\donttest{
data(noise_fluct)

shiny_hugeplot(noise_fluct$f500)
shiny_hugeplot(noise_fluct$time, noise_fluct$f500)
}
}
