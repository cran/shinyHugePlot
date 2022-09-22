#' Wrapper for plotting large-sized data using shiny and plotly
#'
#' @description
#' This S3 class function is a wrapper to plot the large-sized data.
#' It employs an R6 \code{shiny_downsampler} instance to obtain data samples
#' using a specified aggregation method that is defined in the instance.
#' The figure will be updated interactively according to the x-range
#' that user select manually in the shiny app.
#' @param obj Numeric vector, nanotime vector, numeric matrix, data.frame,
#' or plotly object.
#' If a numeric vector is given, it is used as the y values of the figure.
#' the x values are calculated by \code{seq_along(obj)}.
#' If you use \code{y} argument together, this argument is interpreted as
#' the x values of the figure.
#' If a nanotime vector is given, it is used as the x values of the figure.
#' You must also give \code{y} argument, which will be used as the y values.
#' Regarding \code{nanotime}, see the \code{nanotime} package.
#' If a numeric matrix is given, the first and second column values are used
#' as the x and y values. (The matrix must have more than 2 columns.)
#' If a data.frame is given, \code{x} and \code{y} columns are used as the
#' x and y values. If the class of the \code{x} column is \code{nanotime},
#' the date type will be applied to the x axis.
#' The data.frame must have columns named x and y.
#' If a plotly object is given, it will be displayed.
#' @param y Numeric vector, optional.
#' The y values of the figure.
#' It is required if the \code{obj} argument is used as x values of the figure.
#' @param tz Timezone, optional.
#' It is used to convert the nanotime to the time displayed in the figure.
#' It is used if the class of the \code{obj} argument is nanotime.
#' By default, \code{Sys.timezone()}.
#' @param n_out Integer, optional.
#' Number of samples get by the down-sampling. By default, 1000.
#' @param aggregator R6 class for the aggregation, optional.
#' Select an aggregation function. The list of the functions are obtained
#' using \code{list_aggregators}.
#' By default, \code{min_max_aggregator$new()}.
#' @param downsampler_options Named list, optional.
#' Arguments passed to \code{shiny_downsampler$new}, other than
#' \code{aggregator} and \code{n_shown_samples}.
#' To set these arguments, use \code{aggregator} and \code{n_out} arguments.
#' @param plotly_options Named list, optional.
#' Arguments passed to \code{plotly::plot_ly}.
#' @param plotly_layout_options Named list, optional.
#' Arguments passed to \code{plotly::layout}.
#' @param shiny_options Named list, optional.
#' Arguments passed to \code{shinyApp} function.
#' @param width,height Character, optional.
#' Arguments passed to \code{plotlyOutput}.
#' By default, \code{100\%} and \code{600px}.
#' @param run_shiny Boolean, optional.
#' whether a generated shiny app will be run immediately.
#' By default, \code{TRUE}.
#' @param ... Not used.
#' @export
#' @examples
#' \donttest{
#' data(noise_fluct)
#'
#' shiny_hugeplot(noise_fluct$level)
#' shiny_hugeplot(
#'   noise_fluct$t, noise_fluct$level,
#'   plotly_layout_options = list(xaxis = list(type = "date"))
#' )
#' }
shiny_hugeplot <- function(obj, ...) {
  UseMethod("shiny_hugeplot", obj)
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.default <- function(
    obj = NULL, y = NULL,
    n_out = 1000L,
    aggregator = min_max_aggregator$new(),
    run_shiny = TRUE,
    downsampler_options = list(),
    plotly_options = list(type = "scatter", mode = "lines"),
    plotly_layout_options = list(),
    shiny_options = list(),
    width = "100%", height = "600px",
    ...
) {

  x <- obj

  if (is.null(plotly_options$type)) plotly_options$type <- "scatter"
  if (is.null(plotly_options$mode)) plotly_options$mode <- "lines"


  assertthat::assert_that(
    !is.null(x) || !is.null(y)
  )

  if (is.null(x)) x = seq_along(y)
  if (is.null(y)) {
    y = x
    x = seq_along(y)
  }

  fig <- do.call(
    plotly::plot_ly,
    c(
      list(x = x, y = y),
      plotly_options
    )
  )

  if (length(plotly_layout_options) > 0) {

    fig <- do.call(
      plotly::layout,
      c(
        list(fig),
        plotly_layout_options
      )
    )
  }

  shd <- shiny_hugeplot(
    fig,
    n_out = n_out,
    aggregator = aggregator,
    run_shiny = run_shiny,
    downsampler_options = downsampler_options,
    shiny_options = shiny_options,
    width = width, height = height,
  )

  invisible(shd)
}



#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.nanotime <- function(
    obj = NULL, y = NULL, tz = Sys.timezone(),
    n_out = 1000L,
    aggregator = min_max_aggregator$new(),
    run_shiny = TRUE,
    downsampler_options = list(),
    plotly_options = list(type = "scatter", mode = "lines"),
    plotly_layout_options = list(xaxis = list(type = "date")),
    shiny_options = list(),
    width = "100%", height = "600px",
    ...
) {

  t <- obj

  if (is.null(plotly_options$type)) plotly_options$type <- "scatter"
  if (is.null(plotly_options$mode)) plotly_options$mode <- "lines"
  if (is.null(plotly_layout_options$xaxis$type)) {
    plotly_layout_options$xaxis$type <- "date"
  }

  assertthat::assert_that(!is.null(y))
  x <- nanotime_to_plotlytime(t, tz)

  fig <- do.call(
    plotly::plot_ly,
    c(
      list(x = x, y = y),
      plotly_options
    )
  )

  if (length(plotly_layout_options) > 0) {

    fig <- do.call(
      plotly::layout,
      c(
        list(fig),
        plotly_layout_options
      )
    )
  }

  shd <- shiny_hugeplot(
    fig,
    n_out = n_out,
    aggregator = aggregator,
    run_shiny = run_shiny,
    downsampler_options = downsampler_options,
    shiny_options = shiny_options,
    width = width, height = height
  )

  invisible(shd)
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.POSIXct <- function(
    obj = NULL, y = NULL, tz = Sys.timezone(),
    n_out = 1000L,
    aggregator = min_max_aggregator$new(),
    run_shiny = TRUE,
    downsampler_options = list(),
    plotly_options = list(type = "scatter", mode = "lines"),
    plotly_layout_options = list(xaxis = list(type = "date")),
    shiny_options = list(),
    width = "100%", height = "600px",
    ...
) {

  obj = nanotime::as.nanotime(obj)

  shd <- shiny_hugeplot(
    obj = obj, y = y, tz = tz,
    n_out = n_out,
    aggregator = aggregator,
    run_shiny = run_shiny,
    downsampler_options = downsampler_options,
    plotly_options = plotly_options,
    plotly_layout_options = plotly_layout_options,
    shiny_options = shiny_options,
    width = width, height = height
  )

  invisible(shd)
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.matrix <- function(
  obj = NULL,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  downsampler_options = list(),
  plotly_options = list(type = "scatter", mode = "lines"),
  plotly_layout_options = list(),
  shiny_options = list(),
  width = "100%", height = "600px",
  ...
  ) {

  mat <- obj

  shd <- shiny_hugeplot(
    mat[,1], mat[,2],
    n_out = n_out,
    aggregator = aggregator,
    run_shiny = run_shiny,
    downsampler_options = downsampler_options,
    plotly_options = plotly_options,
    plotly_layout_options = plotly_layout_options,
    shiny_options = shiny_options,
    width = width, height = height
  )

  invisible(shd)
}

#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.data.frame <- function(
    obj = NULL, tz = Sys.timezone(),
    n_out = 1000L,
    aggregator = min_max_aggregator$new(),
    run_shiny = TRUE,
    downsampler_options = list(),
    plotly_options = list(type = "scatter", mode = "lines"),
    plotly_layout_options = list(),
    shiny_options = list(),
    width = "100%", height = "600px",
    ...
) {

  df <- obj

  assertthat::assert_that("x" %in% colnames(df) && "y" %in% colnames(df))

  if (inherits(df$x, "numeric") || inherits(df$x, "integer")) {
    shd <- shiny_hugeplot(
      df$x, df$y,
      n_out = n_out,
      aggregator = aggregator,
      run_shiny = run_shiny,
      downsampler_options = downsampler_options,
      plotly_options = plotly_options,
      plotly_layout_options = plotly_layout_options,
      shiny_options = shiny_options,
      width = width, height = height
    )
    invisible(shd)
  } else if (inherits(df$x, "nanotime")) {
    shd <- shiny_hugeplot(
      df$x, df$y, tz = tz,
      n_out = n_out,
      aggregator = aggregator,
      run_shiny = run_shiny,
      downsampler_options = downsampler_options,
      plotly_options = plotly_options,
      plotly_layout_options = plotly_layout_options,
      shiny_options = shiny_options,
      width = width, height = height
    )
    invisible(shd)
  }

  invisible(shd)
}

#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.plotly <- function(
  obj,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  downsampler_options = list(),
  shiny_options = list(),
  width = "100%", height = "600px",
  ...
  ) {

  p <- obj

  downsampler_options[["n_out"]] <- as.integer(n_out)
  downsampler_options[["aggregator"]] <- aggregator

  shd <- do.call(
    shiny_downsampler$new,
    c(list(figure = p), downsampler_options)
  )

  if (run_shiny) {
    app <- do.call(
      shd$show_shiny,
      list(shiny_options, width, height),
    )
    runApp(app)
  }

  invisible(shd)
}
