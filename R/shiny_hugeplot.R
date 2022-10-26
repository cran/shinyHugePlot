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
#' @importFrom htmltools div br
#' @importFrom tidyselect everything
#' @export
#' @examples
#' \donttest{
#' data(noise_fluct)
#'
#' shiny_hugeplot(noise_fluct$level)
#' shiny_hugeplot(noise_fluct$t, noise_fluct$level)
#' }
shiny_hugeplot <- function(obj, ...) {
  UseMethod("shiny_hugeplot", obj)
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.default <- function(
    obj = NULL, y = NULL, tz = Sys.timezone(),
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

  args <- c(as.list(environment()), list(...))


  if (is.null(args$plotly_options$type)) args$plotly_options$type <- "scatter"
  if (is.null(args$plotly_options$mode)) args$plotly_options$mode <- "lines"

  x <- args$obj
  y <- args$y
  args$obj <- NULL
  args$y <- NULL

  assertthat::assert_that(!is.null(x) || !is.null(y))

  if (is.null(x)) {
    x = seq_along(y)
  } else if (is.null(y)) {
    y = x
    x = seq_along(y)
  }

  assertthat::assert_that(
    inherits(x, c("numeric", "integer", "POSIXt", "integer64"))
    )

  # change x values and axis type, if necessary
  if (inherits(x, "integer64")) {

    tz_hms <- format(as.POSIXct("1970-01-01", tz), "%z") %>%
      stringr::str_replace("^(\\-?)\\+?([0-9]{1,2})([0-9]{2})", "\\1\\2:\\3:00")
    x <- (x + nanotime::as.nanoduration(tz_hms)) %>%
      format("%Y-%m-%d %H:%M:%E9S")
    args$plotly_layout_options$xaxis$type <- "date"

  } else if (inherits(x, "POSIXt")) {

    args$plotly_layout_options$xaxis$type <- "date"
  }


  # generate plotly object
  fig <- do.call(
    plotly::plot_ly,
    c(list(x = x, y = y), args$plotly_options)
  )

  if (length(args$plotly_layout_options) > 0) {

    fig <- do.call(
      plotly::layout,
      c(list(fig), args$plotly_layout_options)
    )
  }

  args$obj <- fig

  # proceed to shiny_hugeplot.plotly
  app <- do.call(shiny_hugeplot, args)
  return(app)
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

  args <- c(as.list(environment()), list(...))
  assertthat::assert_that(inherits(args$obj, "numeric"))

  mat <- args$obj
  args$obj <- mat[,1]
  args$y <- mat[,2]

  # proceed to shiny_hugeplot.default
  app <- do.call(shiny_hugeplot, args)
  return(app)
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

  assertthat::assert_that("x" %in% colnames(obj) && "y" %in% colnames(obj))

  df <- args$obj
  args$obj <- df$x
  args$y <- df$y

  # proceed to shiny_hugeplot.default
  app <- do.call(shiny_hugeplot, args)
  return(app)
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

  downsampler_options[["n_out"]] <- as.integer(n_out)
  downsampler_options[["aggregator"]] <- aggregator

  ds <- do.call(
    downsampler$new,
    c(list(figure = obj), downsampler_options)
  )

  ui <- fluidPage(
    checkboxInput(
      "agg_select_check",
      label = "Change down-sample condition"
    ),
    conditionalPanel(
      condition = "input.agg_select_check == true",
      htmltools::div(
        selectizeInput(
          "agg_selector", label = "Aggregator",
          choices = list_aggregators() %>% str_subset("^[^(cus)]"),
          select = ds$downsample_options$aggregator_name[1]
        ),
        numericInput(
          "n_out_input", label = "Number of samples",
          value = ds$downsample_options$n_out[1],
          step = 1, min = 1, max = 1e5
        ),
        style = "display:flex"
      )
    ),
    plotlyOutput(outputId = "fig", width = width, height = height),
    "Relayout order:",
    verbatimTextOutput("relayout_order"),
    htmltools::br(),
    downloadButton("get_data", "Get shown data")
  )


  server <- function(input, output, session) {
    output$fig <- renderPlotly(ds$figure)
    observe(
      {
        agg_class_input <- input[["agg_selector"]]

        if (input[["agg_selector"]] == "null_aggregator") {
          shinyjs::disable("n_out_input")
        } else {
          shinyjs::enable("n_out_input")
        }

        agg_input <- eval(parse(text = agg_class_input))$new(
          interleave_gaps = ds$downsample_options$interleave_gaps[1],
          nan_position = ds$downsample_options$nan_position[1]
        )
        n_out_input <- input[["n_out_input"]]

        if (
          agg_class_input != ds$downsample_options$aggregator_name[1] ||
          n_out_input != ds$downsample_options$n_out[1]
        ) {
          reload <- TRUE
          ds$set_downsample_options(
            aggregator = agg_input,
            n_out = n_out_input
          )
        } else {
          reload <- FALSE
        }

        updatePlotlyH(
          session = session, outputId = "fig",
          relayout_order = plotly::event_data("plotly_relayout"),
          ds_obj = ds, reload = reload
        )
      },
      label = "figure_updater"
    )

    output[["relayout_order"]] <- renderPrint({
      relayout_order <- plotly::event_data("plotly_relayout")
      if (is.null(relayout_order)) {
        return()
      } else {
        plotly::event_data("plotly_relayout") %>%
          purrr::map(~as.character(.x)) %>%
          tibble::as_tibble() %>%
          tidyr::pivot_longer(tidyselect::everything())
      }
    })

    output[["get_data"]] <- downloadHandler(
      filename = function() {
        paste0(basename(tempfile("")), ".csv")
      },
      content = function(file) {
        traces_df <- ds$plotly_data_to_df(
          ds$figure$x$data, use_datatable = FALSE
          ) %>%
          dplyr::left_join(ds$downsample_options, by = "uid") %>%
          dplyr::select(
            tidyselect::vars_select_helpers$where(!inherits("R6"))
            ) %>%
          # dplyr::select(-aggregator_inst) %>%
          dplyr::mutate(
            x = purrr::modify_if(
              .data$x, ~inherits(.x, "nanotime"),
              ~as.POSIXct(.x, tz = Sys.timezone())
            )
          )

        jsonlite::write_json(traces_df, path = file, pretty = TRUE)

      }
    )
  }


  app <- shinyApp(ui = ui, server = server, options = shiny_options)

  if (run_shiny) {
    runApp(app)
  }

  return(app)
}
