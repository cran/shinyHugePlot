#' Wrapper for plotting large-sized data using \code{shinyHugePlot}
#'
#' @description
#' This is a S3 class function to easily plot large-sized data using
#' \code{downsampler} object including \code{plotly} and
#' \code{shiny} application.
#' Using data that is given as a first argument,
#' \code{shiny} application will be constructed and (by default,) executed.
#' As the first argument, many classes are applicable,
#' ranging from a numeric vector representing y values
#' to a \code{downsampler} object containing
#' original data, layout of the figure and
#' options for aggregating the original data.
#' @param obj Numeric/\code{nanotime}/\code{POSIXt} vector,
#' numeric matrix, data.frame, single character string,
#' \code{plotly} object, or \code{downsampler} object.
#' If a numeric vector is given, it will be used as y values of the figure
#' of the \code{shiny} application
#' (the x values are calculated by \code{seq_along(obj)}).
#' It will be interpreted as the x values if you use \code{y} argument together.
#' If a \code{nanotime} (see \code{nanotime} package) vector is given,
#' it will be used as the x values (\code{y} argument is mandatory).
#' If a numeric matrix is given, which must have more than 2 columns,
#' the first and second column values will be used as the x and y values.
#' If a data frame is given,
#' which must have columns named \code{x} and \code{y},
#' these columns will be used as the x and y values.
#' If a single character string is given, it will be used as a file path
#' to obtain a data frame
#' (data frame will be loaded using \code{data.table::fread}).
#' If a \code{plotly} object is given, the data and layout of it will be used
#' for constructing the figure of the \code{shiny} application.
#' If a \code{downsampler} object is given, the data, layout, and
#' down-sampling options for aggregating original data of it will be used for
#' constructing \code{shiny} application.
#' @param y Numeric vector, optional.
#' y values of the figure of \code{shiny} application,
#' which is required if the \code{obj} argument is used as the x values.
#' @param tz Timezone, optional.
#' It is used to convert the \code{nanotime} to the time displayed in the figure.
#' By default, \code{Sys.timezone()}.
#' @param n_out Integer, optional.
#' Number of samples get by the down-sampling. By default, 1000.
#' @param aggregator Instance of R6 classes for aggregating data, optional.
#' The classes can be listed using \code{list_aggregators}.
#' By default, \code{min_max_aggregator$new()}.
#' @param fread_options Named list, optional.
#' Arguments passed to \code{data.table::fread},
#' which is used if a single character string is given as the \code{obj}.
#' @param downsampler_options Named list, optional.
#' Arguments passed to \code{downsampler$new}.
#' Note that use \code{aggregator} and \code{n_out} arguments
#' if you want to set these arguments.
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
#' whether a generated \code{shiny} application will be launched.
#' By default, \code{TRUE}.
#' @param use_light_build Boolean, optional.
#' Whether \code{shinyHugePlot::plotly_build_light} will be used.
#' (if \code{FALSE}, \code{plotly::plotly_build} will be used)
#' By default, \code{TRUE}.
#' @param ... Not used.
#' @importFrom htmltools div br
#' @importFrom tidyselect everything
#' @export
#' @examples
#' \donttest{
#' data(noise_fluct)
#'
#' shiny_hugeplot(noise_fluct$f500)
#' shiny_hugeplot(noise_fluct$time, noise_fluct$f500)
#' }
shiny_hugeplot <- function(obj, ...) {
  UseMethod("shiny_hugeplot", obj)
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.default <- function(
    obj = NULL, y = NULL,
    tz = Sys.timezone(),
    use_light_build = TRUE,
    plotly_options = list(type = "scatter", mode = "lines"),
    plotly_layout_options = list(),
    aggregator = min_max_aggregator$new(),
    n_out = 1000L,
    run_shiny = TRUE,
    downsampler_options = list(),
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
  args$x <- NULL
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
  if (run_shiny) {
    invisible()
  } else{
    return(app)
  }
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.character <- function(
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
    width = "100%", height = "600px",
    ...
) {

  args <- c(as.list(environment()), list(...))
  assertthat::assert_that(inherits(fread_options, "list"))

  args$obj <- do.call(data.table::fread, c(args$obj, fread_options))

  # proceed to shiny_hugeplot.default
  app <- do.call(shiny_hugeplot, args)
  if (run_shiny) {
    invisible()
  } else{
    return(app)
  }
}

#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.matrix <- function(
  obj = NULL,
  n_out = 1000L,
  aggregator = min_max_aggregator$new(),
  run_shiny = TRUE,
  use_light_build = TRUE,
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
  if (run_shiny) {
    invisible()
  } else{
    return(app)
  }
}

#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.data.frame <- function(
    obj = NULL, tz = Sys.timezone(),
    n_out = 1000L,
    aggregator = min_max_aggregator$new(),
    run_shiny = TRUE,
    use_light_build = TRUE,
    downsampler_options = list(),
    plotly_options = list(type = "scatter", mode = "lines"),
    plotly_layout_options = list(),
    shiny_options = list(),
    width = "100%", height = "600px",
    ...
) {

  assertthat::assert_that("x" %in% colnames(obj) && "y" %in% colnames(obj))
  args <- c(as.list(environment()), list(...))

  df <- args$obj
  args$obj <- df$x
  args$y <- df$y

  # proceed to shiny_hugeplot.default
  app <- do.call(shiny_hugeplot, args)
  if (run_shiny) {
    invisible()
  } else{
    return(app)
  }
}


#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.plotly <- function(
    obj,
    n_out = 1000L,
    aggregator = min_max_aggregator$new(),
    run_shiny = TRUE,
    use_light_build = TRUE,
    downsampler_options = list(),
    shiny_options = list(),
    width = "100%", height = "600px",
    ...
) {

  args <- c(as.list(environment()), list(...))

  args$downsampler_options[["n_out"]] <- as.integer(n_out)
  args$downsampler_options[["aggregator"]] <- aggregator
  args$downsampler_options[["use_light_build"]] <- use_light_build

  ds <- do.call(
    downsampler$new,
    c(list(figure = args$obj), args$downsampler_options)
  )

  args$obj <- ds

  # proceed to shiny_hugeplot.default
  app <- do.call(shiny_hugeplot, args)
  if (run_shiny) {
    invisible()
  } else{
    return(app)
  }
}

#' @rdname shiny_hugeplot
#' @export
shiny_hugeplot.downsampler <- function(
    obj, run_shiny = TRUE,
    shiny_options = list(),
    width = "100%", height = "600px",
    ...) {

  ds <- obj

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
          NA_position = ds$downsample_options$NA_position[1]
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
        paste0(basename(tempfile("")), ".json")
      },
      content = function(file) {
        traces_df <- ds$plotly_data_to_df(
          ds$figure$x$data, use_datatable = FALSE
        ) %>%
          dplyr::left_join(ds$downsample_options, by = "uid") %>%
          dplyr::select(
            tidyselect::vars_select_helpers$where(
              function(x) !inherits(x, "R6")
            )
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
    session$onSessionEnded(function() {
      stopApp()
    })
  }


  app <- shinyApp(ui = ui, server = server, options = shiny_options)

  if (run_shiny) {
    runApp(app)
    invisible()
  } else {
    return(app)
  }
}

