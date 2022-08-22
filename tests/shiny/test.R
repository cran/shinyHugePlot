
# prepare the data
d <- tibble::tibble(
  x = seq(0, 1e7),
  t = nanotime::nanotime(Sys.time()) + seq(0, 1e7) * 7e4,
  tp = Sys.time() + seq(0, 1e7) * 7,
  tch = format(t, "%Y-%m-%d %H:%M:%E9S"),
  y = (3 + sin(x / 200) + runif(1e7 + 1) / 10) * x / 1000
)

# system.time(
# prepare plotly to which down-sampler is applied
figH <- shiny_downsampler$new(
  figure = plotly::plot_ly() %>%
    add_trace(data = d, x = ~t, y = ~y, type = "scatter", mode = "lines"),
  n_out = 200
  )
# )

figH$show_shiny()

plotly::plot_ly() %>%
  add_trace(data = d, x = ~t, y = ~y, type = "scatter", mode = "lines") %>%
  apply_downsampler()


# Define UI
ui <- fluidPage(
  # plotlyOutput(outputId="fig", width="800px", height="500px"),
  plotlyOutput(outputId="figH", width="800px", height="500px")
)

# Define server logic
server <- function(input, output, session) {

  # output$fig <- renderPlotly(fig)
  output$figH <- renderPlotly(figH$figure)#$plotly_with_relayout())

  observeEvent(plotly::event_data("plotly_relayout"),{
  # observeEvent(input[["figH_relayout"]],{
    updatePlotlyH(session, outputId = "figH", plotly::event_data("plotly_relayout"), figH)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
