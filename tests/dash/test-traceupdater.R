# Create a Dash app with trace updater
app <- dash_app()
set_layout(
  app,
  dash::dccGraph(
    id = "resample-figure",
    figure = list(
      data = list(
        list(
          x = 1:1e3,
          y = sin(1:1e3 / 1e2),
          type = "scatter"
        )
      )
    )
  ),
  trace_updater(
    id = "trace-updater",
    gdID = "resample-figure",
    sequentialUpdate = FALSE
  )
)

app$run_server()
