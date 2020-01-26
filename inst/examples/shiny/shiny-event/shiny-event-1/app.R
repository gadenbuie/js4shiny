library(shiny)

ui <- fluidPage(
  # ---- App UI ----
  h2("A Long Running Task"),
  sliderInput("steps", "Steps", 1, 10, value = 2),
  actionButton("run", "Run Task"),
  div(
    id = "plot-container",
    style = "width: 400px; height: 200px;",
    plotOutput("plot", width = "100%", height = "100%")
  ),
  # ---- JavaScript ----
  tags$script(HTML(
    "
    // Find the #run button

    // When the plot is recalculating...
    // - add the 'disabled' class to the button
    // - use .setAttribute() to set disabled to true


    // Undo the above steps when the output is ready
    // -> you need to *remove* the disabled attribute
    "
  ))
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    # run when the button is clicked
    input$run
    steps <- isolate(input$steps)

    wait_times <- runif(steps)
    for (i in seq_len(steps)) {
      Sys.sleep(wait_times[i])
    }

    hist(wait_times)
  }, height = 300, width = 400)
}

shinyApp(ui, server)
