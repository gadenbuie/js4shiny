library(shiny)

ui <- fluidPage(
  titlePanel("htlmwidgets rock"),
  sidebarLayout(
    sidebarPanel(
      # Sidebar with a slider for number of bars
      sliderInput("n_bars", "Number of bars", 1, 26, 10),
      # And a button to generate new data
      actionButton("new_data", "New Data")
    ),
    mainPanel(
      frappeCharts::frappeChartOutput("chart"),
      # preview output from frappeChart widget
      verbatimTextOutput("selected")
    )
  )
)

server <- function(input, output) {
  initial_data <- data.frame(
    x = LETTERS[seq_len(10)],
    Frequency = rep(0.5, 10)
  )

  data <- reactive({
    # Re-generate data on button click or
    # when the number of bars changes
    input$new_data

    data.frame(
      x = LETTERS[seq_len(input$n_bars)],
      Frequency = runif(input$n_bars)
    )
  })

  output$chart <- frappeCharts::renderFrappeChart({
    frappeCharts::frappeChart(
      initial_data,
      type = "bar",
      tooltipOptions = list(
        formatTooltipY = htmlwidgets::JS("d => Math.round(d * 100) + '%'")
      )
    )
  })

  observe({
    # Update the data in the chart
    frappeCharts::updateFrappeChart("chart", data())
  })

  output$selected <- renderPrint({
    # Preview the value returned to Shiny
    input$chart_selected
  })
}

shinyApp(ui = ui, server = server)
