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
      # The widget output goes here with <widget>Output()
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    # Re-generate data on button click or when the number of bars changes
    input$new_data

    data.frame(
      x = LETTERS[seq_len(input$n_bars)],
      Frequency = runif(input$n_bars)
    )
  })

  # Render the widget here with render<widget>(<widget>)
  # output$chart <- ....
}

shinyApp(ui = ui, server = server)
