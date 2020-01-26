library(shiny)

ui <- fluidPage(
  selectInput("element", "Show", choices = c("Hello", "Goodbye", "Custom")),
  conditionalPanel(
    "input.element === 'Custom'",
    textInput("message", NULL, value = "What's up doc?")
  ),
  actionButton("toggle", "Show"),
  fluidRow(
    div(
      id = "hello",
      class = "col-xs-4 hidden",
      h3("Hello, you!")
    ),
    div(
      id = "goodbye",
      class = "col-xs-4 hidden",
      h3("Later, alligator")
    ),
    div(
      id = "custom",
      class = "col-xs-4 hidden",
      uiOutput("custom_message")
    )
  ),
  tags$script(HTML(
    "
    // Add a custom message handler to toggle the hidden class
    // of an element. The id of the element is sent from Shiny.
    "
  ))
)

server <- function(input, output, session) {
  observeEvent(input$toggle, {
    # When the toggle button is clicked,
    # send a custom containing the lowercased element id to show
  })

  output$custom_message <- renderUI(h3(input$message))
}

shinyApp(ui, server)
