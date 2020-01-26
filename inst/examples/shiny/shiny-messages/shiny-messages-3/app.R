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
    Shiny.addCustomMessageHandler('toggle', function(id) {
      const el = document.getElementById(id)
      const isHidden = el.classList.contains('hidden')
      el.classList.toggle('hidden')
      $(el).trigger(isHidden ? 'shown' : 'hidden')
    })
    "
  ))
)

server <- function(input, output, session) {
  observeEvent(input$toggle, {
    session$sendCustomMessage('toggle', tolower(input$element))
  })

  output$custom_message <- renderUI(h3(input$message))
}

shinyApp(ui, server)
