library(shiny)

options(scipen = 1e3)

typingSpeedInput <- function(inputId, label, placeholder = NULL) {
  .label <- label
  htmltools::withTags(
    div(
      class = "form-group typing-speed",
      label(class = "control-label", `for` = inputId, .label),
      textarea(id = inputId, class = "form-control", placeholder = placeholder),
      htmltools::htmlDependency(
        name = "typingSpeed",
        version = "0.0.1",
        src = ".",
        script = "typing.js",
        all_files = FALSE
      )
    )
  )
}

resetTypingSpeed <- function(inputId, session = getDefaultReactiveDomain()) {
  session$sendInputMessage("typing", TRUE)
}

ui <- fluidPage(
  # textAreaInput("typing", "Type here..."),
  typingSpeedInput("typing", "Type here..."),
  actionButton("reset", "Reset"),
  verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  output$debug <- renderPrint(input$typing)

  observeEvent(input$reset, {
    resetTypingSpeed("typing")
  })
}

shinyApp(ui, server)
