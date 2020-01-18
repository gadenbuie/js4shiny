library(shiny)
# devtools::install_github("gadenbuie/js4shiny-frappeCharts@pkg")
library(frappeCharts)

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
  frappeCharts::frappeChartOutput("chart_typing_speed")
  # verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  output$debug <- renderPrint({
    str(list(typing = input$typing, typing_reset = input$typing_reset))
  })

  observeEvent(input$reset, {
    resetTypingSpeed("typing")
  })

  wpm <- reactiveValues(time = c(), wpm = c())

  observeEvent(input$typing_reset, {
    wpm$time <- c()
    wpm$wpm <- c()
  })

  observeEvent(input$typing, {
    req(input$typing)
    wpm$time <- c(wpm$time, input$typing$time)
    wpm$wpm <- c(wpm$wpm, input$typing$wpm)
  })

  output$chart_typing_speed <- frappeCharts::renderFrappeChart({
    frappeCharts::frappeChart(
      data.frame(time = wpm$time, wpm = wpm$wpm),
      type = "line",
      title = "Your Typing Speed",
      is_navigable = FALSE,
      axisOptions = list(xIsSeries = TRUE),
      lineOptions = list(regionFill = TRUE)
    )
  })

}

shinyApp(ui, server)
