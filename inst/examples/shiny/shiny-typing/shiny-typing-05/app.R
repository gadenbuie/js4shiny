library(shiny)
library(frappeCharts)

options(scipen = 1e3)

source("module_typingStats.R")

typingSpeedInput <- function(inputId, label, placeholder = NULL, rows = 4) {
  .label <- label
  htmltools::withTags(
    div(
      class = "form-group typing-speed",
      label(class = "control-label", `for` = inputId, .label),
      textarea(id = inputId, class = "form-control", placeholder = placeholder,
               rows = rows),
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
  typingStatsUI('typing_stats'),
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

  wpm <- typingStats(
    "typing_stats",
    typing = reactive(input$typing),
    typing_reset = reactive(input$typing_reset)
  )

  output$chart_typing_speed <- frappeCharts::renderFrappeChart({
    frappeCharts::frappeChart(
      data.frame(time = 0, wpm = 0),
      type = "line",
      title = "Your Typing Speed",
      is_navigable = FALSE,
      axisOptions = list(xIsSeries = TRUE),
      lineOptions = list(regionFill = TRUE)
    )
  })

  observeEvent(wpm()$time, {
    frappeCharts::updateFrappeChart('chart_typing_speed', wpm())
  })
}

shinyApp(ui, server)
