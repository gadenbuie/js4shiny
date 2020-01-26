library(shiny)
library(htmltools)
library(glue)

drumkitUI <- function(id, height = "300px", width = "300px") {
  ns <- NS(id)
  height <- validateCssUnit(height)
  width <- validateCssUnit(width)
  addResourcePath("sounds", "drumkit/sounds/")
  tagList(
    htmlTemplate(
      filename = "drumkit/drumkit.svg",
      id = glue('id = "{ns("drumkit")}"'),
      width = glue('width = "{width}"'),
      height = glue('height = "{height}"')
    ),
    htmltools::htmlDependency(
      name = "shiny-drumkit",
      version = "1.0.0",
      src = "drumkit",
      script = "drumkit.js",
      stylesheet = "drumkit.css",
      all_files = TRUE
    )
  )
}

drumkit <- function(id) {
  callModule(id = id, function(input, output, session, ...) {
    return(reactive(input$drumkit))
  })
}

ui <- fluidPage(
  div(
    class = "col-xs-6",
    drumkitUI("mickey", "100%", "100%")
  ),
  div(
    class = "col-xs-6",
    drumkitUI("bill", "100%", "100%")
  ),
  verbatimTextOutput("debug")
)

server <- function(input, output, session) {
  mickey <- drumkit("mickey")
  bill <- drumkit("bill")

  output$debug <- renderPrint(list(
    mickey_played = mickey(),
    bill_played = bill()
  ))
}

shinyApp(ui, server)
