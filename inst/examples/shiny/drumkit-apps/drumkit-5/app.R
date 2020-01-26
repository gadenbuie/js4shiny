library(shiny)
library(htmltools)
library(glue)

drumkitUI <- function(id, height = "300px", width = "300px") {
  ns <- NS(id)
  height <- validateCssUnit(height)
  width <- validateCssUnit(width)
  tagList(
    htmlTemplate(
      filename = "drumkit.svg",
      id = glue('id = "{ns("drumkit")}"'),
      width = glue('width = "{width}"'),
      height = glue('height = "{height}"')
    )
  )
}

drumkit <- function(input, output, session, ...) {
}

ui <- fluidPage(
  drumkitUI("drums")
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
