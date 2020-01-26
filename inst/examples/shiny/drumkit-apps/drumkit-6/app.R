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

drumkit <- function(input, output, session, ...) {
}

ui <- fluidPage(
  drumkitUI("drums")
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
