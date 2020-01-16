library(shiny)

options(scipen = 1e3) # "disable" scientific notation for this app

# Use shiny::registerInputHandler() to properly format the timestamp into an R
# datetime object. You may want to use the "shinyregisterInputHandler" snippet
# from {js4shiny}. You also need to decorate the inputId when you set it in the
# JS code so that shiny knows which handler to use.
#
# Hint:
#   Base R   : as.POSIXct(origin = "1970-01-01")
#   tidyverse: lubridate::as_datetime(

ui <- fluidPage(
  h2("Browser Time!"),
  actionButton("get_time", "What time is it?"),
  verbatimTextOutput("the_time_is"),
  tags$script(HTML(
    "
    document
      .getElementById('get_time')
      .addEventListener('click', function() {
        // Here, we're choosing the inputId ourselves
        // Each click of #get_time sends the current date to input$the_time
        Shiny.setInputValue('the_time', Date.now())
      })
    "
  ))
)

server <- function(input, output, session) {
  output$the_time_is <- renderPrint(input$the_time)
}

shinyApp(ui, server)
