library(shiny)

options(scipen = 1e3) # "disable" scientific notation for this app

shiny::registerInputHandler(
  type = "datetime",
  fun = function(value, session, inputName) {
    # value is the parsed JSON value returned by the input
    # if you forget to divide by 1000 your browser will come from the future
    #
    # Question: Is this a good place to correct the time zone? Why/why not?
    #           If you want to take this exercise further, research the Date
    #           object and learn how to get the timezoneOffset of the browser.
    #           Send that information with the current time and use it here.
    lubridate::as_datetime(value / 1000)
  },
  # Only one input handler can be registered per session per type, for
  # prototyping you can use force = TRUE to just overwrite the existing one.
  force = TRUE
)

ui <- fluidPage(
  h2("Browser Time!"),
  actionButton("get_time", "What time is it?"),
  verbatimTextOutput("the_time_is"),
  tags$script(HTML(
    "
    document
      .getElementById('get_time')
      .addEventListener('click', function() {
        Shiny.setInputValue('the_time:datetime', Date.now())
      })
    "
  ))
)

server <- function(input, output, session) {
  output$the_time_is <- renderPrint(input$the_time)
}

shinyApp(ui, server)
