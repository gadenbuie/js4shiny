library(shiny)

options(scipen = 1e3) # "disable" scientific notation for this app

shiny::registerInputHandler(
  type = "datetime",
  fun = function(value, session, inputName) {
    x <- lubridate::as_datetime(value$time / 1000)

    # It's too hard to figure out the actual time zone, so I'm just going to
    # format the time into a string, adjusted to be in the correct time zone.
    strftime(x + value$offset * 60^2, "%F %T")
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
        // JavaScript's built in Date methods are quite odd. This is how you
        // get the current time AND the timezoneOffset (in minutes!) from UTC.
        const now = new Date()
        const time = {time: now.getTime(), offset: now.getTimezoneOffset()}
        Shiny.setInputValue('the_time:datetime', time)

        // Another option is to just get the locale-formatted string in JS
        Shiny.setInputValue('the_js_time', now.toLocaleString())
      })
    "
  ))
)

server <- function(input, output, session) {
  output$the_time_is <- renderPrint(list(
    `processed in r` = input$the_time,
    `processed in the browser` = input$the_js_time
  ))
}

shinyApp(ui, server)
