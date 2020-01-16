library(shiny)

options(scipen = 1e3) # "disable" scientific notation for this app

ui <- fluidPage(
  h2("Browser Time!"),
  actionButton("get_time", "What time is it?"),
  verbatimTextOutput("the_time_is"),
  tags$script(HTML(
    "
    // Your job is to write the JavaScript code to listen for click events on
    // the button created by actionButton() above and return the time from the
    // browser. You can use the Date.now() method. Search MDN to learn more.
    "
  ))
)

server <- function(input, output, session) {
  output$the_time_is <- renderPrint(input$the_time)
}

shinyApp(ui, server)
