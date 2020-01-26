library(shiny)

ui <- fluidPage(
  p(
    "Open this page in your browser and use the developer console to send data",
    "back to Shiny"
  ),
  HTML(
"
<pre><code>Shiny.setInputValue('input_name', value)</code></pre>
"
  ),
  verbatimTextOutput("all_inputs")
)

server <- function(input, output, session) {
  output$all_inputs <- renderPrint({
    inputs <- setNames(nm = names(input))
    all_inputs <- lapply(inputs, function(n) input[[n]])
    str(all_inputs)
  })
}

shinyApp(ui, server)
