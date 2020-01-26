library(shiny)

ui <- fluidPage(
  rmarkdown::html_dependency_font_awesome(),
  # ---- App UI ----
  h2("A Long Running Task"),
  sliderInput("steps", "Steps", 1, 10, value = 2),
  actionButton("run", "Run Task"),
  div(
    id = "plot-container",
    style = "width: 400px; height: 200px;",
    plotOutput("plot", width = "100%", height = "100%")
  ),
  # ---- JavaScript ----
  tags$script(HTML(
    "
    const runBtn = document.getElementById('run')

    // store the initial run button innerHTML
    // const runBtnOriginal = ...

    // Replace the button html with a font-awesome spinner icon
    // <i class=\"fas fa-spinner fa-spin fa-lg\"></i>

    $('#plot').on('shiny:recalculating', function() {
      runBtn.classList.add('disabled')
      runBtn.setAttribute('disabled', true)
      // replace innerHTML of runBtn
    })

    // Undo the above steps when the output is ready
    // -> you need to *remove* the disabled attribute
    $('#plot').on('shiny:value', function() {
      runBtn.classList.remove('disabled')
      runBtn.removeAttribute('disabled')
      // restore innerHTML of runBtn
    })
    "
  ))
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    # run when the button is clicked
    input$run
    steps <- isolate(input$steps)

    wait_times <- runif(steps)
    for (i in seq_len(steps)) {
      Sys.sleep(wait_times[i])
    }

    hist(wait_times)
  }, height = 300, width = 400)
}

shinyApp(ui, server)
