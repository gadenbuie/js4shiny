library(shiny)

addResourcePath("figures", js4shiny:::js4shiny_file('man', 'figures'))

ui <- fluidPage(
  rmarkdown::html_dependency_font_awesome(),
  tags$head(includeCSS("style.css")),
  # ---- App UI ----
  h2("A Long Running Task"),
  sliderInput("steps", "Steps", 1, 10, value = 2),
  actionButton("run", "Run Task"),
  div(
    id = "plot-container",
    style = "width: 400px; height: 200px;",
    tags$div(class="heartbeat-loader"),
    plotOutput("plot", width = "100%", height = "100%")
  ),
  # ---- JavaScript ----
  tags$script(HTML(
    "
    const runBtn = document.getElementById('run')
    const runBtnOriginal = runBtn.innerHTML

    const $plotLoader = $('#plot-container .heartbeat-loader')

    $('#plot').on('shiny:recalculating', function() {
      runBtn.classList.add('disabled')
      runBtn.setAttribute('disabled', true)
      runBtn.innerHTML =
        `<i class=\"fas fa-spinner fa-spin\"></i> Please wait...`
      $('#plot').hide()
      $plotLoader.show()
    })

    $('#plot').on('shiny:value', function() {
      runBtn.classList.remove('disabled')
      runBtn.removeAttribute('disabled')
      runBtn.innerHTML = runBtnOriginal
      $plotLoader.hide()
      $('#plot').show()
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
