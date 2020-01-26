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
    # un-comment this code
    # tags$div(class="heartbeat-loader", HTML("Loading&#8230;")),
    plotOutput("plot", width = "100%", height = "100%")
  ),
  # ---- JavaScript ----
  tags$script(HTML(
    "
    const runBtn = document.getElementById('run')
    const runBtnOriginal = runBtn.innerHTML

    // I've added spinner in the #plot-container above
    // *use jQuery* to find the .heartbeat-loader
    // const $plotLoader = ...

    // Then use jQuery's .hide() and .show() functions
    // to toggle the hide the plot and show the loader
    // when the plot is rendering and then reverse the
    // operation when the plot is finished.

    $('#plot').on('shiny:recalculating', function() {
      runBtn.classList.add('disabled')
      runBtn.setAttribute('disabled', true)
      runBtn.innerHTML =
        `<i class=\"fas fa-spinner fa-spin\"></i> Please wait...`
      // hide #plot
      // show $plotLoader
    })

    // And restore the original text when done
    $('#plot').on('shiny:value', function() {
      runBtn.classList.remove('disabled')
      runBtn.removeAttribute('disabled')
      runBtn.innerHTML = runBtnOriginal
      // hide $plotLoader
      // show #plot
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
