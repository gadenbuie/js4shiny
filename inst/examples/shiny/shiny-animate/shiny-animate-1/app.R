library(shiny)

ui <- fluidPage(
  actionButton("toggle", "Jump"),
  div(id = "hello", h3("Hello, you!")),
  tags$style(HTML(
    "
    /* Use keyfrmes below define the 'jump' animation:
     * > transform: translateY(Npx);
     # starting and ending (0%, 100%) at 0 and going to Npx at 50%.
     * Play with the translateY direction and size to get
     * the correct range of motion.
     */
    @keyframes jump {
      /* Define the keyframes here */
    }
    .jump {
      animation: jump 1s linear;
    }
    "
  )),
  tags$script(HTML(
    "
    // Add a custom message handler to add the animation class
    // to an element. An element may have already been animated,
    // so this function should remove the animtion class first.
    // If you immediately ad the .jump class back, it happens
    // too fast and the two action cancel each other out.
    // Instead, use setTimeout() and a delay of ~200 to add
    // the jump class to the element. Then clear the animation
    // once it's complete (i.e. ~1000ms later).
    "
  ))
)

server <- function(input, output, session) {
  observeEvent(input$toggle, {
    # When the toggle button is clicked,
    # send a custom containing the lowercased element id to show
  })
}

shinyApp(ui, server)
