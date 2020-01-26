library(shiny)

ui <- fluidPage(
  actionButton("toggle", "Jump"),
  div(id = "hello", h3("Hello, you!")),
  tags$style(HTML(
    "
    @keyframes jump {
      from {
        transform: translateY(0);
      }

      50% {
        transform: translateY(-20px);
      }

      to {
        transform: translateY(0);
      }
    }
    /* The .jump animation applies the 'jump' animation.
     * This is where the timing and easing function of the
     * animation are controlled. Both probably need to be adjusted.
     * The Firefox devtools are helpful, as is easings.net
     */
    .jump {
      animation: jump 1s linear;
    }
    "
  )),
  tags$script(HTML(
    "
    // update the message handler so that it takes an object
    // containing properties 'id' and 'animation' so that in
    // the future users can choose which animation is applied.
    Shiny.addCustomMessageHandler('animate', function(id) {
      const el = document.getElementById(id)
      el.classList.remove('jump')
      setTimeout(() => el.classList.add('jump'), 50)
      setTimeout(() => el.classList.remove('jump'), 1000)
    })
    "
  ))
)

server <- function(input, output, session) {
  # Write a function called "animate" that gives end users
  # a more friendly interface to the animation trigger
  observeEvent(input$toggle, {
    session$sendCustomMessage("animate", 'hello')
  })
}

shinyApp(ui, server)
