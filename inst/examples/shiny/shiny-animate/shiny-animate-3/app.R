library(shiny)

people <- sample(babynames::babynames$name, 4)

ui <- fluidPage(
  fluidRow(
    style = "margin-top: 1em;",
    purrr::map(people, ~ {
      div(
        class = "col-xs-3",
        h3(id = .x, .x)
      )
    })
  ),
  selectInput("name", "Hello", choices = people),
  div(
    class = "btn-group",
    actionButton("jump", "Jump"),
    actionButton("blink", "Blink")
  ),
  tags$style(HTML(
    "
    @keyframes jump {
      from {
        -webkit-transform: translateY(0);
        transform: translateY(0);
      }

      50% {
        -webkit-transform: translateY(-20px);
        transform: translateY(-20px);
      }

      to {
        -webkit-transform: translateY(0);
        transform: translateY(0);
      }
    }
    .jump {
      animation: jump 0.66s cubic-bezier(.91,-0.34,0,1.61);
    }

    @keyframes blink {
      0%   { opacity: 1 }
      50%  { opacity: 0 }
      100% { opacity: 1 }
    }
    .blink {
      animation: blink 0.66s ease-in-out;
      animation-iteration-count: 2; /* repeat twice */
    }
    "
  )),
  tags$script(HTML(
    "
    Shiny.addCustomMessageHandler('animate', function({id, animation}) {
      const el = document.getElementById(id)
      el.classList.remove(animation)
      setTimeout(() => el.classList.add(animation), 50)
      setTimeout(() => el.classList.remove(animation), 1000)
    })
    "
  ))
)

server <- function(input, output, session) {
  animate <- function(
    id,
    animation = "jump",
    session = shiny::getDefaultReactiveDomain()
  ) {
    session$sendCustomMessage("animate", list(id = input$name, animation = animation))
  }

  observeEvent(input$jump, animate(input$name))
  observeEvent(input$blink, animate(input$name, "blink"))
}

shinyApp(ui, server)
