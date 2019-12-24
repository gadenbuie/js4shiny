library(shiny)

ui <- htmlTemplate(
  "app.html",
  box_size = sliderInput(
    "size",
    "Size",
    min = 0, max = 400, step = 5L, value = 100L, ticks = FALSE
  ),
  box_type = selectInput(
    "box_sizing",
    "Box Sizing",
    choices = c(
      "Content Box" = "content-box",
      "Border Box" = "border-box"
    )
  ),
  box_padding = sliderInput(
    "padding",
    "Padding Width",
    min = 0, max = 40, step = 5L, value = 0L, ticks = FALSE
  ),
  box_border = sliderInput(
    "border",
    "Border Width",
    min = 0, max = 40, step = 1L, value = 0L, ticks = FALSE
  ),
  box_margin = sliderInput(
    "margin",
    "Margin Width",
    min = 0, max = 40, step = 5L, value = 0L, ticks = FALSE
  )
)

server <- function(input, output, session) {
  px <- function(x) {
    if (!is.numeric(x)) return(paste(x, collapse = " "))
    paste0(x, "px", collapse = " ")
  }
  observe({
    x <- list(
      width = input$size,
      height = input$size,
      "box-sizing" = input$box_sizing,
      padding = input$padding,
      "border-width" = input$border,
      margin = input$margin
    )
    x <- lapply(x, px)
    session$sendCustomMessage('setBoxSize', x)
  })

  observeEvent(input$preset, {
    if (input$preset == "preset-content") {
      updateSliderInput(session, "size", value = 200)
      updateSelectInput(session, "box_sizing", selected = "content-box")
      updateSliderInput(session, "padding", value = 30)
      updateSliderInput(session, "border", value = 10)
      updateSliderInput(session, "margin", value = 20)
    }
    if (input$preset == "preset-border") {
      updateSliderInput(session, "size", value = 280)
      updateSelectInput(session, "box_sizing", selected = "border-box")
      updateSliderInput(session, "padding", value = 30)
      updateSliderInput(session, "border", value = 10)
      updateSliderInput(session, "margin", value = 20)
    }
    if (input$preset == "preset-reset") {
      updateSliderInput(session, "padding", value = 0)
      updateSliderInput(session, "border", value = 0)
      updateSliderInput(session, "margin", value = 0)
    }
  })
}

shinyApp(ui, server)
