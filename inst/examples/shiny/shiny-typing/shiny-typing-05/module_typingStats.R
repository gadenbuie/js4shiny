typingStatsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-12 col-sm-9 col-md-6",
        uiOutput(ns("prompt"))
      ),
      div(
        class = "col-xs-12 col-sm-3 col-md-6",
        style = "min-height: 75px;",
        uiOutput(ns("wpm"))
      )
    )
  )
}

typingStats <- function(id, typing, typing_reset = reactive(NULL), n_prompt = 4) {
  callModule(typingStats_module, id, typing = typing, typing_reset = typing_reset)
}

date_now <- function() as.integer(Sys.time()) * 1000

typingStats_module <- function(
  input,
  output,
  session,
  typing,
  typing_reset = reactive(NULL),
  n_prompt = 4
) {
  ns <- session$ns

  wpm <- reactiveValues(time = date_now(), wpm = 0)

  reactive_df <- function(x) {
    as.data.frame(reactiveValuesToList(x))
  }

  observeEvent(typing_reset(), {
    wpm$time <- date_now()
    wpm$wpm <- 0
  })

  observeEvent(typing()$time, {
    wpm$time <- c(wpm$time, typing()$time)
    wpm$wpm <- c(wpm$wpm, typing()$wpm)
  })

  prompt <- reactive({
    typing_reset()
    sample(stringr::sentences, n_prompt)
  })

  output$prompt <- renderUI({
    tags$blockquote(lapply(prompt(), tags$p))
  })

  has_stringdist <- requireNamespace("stringdist", quietly = TRUE)
  if (!has_stringdist) {
    warning(
      "Install `stringdist` to get typing errors: install.packages('stringdist')",
      immediate. = TRUE, call. = FALSE
    )
  }

  output$wpm <- renderUI({
    req(typing()$wpm)
    wpm_class <- paste(
      "wpm",
      if (typing()$wpm < 40) {
        "text-danger"
      } else if (typing()$wpm < 75) {
        "text-warning"
      } else {
        "text-success"
      }
    )
    tagList(
      div(
        class = if (!has_stringdist) "col-xs-12" else "col-xs-6 col-sm-12",
        tags$h2(
          class = wpm_class,
          round(typing()$wpm, 2), "wpm"
        )
      ),
      if (has_stringdist) div(
        class = "col-xs-6 col-sm-12",
        tags$h2(
          class = "errors",
          stringdist::stringdist(
            substring(
              paste(prompt(), collapse = "\n"),
              1,
              nchar(typing()$text)
            ),
            typing()$text
          ),
          "errors"
        )
      )
    )
  })

  return(reactive(reactive_df(wpm)))
}
