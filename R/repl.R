# library(shiny)
# source("R/example_files.R")

# example_file_choices <- get_example_file_paths()

get_instructions <- function(path) {
  extract_yaml(path)$example$instructions
}

get_hint <- function(path) {
  extract_yaml(path)$example$hint
}

extract_yaml <- function(path) {
  x <- readLines(path)
  yaml_between <- grep("^---\\s*", x)[1:2] # assume first two ---
  yaml::yaml.load(x[(yaml_between[1] + 1):(yaml_between[2] - 1)])
}

repl_ui <- function(examples = NULL) {
  shiny::addResourcePath("repl", js4shiny_file("repl"))
  shiny::addResourcePath("redirect", js4shiny_file("redirect"))

  if (is.null(examples)) {
    example_file_choices <- file.path(js4shiny_file("repl", "0-empty.Rmd"))
  }

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(href = "repl/repl.css", rel = "stylesheet", type = "text/css"),
      shiny::tags$link(href = "redirect/jslog.css", rel = "stylesheet", type = "text/css"),
      shiny::tags$script(src = "repl/repl.js")
    ),
    shiny::tags$nav(
      class = "navbar navbar-default",
      shiny::div(
        class = "container-fluid",
        shiny::div(
          class = "navbar-header",
          shiny::div(class = "navbar-brand", "js4shiny::repl()")
        ),
        shiny::tags$form(
          class = "navbar-form navbar-right",
          shiny::selectInput("example", NULL, example_file_choices, selectize = FALSE)
        )
      )
    ),
    shiny::div(
      class = "full-height-container",
      shiny::div(
        id = "col-js",
        shinyAce::aceEditor(
          "code",
          mode = "javascript",
          debounce = 3000,
          height = "100%"
        ),
        shiny::div(
          shiny::span(
            shiny::icon("trash"),
            style = "margin-right: 5px"
          ),
          shiny::div(
            class = "btn-group",
            role = "group",
            shiny::actionButton("clear-source", "Clear Source"),
            shiny::tags$button(
              id = "clear-log",
              class = "btn btn-default",
              "Clear Log"
            )
          ),
          shiny::tags$button(
            id = "show_solution",
            class = "btn btn-default action-button btn-primary pull-right shiny-bound-input",
            style = "display: none",
            "Show Solution"
          )
        ),
        shiny::tags$pre(id = "log")
      ),
      shiny::div(
        id = "col-html",
        shiny::uiOutput("instructions"),
        shiny::uiOutput("hint"),
        shiny::uiOutput("example_html")
      )
    )
  )
}

repl_server <- function(render_dir) {

  dir.create(render_dir, showWarnings = FALSE)
  shiny::addResourcePath("render", render_dir)

  function(input, output, session) {
    `%||%` <- function(x, y) if (is.null(x)) y else x

    compiled_html <- shiny::reactive({
      js <- input$code
      rmd_file <- input$example

      # create js file from input code
      js_file <- file.path(render_dir, paste0("script_", session$token, ".js"))
      cat(js, file = js_file, sep = "\n")

      html_out_file <- file.path("render", paste0(session$token, ".html"))
      html_out_file_abs <- file.path(render_dir, paste0(session$token, ".html"))

      rmarkdown::render(
        input = rmd_file,
        output_file = html_out_file_abs,
        quiet = TRUE,
        output_options = list(
          script = js4shiny:::include_script(
            before = c(
              js4shiny_file("redirect", "redirectConsoleLog.js"),
              js4shiny_file("repl", "repl-child-redirect.js")
            ),
            after = c(
              js_file
            )
          ),
          css = c("normalize", js4shiny_file("repl", "repl-child.css")),
          self_contained = TRUE,
          pandoc_args = c(
            "--to",
            "html5"
          )
        )
      )

      html_out_file
    })

    example_yaml <- shiny::reactive({
      input$example %||% return(NULL)
      extract_yaml(input$example)$example
    })

    shiny::observeEvent(input$`clear-source`, {
      shinyAce::updateAceEditor(session, "code", value = "")
    })

    shiny::observeEvent(input$show_solution, {
      shinyAce::updateAceEditor(session, "code", value = solution())
    })

    solution <- shiny::reactive({
      example_yaml()$solution %||% NULL
    })

    shiny::observe({
      state <- !is.null(solution())
      session$sendCustomMessage("showSolutionButton", state)
    })

    output$instructions <- shiny::renderUI({
      shiny::req(example_yaml())
      instr <- example_yaml()$instructions
      shiny::req(instr)
      shiny::tagList(
        shiny::div(
          class = "bs-callout bs-callout-default instructions",
          shiny::HTML(commonmark::markdown_html(instr))
        )
      )
    })

    output$hint <- shiny::renderUI({
      shiny::req(example_yaml())
      hint <- example_yaml()$hint
      shiny::req(hint)
      shiny::tagList(
        shiny::div(
          class = "bs-callout bs-callout-info",
          shiny::HTML(commonmark::markdown_html(hint))
        )
      )
    })

    output$example_html <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$iframe(
          style = "overflow:hidden;overflow-scroll:auto;height:100%;width:100%",
          # class = "vh-90",
          class = "outputHTML",
          width = "100%",
          height = "100%",
          src = compiled_html()
        )
      )
    })
  }
}

repl <- function(examples = NULL, render_dir = NULL) {
  if (is.null(render_dir)) {
    render_dir <- file.path(tempdir(), "repl_render")
  }
  shiny::shinyApp(ui = repl_ui(examples), server = repl_server(render_dir))
}
