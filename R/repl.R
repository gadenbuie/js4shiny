get_example_file_paths <- function(path) {
  path_files <- dir(path, full.names = TRUE)
  file_info <- purrr::map(path_files, extract_yaml)
  names(path_files) <- file_info %>%
    purrr::map("example") %>%
    purrr::map_chr("title")
  path_files
}

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

blank_example <- function() {
  c("Blank" = file.path(js4shiny_file("repl", "0-empty.Rmd")))
}

repl_ui_code <- function(css = TRUE, md = TRUE, ...) {
  repl_js <- shinyAce::aceEditor(
    "code_js",
    mode = "javascript",
    debounce = 1000,
    height = "100%",
    ...
  )

  include_css <- is.character(css) || isTRUE(css)
  include_md <- is.character(md) || isTRUE(md)

  if (!include_css && !include_md) {
    return(repl_js)
  }

  shiny::tabsetPanel(
    id = "panel-code-tabset",
    shiny::tabPanel(
      "JS",
      repl_js
    ),
    if (include_css) shiny::tabPanel(
      "CSS",
      shinyAce::aceEditor(
        "code_css",
        value = if (is.character(css)) css else "",
        mode = "css",
        debounce = 1000,
        height = "100%",
        ...
      )
    ),
    if (include_md) shiny::tabPanel(
      "HTML/Markdown",
      shinyAce::aceEditor(
        "code_md",
        value = if (is.character(md)) md else "",
        mode = "markdown",
        debounce = 1000,
        height = "100%",
        ...
      )
    )
  )
}

repl_ui <- function(examples = NULL, js_repl_only = FALSE) {
  shiny::addResourcePath("repl", js4shiny_file("repl"))
  shiny::addResourcePath("redirect", js4shiny_file("redirect"))

  example_file_choices <- c(blank_example(), get_example_file_paths(examples))

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(href = "repl/repl.css", rel = "stylesheet", type = "text/css"),
      shiny::tags$link(href = "redirect/jslog.css", rel = "stylesheet", type = "text/css"),
      shiny::tags$script(src = "repl/repl.js")
    ),
    class = if (js_repl_only) "hide-navbar",
    shiny::tags$nav(
      class = "navbar navbar-default",
      shiny::div(
        class = "container-fluid",
        shiny::div(
          class = "navbar-header",
          shiny::div(
            class = "navbar-brand",
            "js4shiny::repl()",
            shiny::span(class = "loader")
          )
        ),
        shiny::tags$form(
          class = "navbar-form navbar-right",
          shiny::tags$button(
            id = "show_solution",
            class = "btn btn-default action-button btn-primary pull-right shiny-bound-input",
            style = "display: none",
            "Show Solution"
          ),
          shiny::selectInput("example", NULL, example_file_choices, selectize = FALSE)
        )
      )
    ),
    shiny::div(
      class = paste0(
        "full-height-container",
        if (js_repl_only) " hide-html-preview"
      ),
      shiny::div(
        class = "panel-code",
        shiny::div(
          class = paste0(
            "panel-code-js",
            if(js_repl_only) " panel-code-js__repl-only"
          ),
          repl_ui_code(
            css = !js_repl_only,
            md = !js_repl_only,
            theme = "textmate",
            wordWrap = TRUE,
            autoComplete = "live",
            tabSize = 4
          )
        ),
        shiny::div(
          class = "panel-code-js-console",
          shiny::div(
            class = "btn-group console__buttons",
            if (!js_repl_only) shiny::tags$button(
              id = "hide-log",
              class = "btn btn-default btn-sm",
              "Hide"
            ),
            shiny::tags$button(
              id = "clear-log",
              class = "btn btn-default btn-sm",
              "Clear"
            )
          ),
          shiny::tags$pre(id = "log")
        )
      ),
      shiny::div(
        class = "panel-html",
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
      js <- input$code_js
      css <- input$code_css
      md <- input$code_md %||% ""

      # create rmd_file from input md
      rmd_file <- tempfile(fileext = ".Rmd")
      cat(glue("
        ---
        output: js4shiny::html_document_plain
        ---

        {md}
        "
      ), file = rmd_file)

      # create js file from input code
      js_file <- file.path(render_dir, paste0("script_", session$token, ".js"))
      cat(js, file = js_file, sep = "\n")

      # create css file from input css
      if (!is.null(css)) {
        css_file <- file.path(render_dir, paste0("style_", session$token, ".css"))
        cat(css, file = css_file, sep = "\n")
      } else {
        css_file <- NULL
      }

      html_out_file <- file.path("render", paste0(session$token, ".html"))
      html_out_file_abs <- file.path(render_dir, paste0(session$token, ".html"))

      res <- list(file = html_out_file)
      tryCatch({
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
            css = c(
              "normalize",
              js4shiny_file("repl", "repl-child.css"),
              css_file
            ),
            self_contained = TRUE,
            pandoc_args = c(
              "--to",
              "html5"
            )
          )
        )
      },
      warning = function(w) {
        res$warning <<- w$message
      },
      error = function(e) {
        res$error <<- e$message
      }
      )

      if (!file.exists(html_out_file_abs)) res$file <- NULL
      res
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
      I("Show/Hide Btn: Show Solution")
      state <- !is.null(solution())
      session$sendCustomMessage("showSolutionButton", state)
    })

    shiny::observe({
      I("Set HTML/MD to Selected Example")
      shinyAce::updateAceEditor(
        session, "code_md",
        value = paste(readLines(input$example), collapse = "\n")
      )
    }, priority = 1000)

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
      out_html <- compiled_html()

      shiny::tagList(
        if (!is.null(out_html$error)) {
          shiny::tags$div(
            class = "panel-html__error alert alert-danger",
            shiny::tags$strong("Error:"),
            out_html$error
          )
        },
        if (!is.null(out_html$file)) shiny::tags$iframe(
          style = "overflow:hidden;overflow-scroll:auto;height:100%;width:100%",
          # class = "vh-90",
          class = "outputHTML",
          width = "100%",
          height = "100%",
          src = out_html$file
        )
      )
    })
  }
}

repl <- function(examples = NULL, render_dir = NULL, js_repl_only = FALSE) {
  if (is.null(render_dir)) {
    render_dir <- file.path(tempdir(), "repl_render")
  }
  shiny::shinyApp(
    ui = repl_ui(examples, js_repl_only),
    server = repl_server(render_dir)
  )
}

repl_js <- function(render_dir = NULL, ...) {
  repl(js_repl_only = TRUE, render_dir = render_dir, ...)
}
