#' REPL for live JS, CSS, and HTML development
#'
#' Launches an interactive Shiny app for live editing of frontend JavaScript,
#' CSS, and HTML/Markdown/R Markdown. The app allows users to write JS, CSS and
#' HTML, preview the final product, observe the JavaScript console (specifically
#' items printed to the console via `console.log()`), and download a zip file
#' containing the source files.
#'
#' @section Examples for js4shiny workshop:
#' The app was developed for the **js4shiny** rstudio::conf workshop and can be
#' used to load examples for practicing and learning JavaScript and web
#' development concepts.
#'
#' TODO: include details on how to load examples.
#'
#' @param examples Path to folder containing examples
#' @param render_dir Where to render temporary files, defaults to `tempdir()`
#' @param js_repl_only When `TRUE`, the app is simplified to contain only a
#'   JavaScript source editor and a console output. `repl_js()` is an alias to
#'   launch `repl()` with `js_repl_only = TRUE`.
#' @param theme_app The theme of the app, using \pkg{shinythemes}. See
#'   [shinythemes::shinytheme()] for a list of valid themes.
#' @param theme_editor The theme of the \pkg{shinyAce} source code editors. See
#'   [shinyAce::getAceThemes()] for a list of valid themes.
#' @param ... Arguments passed from `repl_js()` to `repl()` or from `repl()` to
#'   [shiny::shinyApp()].
#'
#' @return A shiny app
#' @export
repl <- function(
  examples = NULL,
  js_repl_only = FALSE,
  theme_app = NULL,
  theme_editor = "textmate",
  render_dir = NULL,
  ...
) {
  if (is.null(render_dir)) {
    render_dir <- file.path(tempdir(), "repl_render")
  }
  if (!js_repl_only) repl_show_disclaimer()
  shiny::shinyApp(
    ui = repl_ui(examples, js_repl_only, theme_app = theme_app, theme_editor = theme_editor),
    server = repl_server(render_dir),
    ...
  )
}

#' @rdname repl
#' @export
repl_js <- function(render_dir = NULL, ...) {
  repl(js_repl_only = TRUE, render_dir = render_dir, ...)
}



get_example_file_paths <- function(path = NULL) {
  path %||% return()
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

remove_yaml <- function(text) {
  if (length(text) == 1 || !grepl('\n', text)) {
    text <- readLines(text)
  }
  yaml_between <- grep("^---\\s*", text)[1:2]
  text[-(yaml_between[1]:yaml_between[2])]
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

repl_ui <- function(
  examples = NULL,
  js_repl_only = FALSE,
  theme_app = NULL,
  theme_editor = "textmate"
) {
  shiny::addResourcePath("repl", js4shiny_file("repl"))
  shiny::addResourcePath("redirect", js4shiny_file("redirect"))

  example_file_choices <- c(blank_example(), get_example_file_paths(examples))

  function(request) {
    shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$link(href = "repl/repl.css", rel = "stylesheet", type = "text/css"),
        shiny::tags$link(href = "redirect/jslog.css", rel = "stylesheet", type = "text/css"),
        shiny::tags$script(src = "repl/repl.js")
      ),
      class = if (js_repl_only) "hide-navbar",
      theme = if (!is.null(theme_app)) {
        requires_pkg("shinythemes")
        shinythemes::shinytheme(theme_app)
      },
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
            shiny::div(
              class = "form-group",
              shiny::tags$button(
                id = "show_solution",
                class = "btn btn-default action-button btn-primary shiny-bound-input",
                style = "display: none",
                `aria-label` = "Show Solution to Exercise",
                title = "Show Solution to Exercise",
                "Show Solution"
              ),
              shiny::selectInput(
                inputId = "example",
                label = NULL,
                choices = example_file_choices,
                selected = example_file_choices[min(length(example_file_choices), 2)],
                selectize = FALSE,
                width = "250px"
              ),
              shiny::tags$button(
                id = "do_save",
                class = "btn btn-default action-button shiny-bound-input",
                `aria-label` = "Save Project",
                title = "Save Project",
                shiny::icon("floppy-o")
              )
            )
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
              if (js_repl_only) " panel-code-js__repl-only"
            ),
            repl_ui_code(
              css = !js_repl_only,
              md = !js_repl_only,
              theme = theme_editor,
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
                `aria-label` = "Hide Console Log",
                title = "Hide Console Log",
                "Hide"
              ),
              shiny::tags$button(
                id = "clear-log",
                class = "btn btn-default btn-sm",
                `aria-label` = "Clear Console Log",
                title = "Clear Console Log",
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
}

repl_server <- function(render_dir) {

  dir.create(render_dir, showWarnings = FALSE)
  shiny::addResourcePath("render", render_dir)

  function(input, output, session) {
    `%||%` <- function(x, y) if (is.null(x)) y else x

    shiny::setBookmarkExclude(repl_exclude_bookmark())

    shiny::observe({
      shiny::req(input$repl_debug == "gubed")
      browser()
    })

    shiny::onRestored(function(state) {
      shinyAce::updateAceEditor(session, "code_js", state$input$code_js)
      if (!is.null(state$input$code_css)) {
        shinyAce::updateAceEditor(session, "code_css", state$input$code_css)
      }
      if (!is.null(state$input$code_md)) {
        shinyAce::updateAceEditor(session, "code_md", state$input$code_md)
      }
    })

    shiny::observe({
      shiny::reactiveValuesToList(input)
      session$doBookmark()
    })
    shiny::onBookmarked(function(url) {
      shiny::updateQueryString(url)
    })

    compiled_html <- shiny::reactive({
      input$refresh_html

      js  <- input$code_js
      css <- input$code_css
      md  <- input$code_md %||% ""

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
            script = include_script(
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

      if (!file.exists(html_out_file_abs)) {
        res$file <- NULL
      } else {
        session$sendCustomMessage("clearElementById", "log")
      }
      res
    })

    example_yaml <- shiny::reactive({
      input$example %||% return(NULL)
      extract_yaml(input$example)$example
    })

    shiny::observeEvent(input$`clear-source`, {
      shinyAce::updateAceEditor(session, "code", value = "")
    })

    solution <- shiny::reactive({
      example_yaml()$solution %||% NULL
    })

    shiny::observeEvent(input$show_solution, {
      shiny::req(solution()$js)
      shinyAce::updateAceEditor(session, "code_js", value = solution()$js)
    })

    shiny::observeEvent(input$show_solution, {
      shiny::req(solution()$css)
      shinyAce::updateAceEditor(session, "code_css", value = solution()$css)
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
        value = paste(remove_yaml(input$example), collapse = "\n")
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

    html_path <- shiny::reactiveFileReader(
      intervalMillis = 1000,
      session = session,
      filePath = shiny::reactive({compiled_html()$file}),
      readFunc = function(path) return(path)
    )

    output$example_html <- shiny::renderUI({
      out_html <- compiled_html()
      html_path() # poll file in case compiled_html() misses an update

      shiny::tagList(
        if (!is.null(out_html$error)) {
          shiny::tags$div(
            class = "panel-html__error alert alert-danger",
            shiny::tags$strong("Error:"),
            out_html$error
          )
        },
        if (!is.null(out_html$file)) shiny::div(
          class = "panel-html__output",
          shiny::div(
            class = "panel-html__options btn-group",
            shiny::tags$button(
              id = "refresh_html",
              class = "btn btn-default btn-sm action-button shiny-bound-input",
              `aria-label` = "Refresh Preview",
              title = "Refresh Preview",
              shiny::icon("refresh")
            ),
            shiny::tags$a(
              id = "open_external_preview",
              class = "btn btn-default btn-sm",
              href = out_html$file,
              target = "_blank",
              `aria-label` = "Open External Preview",
              title = "Open External Preview",
              shiny::icon("external-link-alt")
            )
          ),
          shiny::tags$iframe(
            style = "overflow:hidden;overflow-scroll:auto;height:100%;width:100%",
            # class = "vh-90",
            class = "outputHTML",
            width = "100%",
            height = "100%",
            src = out_html$file
          )
        )
      )
    })

    shiny::observeEvent(input$do_save, {
      repl_save(example_yaml())
    })

    output$download_project <- shiny::downloadHandler(
      filename = function() {
        if (input$save_format == "zip") {
          "project.zip"
        } else {
          "example.Rmd"
        }
      },
      content = function(file) {
        if (input$save_format == "zip") {
          create_project_zip(
            js = input$code_js,
            css = input$code_css,
            md = input$code_md,
            token = session$token,
            out_file = file
          )
        } else if (input$save_format == "example") {
          create_example_rmd(
            title = input$save_example_title,
            instructions = input$save_example_instructions,
            hint = input$save_example_hint,
            js = if ("js" %in% input$save_example_include_solution) {
              input$code_js
            },
            css = if ("css" %in% input$save_example_include_solution) {
              input$code_css
            },
            md = input$code_md,
            output_file = file
          )
        } else if (input$save_format == "rmd") {
          create_plain_rmd(
            js = input$code_js,
            css = input$code_css,
            md = input$code_md,
            output_file = file
          )
        }
        shiny::removeModal()
      }
    )
  }
}

repl_save <- function(example_yaml) {
  shiny::showModal(
    shiny::modalDialog(
      title = "Save Project",
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::downloadButton("download_project", "Save")
      ),
      shiny::radioButtons(
        inputId = "save_format",
        label = "Download as...",
        inline = TRUE,
        choices = c(
          "Zip file with HTML, CSS and JS" = "zip",
          "Plain R Markdown" = "rmd",
          "js4shiny Example" = "example"
        )
      ),
      shiny::conditionalPanel(
        condition = "input.save_format === 'example'",
        shiny::textInput(
          inputId = "save_example_title",
          label = "Title",
          width = "100%",
          value = example_yaml$title %||% ""
        ),
        shiny::div(
          class = "textarea-monospace constrain-textarea-width",
          shiny::textAreaInput(
            inputId = "save_example_instructions",
            label = "Instructions",
            width = "100%",
            value = example_yaml$instructions %||% ""
          ),
          shiny::textAreaInput(
            inputId = "save_example_hint",
            label = "Hint",
            width = "100%",
            value = example_yaml$hint %||% ""
          )
        ),
        shiny::checkboxGroupInput(
          inputId = "save_example_include_solution",
          label = "Include as \"Solution\"",
          choices = c("JavaScript" = "js", "CSS" = "css"),
          selected = c("js", "css"),
          inline = TRUE,
          width = "100%"
        )
      )
    )
  )
}

write_temp_files <- function(
  js = "",
  css = "",
  md = "",
  destdir = tempfile(),
  yaml_list = list(output = "js4shiny::html_document_plain")
) {
  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)

  # file names
  js_file   <- file.path(destdir, "script.js")
  css_file  <- file.path(destdir, "style.css")
  md_file   <- file.path(destdir, "index.md")

  # write into file names
  writeLines(js %||% "", js_file)
  writeLines(css %||% "", css_file)
  md <- glue("
    ---
    output: js4shiny::html_document_plain
    ---

    {md %||% ''}
    "
  )
  writeLines(md %||% "", md_file)

  destdir
}

create_project_zip <- function(js = "", css = "", md = "", token = NULL, out_file = "project.zip") {
  destdir <- if (!is.null(token)) {
    file.path(tempdir(), token)
  } else {
    tempfile()
  }

  write_temp_files(js, css, md, destdir)

  md_file   <- file.path(destdir, "index.md")
  html_file <- file.path(destdir, "index.html")

  rmarkdown::render(
    input = md_file,
    output_file = html_file,
    quiet = TRUE,
    output_options = list(
      self_contained = FALSE,
      script = include_script(after = "script.js"),
      css = "style.css"
    )
  )

  # zip up!
  old_wd <- setwd(destdir)
  zip::zipr(
    zipfile = out_file,
    files = c("index.html", "script.js", "style.css")
  )
  setwd(old_wd)
}

default_example_value <- function(x, default = NULL, single = FALSE) {
  xn <- substitute(x)
  x <- if (is.null(x) || x == "") default else x
  if (is.null(x)) return(x)
  if (single && (length(x) > 1 || any(grepl("\n", x)))) {
    warning("`", xn, "` was coerced to a single line", call. = FALSE)
    x <- paste(gsub("\n", " ", x), collapse = " ")
  }
  paste(x, collapse = "\n")
}

create_example_rmd <- function(
  title = "Example",
  instructions = NULL,
  hint = NULL,
  js = NULL,
  css = NULL,
  md = NULL,
  output_file = "example.Rmd"
) {
  example <- list(
    title = default_example_value(title, "Example", single = TRUE),
    instructions = default_example_value(instructions),
    hint = default_example_value(hint),
    solution = list(
      js   = default_example_value(js),
      css  = default_example_value(css)
    )
  )

  yaml_header <- list(
    example = example,
    output = "js4shiny::html_document_plain"
  )

  md <- glue("
    ---
    {yaml::as.yaml(yaml_header)}
    ---

    {default_example_value(md, '')}
    "
  )

  cat(md, file = output_file)
  invisible(output_file)
}

create_plain_rmd <- function(
  js = NULL,
  css = NULL,
  md = NULL,
  output_file = "example.Rmd"
) {
  js <- js %||% ""
  css <- css %||% ""
  md <- md %||% ""

  out_txt <- glue("
  ---
  output: html_document
  ---

  {md}

  ```{{js, echo=FALSE}}
  {js}
  ````

  ```{{css, echo=FALSE}}
  {css}
  ```
  ")

  cat(out_txt, file = output_file)
  invisible(output_file)
}

repl_show_disclaimer <- function() {
  if (!isTRUE(getOption("js4shiny.repl_disclaimer", FALSE))) {
    warning(
      "js4shiny::repl() IS INTENDED FOR LOCAL TESTING ONLY!\n",
      "The repl environment renders R Markdown and if deployed publicly, ",
      "will allow any user to execute arbitrary R code on your server. ",
      "(This message is shown once per session.)\n",
      call. = FALSE,
      immediate. = TRUE
    )
    options(js4shiny.repl_disclaimer = TRUE)
  }
}

repl_exclude_bookmark <- function() {
  c(
    "code_css_shinyAce_annotationTrigger",
    "code_js_shinyAce_annotationTrigger",
    "code_js_shinyAce_tooltipItem",
    "code_md_shinyAce_annotationTrigger",
    "do_save",
    "example",
    "panel-code-tabset",
    "refresh_html",
    "repl_debug",
    "show_solution"
  )
}
