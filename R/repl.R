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
#' @param example Path to folder containing example or to an example Rmd file.
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
  example = NULL,
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
    ui = repl_ui(example, js_repl_only, theme_app = theme_app, theme_editor = theme_editor),
    server = repl_server(render_dir),
    ...
  )
}

#' @rdname repl
#' @export
repl_js <- function(..., render_dir = NULL) {
  repl(js_repl_only = TRUE, render_dir = render_dir, ...)
}

get_example_file_paths <- function(path = NULL) {
  path %||% return()
  if (length(path) != 1) {
    stop("Please provide a single path to a directory or file", call. = FALSE)
  }
  if (fs::is_dir(path)) {
    path_files <- fs::dir_ls(path, regexp = "[.][Rr][Mm][Dd]")
  } else {
    path_files <- path
  }
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
  if (!sum(grepl("^---", x)) >= 2) return()
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

extract_resources <- function(path) {
  # path is a file path or previously processed yaml list
  if (is.null(path)) {
    return()
  } else if (is.character(path)) {
    yml <- extract_yaml(path)
  } else if (is.list(path)) {
    yml <- path
  }

  # if no resources, return NULL
  if (is.null(yml)) return()
  if (is.null(yml$output)) return()
  if (is.character(yml$output)) return()
  resources <- yml$output[['js4shiny::html_document_plain']]
  if (is.null(resources) || is.character(resources)) return()
  resources <- resources[c("css", "script")]
  names(resources) <- c("css", "javascript")
  resources <- purrr::compact(resources)
  if (!length(resources)) return()

  res <- list()
  if ("css" %in% names(resources)) {
    res <- c(
      res,
      purrr::map(
        resources$css,
        ~ list(path = .x, type = "css", where = "head")
      )
    )
  }

  if ("javascript" %in% names(resources)) {
    for (where in names(resources$javascript)) {
      res <- c(res, purrr::map(
        resources$javascript[[where]],
        ~ list(path = .x, type = "javascript", where = where)
      ))
    }
  }

  res
}

resource_to_js4shiny_yaml <- function(resources) {
  # does the reverse of extract_resources
  purrr::reduce(
    resources,
    .init = list(css = c(), script = list()),
    function(acc, item) {
      if (item$type == "css") {
        acc$css <- c(acc$css, item$path)
      }
      if (item$type == "javascript") {
        acc$script[[item$where]] <- c(acc$script[[item$where]], item$path)
      }
      acc
    })
}

resource_add_ids <- function(resources) {
  if (is.null(resources)) return(list())
  if (!length(resources)) return(list())
  for (i in seq_along(resources)) {
    resources[[i]]$id <- resources[[i]]$id %||% rand_id()
  }
  ids <- purrr::map_chr(resources, ~ .$id)
  names(resources) <- ids
  resources
}

map_name_into_list <- function(ll) {
  ll <- purrr::imap(ll, function(n, p) {
    purrr::map(p, ~ list(path = .x, where = n))
  })

  purrr::reduce(ll, c)
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
      value = "js",
      repl_js
    ),
    if (include_css) shiny::tabPanel(
      "CSS",
      value = "css",
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
      "R Markdown",
      value = "md",
      shinyAce::aceEditor(
        "code_md",
        value = if (is.character(md)) md else "",
        mode = "markdown",
        debounce = 1000,
        height = "100%",
        ...
      )
    ),
    if (include_css | include_md) shiny::tabPanel(
      title = NULL,
      value = "settings",
      icon = shiny::icon("gear"),
      shiny::div(
        class = "tab-settings",
        shiny::div(
          class = "scale--smaller",
          shiny::selectInput("md_format", "Document Mode", choices = c("R Markdown" = "md", "HTML" = "html")),
          # shiny::selectInput("css_format", "Styles", choices = c("CSS" = "css", "Sass" = "sass")),
          shiny::tags$div(
            class = "col-xs-12",
            includeExtrasUI("extras")
          )
        )
      )
    )
  )
}

repl_ui <- function(
  example = NULL,
  js_repl_only = FALSE,
  theme_app = NULL,
  theme_editor = "textmate"
) {
  shiny::addResourcePath("repl", js4shiny_file("repl"))
  shiny::addResourcePath("redirect", js4shiny_file("redirect"))

  example_file_choices <- c(blank_example(), get_example_file_paths(example))

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
              tabSize = 2,
              useSoftTabs = TRUE
            )
          ),
          shiny::div(
            class = paste(
              "resize-handle",
              if (js_repl_only) "resize--horizontal" else "resize--vertical")
          ),
          shiny::div(
            class = "panel-code-js-console",
            if (js_repl_only) shiny::uiOutput("instructions"),
            if (js_repl_only) shiny::uiOutput("hint"),
            shiny::div(
              class = "log-container",
              shiny::div(
                class = "btn-group console__buttons",
                if (!js_repl_only) shiny::tags$button(
                  id = "hide-log",
                  class = "btn btn-default btn-sm",
                  `aria-label` = "Hide Console Log",
                  title = "Hide Console Log",
                  shiny::icon("window-minimize")
                ),
                shiny::tags$button(
                  id = "clear-log",
                  class = "btn btn-default btn-sm",
                  `aria-label` = "Clear Console Log",
                  title = "Clear Console Log",
                  shiny::icon("backspace")
                )
              ),
              shiny::tags$pre(id = "log")
            )
          )
        ),
        if (!js_repl_only) shiny::div(
          class = "resize-handle resize--horizontal__top",
          shiny::HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 36 36"><path d="M20 9H4v2h16V9zM4 15h16v-2H4v2z"></path></svg>')
        ),
        shiny::div(
          class = "panel-html",
          if (!js_repl_only) shiny::uiOutput("instructions"),
          if (!js_repl_only) shiny::uiOutput("hint"),
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

    example_cache <- list()

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

    example_yaml <- shiny::reactive(label = "yaml from selected example", {
      input$example %||% return(NULL)
      extract_yaml(input$example)$example %>%
        purrr::map(purrr::compact) %>%
        purrr::compact()
    })

    example_resources <- shiny::reactiveValues(files = list())

    extra_resources <- includeExtras("extras", files = example_resources)

    skip_compile <- shiny::reactiveVal(FALSE)

    #---- Compile HTML Preview ----
    compiled_html <- shiny::reactive(label = "compiled_html", {
      I("compile HTML")
      input$refresh_html

      js  <- input$code_js
      css <- input$code_css
      md  <- input$code_md %||% ""

      html_out_file <- file.path("render", paste0(session$token, ".html"))
      html_out_file_abs <- file.path(render_dir, paste0(session$token, ".html"))

      res <- list(file = html_out_file)

      if (shiny::isolate(skip_compile())) {
        skip_compile(FALSE)
        return(res)
      }

      extra_css <- extra_resources() %>%
        purrr::keep(~ .$type == "css") %>%
        purrr::map_chr("path")
      extra_js <- extra_resources() %>%
        purrr::keep(~ .$type == "javascript") %>%
        purrr::reduce(.init = list(), function(acc, item) {
          acc[[item$where]] <- c(acc[[item$where]], item$path)
          acc
        })

      # Cache current values of inputs
      example_cache[[shiny::isolate(input$example)]] <<- list(
        js = input$code_js,
        css = input$code_css,
        md = input$code_md,
        resources = extra_resources()$files
      )

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

      tryCatch({
        rmarkdown::render(
          input = rmd_file,
          output_file = html_out_file_abs,
          quiet = getOption("js4shiny.debug.repl", FALSE),
          output_options = list(
            script = include_script(
              head = extra_js$head,
              before = c(
                extra_js$before,
                js4shiny_file("redirect", "redirectConsoleLog.js"),
                js4shiny_file("repl", "repl-child-redirect.js")
              ),
              after = c(
                extra_js$after,
                js_file
              )
            ),
            css = c(
              "normalize",
              if (!length(extra_css)) c(
                js4shiny_file("repl", "repl-child.css")
              ) else {
                extra_css
              },
              css_file
            ),
            self_contained = TRUE
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

    shiny::observeEvent(input$`clear-source`, {
      shinyAce::updateAceEditor(session, "code", value = "")
    })

    solution <- shiny::reactive({
      ex_yml <- example_yaml()
      solution <- ex_yml$solution
      initial <- ex_yml$initial
      current <- list(
        js  = if (!is.null(input$code_js))  trimws(input$code_js),
        css = if (!is.null(input$code_css)) trimws(input$code_css)
      )
      if (!is.null(current$js)  && current$js  == "") current$js  <- NULL
      if (!is.null(current$css) && current$css == "") current$css <- NULL

      # Use solution > current code > initial code > NULL
      # Current code is included here in case the example has an initial state
      # in which case, we'd prefer not to overwrite the current original code
      solution$js <- solution$js %||% current$js %||% initial$js %||% NULL
      solution$css <- solution$css %||% current$css %||% initial$css %||% NULL

      solution
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
      I("Update Editors to Selected Example")
      shiny::req(example_yaml())
      # str(example_yaml())

      this_example <- shiny::isolate(input$example)

      # Get cached inputs
      cache <- example_cache[[this_example]] %>%
        purrr::compact() %>%
        purrr::keep(~ . != "")

      shinyAce::updateAceEditor(
        session,
        "code_js",
        value = paste(
          cache$js %||% example_yaml()$initial$js %||% "",
          collapse = "\n"
        )
      )

      shinyAce::updateAceEditor(
        session,
        "code_css",
        value = paste(
          cache$css %||% example_yaml()$initial$css %||% "",
          collapse = "\n"
        )
      )

      shinyAce::updateAceEditor(
        session, "code_md",
        value = paste(
          cache$md %||% remove_yaml(this_example),
          collapse = "\n"
        )
      )
      example_resources$files <- cache$resources %||%
        extract_resources(this_example %||% return(NULL))
      skip_compile(TRUE)
    }, priority = 1000, label = "update editors based on example")

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
      filePath = file.path("render", paste0(session$token, ".html")),
      readFunc = function(path) {
        if (!file.exists(path)) {
          cat(
            "<html><body>",
            "<p style='color:#999;'>Hang on while I get this started...</p>",
            "</body></html>",
            sep = "\n",
            file = file.path(render_dir, paste0(session$token, ".html"))
          )
        }
        path
      }
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

    # ---- Settings ----
    shiny::observe({
      shiny::req(input$md_format)
      fmt_label <- switch(input$md_format, md = "R Markdown", html = "HTML")
      fmt_mode <- switch(input$md_format, md = "markdown", html = "html")
      session$sendCustomMessage("updateTabName", list(
        id = "panel-code-tabset",
        value = "md",
        replacement = fmt_label
      ))
      shinyAce::updateAceEditor(
        session = session,
        editorId = "code_md",
        mode = fmt_mode
      )
    }, label = "change md/html editor format")

    # ---- Saving Current Document ----
    shiny::observeEvent(input$do_save, {
      repl_save(example_yaml(), length(extra_resources()) > 0)
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
            resources = extra_resources(),
            out_file = file
          )
        } else if (input$save_format == "example") {
          create_example_rmd(
            title = input$save_example_title,
            instructions = input$save_example_instructions,
            hint = input$save_example_hint,
            initial = list(
              js = if (input$save_example_location_js %in% c("both", "initial")) {
                input$code_js
              },
              css = if (input$save_example_location_css %in% c("both", "initial")) {
                input$code_css
              }
            ),
            solution = list(
              js = if (input$save_example_location_js %in% c("both", "solution")) {
                input$code_js
              },
              css = if (input$save_example_location_css %in% c("both", "solution")) {
                input$code_css
              }
            ),
            md = input$code_md,
            resources = extra_resources(),
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

repl_save <- function(example_yaml, has_extra_resources = FALSE) {
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
      if (has_extra_resources) shiny::conditionalPanel(
        condition = "input.save_format === 'rmd'",
        shiny::helpText(shiny::HTML(
          "This format is a plain <code>rmarkdown::html_document()</code>",
          "and will not include the extra resources. Please instead download",
          "the project as a zip file."
        ))
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
        shiny::fluidRow(
          shiny::div(
            class = "col-xs-6",
            shiny::selectInput(
              inputId = "save_example_location_js",
              label = "Include JavaScript as",
              choices = c("Initial" = "initial", "Solution" = "solution", "Both" = "both"),
              selected = "solution",
              selectize = FALSE
            )
          ),
          shiny::div(
            class = "col-xs-6",
            shiny::selectInput(
              inputId = "save_example_location_css",
              label = "Include CSS as",
              choices = c("Initial" = "initial", "Solution" = "solution", "Both" = "both"),
              selected = "solution",
              selectize = FALSE
            )
          )
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

create_project_zip <- function(
  js = "",
  css = "",
  md = "",
  token = NULL,
  resources = NULL,
  out_file = "project.zip"
) {
  destdir <- if (!is.null(token)) {
    file.path(tempdir(), token)
  } else {
    tempfile()
  }

  write_temp_files(js, css, md, destdir)

  md_file <- file.path(destdir, "index.md")
  html_file <- file.path(destdir, "index.html")

  includes <- list(script = list(after = "script.js"), css = "style.css")
  if (!is.null(resources) && length(purrr::compact(resources))) {
    resources <- resource_to_js4shiny_yaml(resources)
    if (!is.null(resources$script)) {
      for (where in c("head", "before", "after")) {
        if (is.null(resources$script[[where]])) next
        includes$script[[where]] <- c(
          resources$script[[where]],
          includes$script[[where]]
        )
      }
    }
    if (!is.null(resources$css)) {
      includes$css <- c(resources$css, includes$css)
    }
  }

  rmarkdown::render(
    input = md_file,
    output_file = html_file,
    quiet = TRUE,
    output_options = list(
      self_contained = FALSE,
      script = includes$script,
      css = includes$css
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
  initial = NULL,
  solution = NULL,
  md = NULL,
  resources = NULL,
  output_file = "example.Rmd"
) {
  example <- list(
    title = default_example_value(title, "Example", single = TRUE),
    instructions = default_example_value(instructions),
    hint = default_example_value(hint),
    initial = list(
      js = default_example_value(initial$js),
      css = default_example_value(initial$css)
    ),
    solution = list(
      js   = default_example_value(solution$js),
      css  = default_example_value(solution$css)
    )
  )

  yaml_header <- list(
    example = example,
    output = if (is.null(resources) || !length(purrr::compact(resources))) {
      "js4shiny::html_document_plain"
    } else {
      list(
        "js4shiny::html_document_plain" = resources %>%
          purrr::compact() %>%
          resource_to_js4shiny_yaml()
      )
    }
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
    "example_html",
    "panel-code-tabset",
    "refresh_html",
    "repl_debug",
    "show_solution",
    "compiled_html"
  )
}

# Include Extra CSS/JS Module ---------------------------------------------

includeExtrasUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::div(
        class = "pull-left",
        style = "width: calc(100% - 30px - 1em);",
        shiny::selectizeInput(
          ns("file_url"),
          "Add Resource",
          choices = list(
            "Enter a URL or choose from options" = "",
            "Shiny, Current" = list(
              "jQuery v3.4.1" = "jquery@3.4.1",
              "Bootstrap CSS v3.4.1" = "bootstrap@3.4.1/dist/css/bootstrap.min.css",
              "Bootstrap JS v3.4.1" = "bootstrap@3.4.1/dist/js/bootstrap.js"
            ),
            "Shiny, Legacy" = list(
              "jQuery v1.12.4" = "jquery@1.12.4",
              "Bootstrap CSS v3.3.7" = "bootstrap@3.3.7/dist/css/bootstrap.min.css",
              "Bootstrap JS v3.3.7" = "bootstrap@3.3.7/dist/js/bootstrap.js"
            ),
            "Others" = list(
              "Bulma" = "bulma",
              "BulmaJS" = "bulmajs",
              "Tachyons" = "tachyons"
            )
          ),
          width = "100%",
          options = list(create = TRUE)
        )
      ),
      shiny::div(
        class = "pull-right",
        style = "margin-top: 25px;",
        shiny::actionButton(ns("add_file"), NULL, icon = shiny::icon("plus"))
      )
    ),
    shiny::fluidRow(
      shiny::helpText(
        "Enter a full URL or an ",
        shiny::a(href = "https://unpkg.com", "unpkg"),
        " identifier, like ",
        shiny::tags$code("three"),
        " or ",
        shiny::tags$code("react@16.7.0"),
        "."
      )
    ),
    shiny::fluidRow(
      class = "includes-file-list",
      shiny::uiOutput(ns("file_list"))
    ),
    shiny::verbatimTextOutput(ns("debug")),
    shiny::singleton(shiny::tags$script(shiny::HTML("
    $(document).on('click', '.includes-file-list .btn__file', (ev) => {
      //console.log(ev.target.closest('.btn__file').dataset)
      const btnData = ev.target.closest('.btn__file').dataset
      Shiny.setInputValue(btnData.inputid, btnData.fileid, {priority: 'event'})
    })

    $(document).on('change', '.includes-file-list select', (ev) => {
      const [inputId, fileId] = ev.target.id.split('__')
      const value = ev.target.value
      Shiny.setInputValue(inputId + '_change', {id: fileId, value})
    })

    const btnWait = (id) => {
      const btn = document.getElementById(id)
      const btnIcon = btn.querySelector('i')
      btnIcon.dataset.restoreClass = btnIcon.className
      btnIcon.className = 'fas fa-spinner fa-spin'
      btn.classList.add('disabled')
    }
    const btnRestore = (id) => {
      const btn = document.getElementById(id)
      const btnIcon = btn.querySelector('i')
      btnIcon.className = btnIcon.dataset.restoreClass || 'fa fa-plus'
      btn.classList.remove('disabled')
    }

    Shiny.addCustomMessageHandler('btnWait', btnWait)
    Shiny.addCustomMessageHandler('btnRestore', btnRestore)
    "))),
    shiny::singleton(shiny::tags$style(shiny::HTML("
    .file-list-row {
      display: grid;
      grid-template-columns: auto 100px 120px 125px;
      grid-column-gap: 1em;
      padding-top: 0.5em;
      padding-bottom: 0.5em;
    }
    .file-list-row * {
      margin-bottom: 0;
    }
    .file-list-row p {
      word-break: break-all;
    }
    .file-type-indicator {
      float: right;
      padding-top: 4px;
    }
    .file--moved {
      animation: anim-file-moved 2s ease-in-out;
    }
    @keyframes anim-file-moved {
      0% {
        background: #f2ed6faa;
      }
      100% {
        background: transparent;
      }
    }
    ")))
  )
}

includeExtras <- function(id, files = NULL, ...) {
  shiny::callModule(includeExtrasModule, id = id, files = files, ...)
}

includeExtrasModule <- function(input, output, session, files = list(), ...) {
  ns <- session$ns

  if (shiny::is.reactivevalues(files)) {
    rv <- files
    shiny::observe({
      rv$files <- resource_add_ids(rv$files)
    }, priority = 5000)
  } else {
    rv <- shiny::reactiveValues(files = resource_add_ids(files))
  }

  trigger_file_update <- shiny::reactiveVal(0)

  if (isTRUE(getOption("js4shiny.debug.includeExtras", FALSE))) {
    output$debug <- shiny::renderPrint(str(rv$files))
  }

  discard_class <- function(ll, keep = NULL, keep_class = "file--moved") {
    purrr::map(ll, ~ {
      if (is.null(keep)) {
        .x$class <- NULL
      } else {
        .x$class <- if (.x$id %in% keep) keep_class
      }
      .x
    })
  }

  shiny::observeEvent(input$add_file, {
    session$sendCustomMessage("btnWait", ns("add_file"))
    resource <- file_resource_list(input$file_url)
    if (is.null(resource)) {
      # doesn't exist on unpkg?
      shiny::showNotification(
        glue("Could not locate {url} on unpkg"),
        type = "warning"
      )
    } else {
      resource[[1]]$class <- "file--moved"
      rvf <- discard_class(rv$files)
      rvf <- c(rvf, resource)
      rv$files <- rvf
      shiny::updateSelectizeInput(session, "file_url", selected = "")
      trigger_file_update(trigger_file_update() + 1)
    }
    session$sendCustomMessage("btnRestore", ns("add_file"))
  })

  file_list_ui <- function(file) {
    file_glue <- function(...) {
      paste0(glue(..., .envir = file))
    }
    id_where <- file_glue("file_where__{id}")
    id_type <- file_glue("file_type__{id}")
    where_choice <- validate_where_choice(
      type = file$type,
      where = shiny::isolate(input[[id_where]]) %||% file$where
    )
    if (!identical(where_choice$where, file$where)) {
      # message("validation changed the where location")
      rv$files[[file$id]]$where <- where_choice$selected
    }
    v_type <- validate_type_choice(
      shiny::isolate(input[[id_type]]) %||% file$type
    )
    if (!identical(file$type, v_type$type)) {
      rv$files[[file$id]]$type <- v_type$type
      file$type <- v_type$type
    }
    shiny::tags$div(
      class = paste("file-list-row", file$class),
      shiny::tags$p(
        shiny::tags$span(
          class = paste("file-type-indicator", switch(
            file$type,
            "javascript" = "fab fa-js-square fa-2x",
            "css" = "fab fa-css3-alt fa-2x",
            "fas fa-exclamation-triangle fa-2x text-danger"
          )),
          style = switch(
            file$type,
            "javascript" = "color: #f7df1e",
            "css" = "color: #0277BD"
          )
        ),
        file$name %||% file$path
      ),
      shiny::selectInput(
        inputId = ns(id_type),
        label = NULL,
        selectize = FALSE,
        choices = v_type$choices,
        selected = file$type
      ),
      shiny::selectInput(
        inputId = ns(id_where),
        label = NULL,
        selectize = FALSE,
        choices = where_choice$choices,
        selected = where_choice$selected
      ),
      shiny::div(
        class = "btn-group",
        shiny::tags$button(
          class = "btn btn-default btn__file",
          `data-fileId` = file$id,
          `data-inputId` = ns("file_up"),
          title = "Load resource earlier",
          shiny::icon("caret-up")
        ),
        shiny::tags$button(
          class = "btn btn-default btn__file",
          `data-fileId` = file$id,
          `data-inputId` = ns("file_down"),
          title = "Load resource later",
          shiny::icon("caret-down")
        ),
        shiny::tags$button(
          class = "btn btn-default btn__file",
          `data-fileId` = file$id,
          `data-inputId` = ns("file_delete"),
          title = "Remove resource",
          shiny::icon("times")
        )
      )
    )
  }

  output$file_list <- shiny::renderUI({
    trigger_file_update()
    rvf <- shiny::isolate(rv$files)
    shiny::req(length(rvf))
    shiny::tagList(
      purrr::map(rvf, file_list_ui)
    )
  })

  # These observers all use custom javascript for receiving updates from the
  # client. Otherwise it would have been too difficult to track which resource
  # is being updated.
  shiny::observeEvent(input$file_type_change, {
    rv$files[[input$file_type_change$id]]$type <- input$file_type_change$value
    rv$files <- discard_class(rv$files)
    trigger_file_update(trigger_file_update() + 1)
  }, priority = 1000)

  shiny::observeEvent(input$file_where_change, {
    rv$files[[input$file_where_change$id]]$where <- input$file_where_change$value
    rv$files <- discard_class(rv$files)
    trigger_file_update(trigger_file_update() + 1)
  }, priority = 1000)

  shiny::observeEvent(input$file_up, {
    ids <- purrr::map_chr(rv$files, "id")
    if (length(ids) == 1) return()

    this_idx <- which(ids == input$file_up)
    if (this_idx == 1) return()

    new_order <- seq_along(ids)
    new_order[this_idx - 1] <- this_idx
    new_order[this_idx] <- this_idx - 1
    rv$files <- discard_class(rv$files[new_order], input$file_up, "file--moved")
    trigger_file_update(trigger_file_update() + 1)
  })

  shiny::observeEvent(input$file_down, {
    ids <- purrr::map_chr(rv$files, "id")
    if (length(ids) == 1) return()

    this_idx <- which(ids == input$file_down)
    if (this_idx == length(ids)) return()

    new_order <- seq_along(ids)
    new_order[this_idx + 1] <- this_idx
    new_order[this_idx] <- this_idx + 1
    rv$files <- discard_class(rv$files[new_order], input$file_down, "file--moved")
    trigger_file_update(trigger_file_update() + 1)
  })

  shiny::observeEvent(input$file_delete, {
    ids <- purrr::map_chr(rv$files, "id")
    this_idx <- which(ids == input$file_delete)
    rv$files <- discard_class(rv$files[-this_idx])
    trigger_file_update(trigger_file_update() + 1)
  })

  return(shiny::reactive(rv$files))
}

file_resource_list <- function(path, type = NULL, where = "head", name = NULL) {
  url <- path
  if (is_url(url) & !grepl("unpkg", url)) {
    name <- tolower(basename(url))
    type <- sub("^.+(css|js)$", "\\1", name)
    type <- intersect(c("js", "css"), type)
    type <- if (length(type) && nzchar(type)) {
      c(js = "javascript", css = "css")[type]
    } else {
      ""
    }
  } else {
    name <- path
    url <- unpkg_url(url)
    type <- unpkg_type(url)
  }
  if (is.null(type)) {
    return(NULL)
  }

  add_file(NULL, url, type, where, name)
}

add_file <- function(
  file_list,
  path,
  type = NULL,
  where = c("head", "before", "after"),
  name = NULL,
  type_allow_unknown = TRUE
) {
  type <- type %||% ""
  if (!type_allow_unknown) {
    type <- match.arg(type, choices = c("javascript", "css"))
  } else {
    if (length(setdiff(type, c("javascript", "css", "")))) {
      type <- ""
    }
  }
  where <- where %||% "head"
  where <- match.arg(where, several.ok = FALSE)
  resource <- list()
  id <- rand_id()
  resource[[id]] <- list(
    id = id,
    name = name,
    path = paste(path),
    type = type,
    where = where
  )

  c(file_list, resource)
}

is_url <- function(url) {
  grepl("^https?", url)
}

validate_where_choice <- function(type, where = NULL) {
  choices <- switch(
    type,
    javascript = c("head", "before body" = "before", "after body" = "after"),
    css = c("head"),
    c("set type" = "", "head")
  )
  where <- where %||% choices[1]
  selected <- intersect(choices, where)
  if (!length(selected)) selected <- choices[1]
  list(choices = choices, selected = selected)
}

validate_type_choice <- function(type) {
  if (length(type) > 1) {
    warning("Multiple types provided, using only first.")
  }
  choices <- c("js" = "javascript", "css" = "css")
  if (length(type) == 0) {
    list(type = "", choices = c("", choices))
  } else if (type %in% c("js", "javascript")) {
    list(type = "javascript", choices = choices)
  } else if (type == "css") {
    list(type = "css", choices = choices)
  } else {
    list(type = "", choices = c("", choices))
  }
}

rand_id <- function() {
  paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = "")
}

unpkg_url <- function(slug) {
  if (is_url(slug)) {
    return(slug)
  }
  glue("https://unpkg.com/{slug}")
}

unpkg_meta <- function(url) {
  url <- unpkg_url(url)
  url <- glue("{url}?meta")
  tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) {
      warning(e$message)
      return(NULL)
    }
  )
}

unpkg_type <- function(slug) {
  meta <- unpkg_meta(slug)
  if (is.null(meta)) {
    return(NULL)
  }
  type <- sub("^\\w+/", "", meta$contentType)
  if (type %in% c("javascript", "css")) type else ""
}

includeExtrasDev <- function(files = NULL) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::div(
        class = "col-xs-12 col-sm-8 col-sm-offset-2",
        includeExtrasUI("test")
      )
    ),
    server = function(input, output, session) {
      r_files <- shiny::reactiveValues(files = files)
      includeExtras("test", files = r_files)
    }
  )
}
