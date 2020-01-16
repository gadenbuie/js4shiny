#' Serve a Live Preview
#'
#' Opens a live preview of the files in a directory. The live preview server
#' automatically renders R Markdown files when they are saved, and the preview
#' is refreshed whenever R Markdown files or supporting files, such as `.js`,
#' `.css`, `.htm`, `.html`, `.sass`, or `.scss` files, are updated. This
#' functionality requires the \pkg{servr} package.
#'
#' @section RStudio Addins: There are three Live Preview addins provided by
#'   \pkg{js4shiny}. **Live Preview** and **Live Preview (External)** open a
#'   live preview of the directory of the currently open document, if possible
#'   at the current HTML document corresponding to the open document. The
#'   external preview addin automatically opens the preview in your web browser,
#'   otherwise the preview is opened in the RStudio Viewer pane.
#'
#'   To stop the live server, you can call `servr::daemon_stop()` or
#'   `live_preview_stop()`, which will stop all bakground \pkg{servr} daemons,
#'   or you can use the **Live Preview Stop** addin.
#'
#' @examples
#' if (interactive()) {
#'
#' tmp_dir <- tempfile("live-preview")
#' dir.create(tmp_dir)
#' tmp_rmd <- file.path(tmp_dir, "js4shiny-plain.Rmd")
#'
#' # Create a new js4shiny plain HTML document. If interactive
#' # and in RStudio, this file will open and you can use the
#' # addins to launch the live preview
#' js4shiny_rmd("js", full_template = TRUE, path = tmp_rmd)
#'
#' srvr <- live_preview(tmp_rmd)
#'
#' # Stop all background servers with either of the following
#' # live_preview_stop()
#' # servr::daemon_stop()
#' #
#' # Or if you've saved the return value from live_preview()
#' # srvr$stop_server()
#' }
#'
#' @param path The path for the directory or file to preview. If the path given
#'   is an R Markdown document or HTML document, the HTML version of that file
#'   will be opened directly, otherwise the directory containing the file will
#'   be served.
#' @param update_pattern Update the live preview when files matching this
#'   pattern are updated. By default, updating files with the following
#'   extensions will update the preview: `.Rmd` (case insensitive), `.html`,
#'   `.htm`, `.js`, `.css`, `.sass`, `.scss`.
#' @param render_quietly If `TRUE` (default), the output from
#'   [rmarkdown::render()] will not be shown. Set to `FALSE` for debugging. You
#'   can set the default value with a global option:
#'
#'   `options(js4shiny.live_preview.quiet = FALSE)`
#' @param external Should the live preview be opened in an external browser?
#'   The default is `FALSE` and the preivew is opened in the RStudio viewer pane
#'   (if launched inside RStudio).
#' @inheritDotParams servr::httw
#'
#' @return Invisibly returns the [servr::httw()] object, so that you can
#'   manually stop the server with the `$stop_server()` method.
#' @export
live_preview <- function(
  path = getwd(),
  update_pattern = "[.](js|css|[Rr][Mm][Dd]|html?|s[ca]ss)$",
  ...,
  render_quietly = getOption("js4shiny.live_preview.quiet", TRUE),
  external = FALSE
) {
  requires_pkg("servr")
  is_servr_old <- utils::packageVersion("servr") < package_version("0.13")

  path_dir <- if (fs::is_dir(path)) path else fs::path_dir(path)

  render_quietly <- isTRUE(render_quietly)

  render <- function(path) {
    rmd_paths <- path[grepl("[.]rmd", path, ignore.case = TRUE)]
    if (length(rmd_paths)) {
      for (rmd_path in rmd_paths) {
        if (render_quietly) message(glue(
          "Rendering {rmd_path}"
        ))
        rmarkdown::render(rmd_path, envir = new.env(), quiet = render_quietly)
      }
    }
    path
  }

  path_is_rmd <- fs::is_file(path) && tolower(fs::path_ext(path)) == "rmd"
  path_file <- "/"
  if (path_is_rmd) {
    # will it render to html?
    output_format <- rmarkdown::default_output_format(path)$name
    is_html <- grepl("html", output_format)
    if (is_html) {
      path_html <- path
      fs::path_ext(path_html) <- "html"
      if (!fs::file_exists(path_html) || is_outdated(path_html, path)) {
        render(path)
      }
      path_file <- fs::path_file(path_html)
    }
  }

  viewer <- if (external) {
    utils::browseURL
  } else {
    getOption("viewer", utils::browseURL)
  }

  x <- servr::httw(
    dir = path_dir,
    pattern = update_pattern,
    initpath = path_file,
    browser = is_servr_old && !external,
    handler = render,
    ...
  )
  if (is_servr_old) {
    # older versions of servr don't return the config option, so we have to
    # rely on servr to open. Or we just don't open and we let the user choose.
    if (external) {
      message("Your version of servr is out of date. Use the link above to open the preview.")
    }
  } else {
    viewer(x$url)
  }
  invisible(x)
}

#' @describeIn live_preview Stop the live preview background daemons. See
#'   [servr::daemon_list()] for more information.
#' @inheritParams servr::daemon_stop
#' @export
live_preview_stop <- function(which = NULL) {
  requires_pkg("servr")
  which <- which %||% servr::daemon_list()
  if (!length(which)) {
    message("No server daemon specified or no daemon is running")
    return(invisible())
  }
  word_daemon <- paste0("daemon", if (length(which) > 1) "s")
  servr::daemon_stop(which %||% servr::daemon_list())
  message(glue("Stopped {length(which)} server {word_daemon}"))
}

live_preview_addin <- function() {
  ctx <- get_source_context("The live preview addin only works in RStudio.")
  live_preview(ctx$path, external = FALSE)
}

live_preview_external_addin <- function() {
  ctx <- get_source_context("The live preview addin only works in RStudio.")
  live_preview(ctx$path, external = TRUE)
}

get_source_context <- function(error_msg = "Requires RStudio") {
  requires_pkg("rstudioapi")
  if (!isTRUE(rstudioapi::hasFun("getSourceEditorContext"))) {
    stop(error_msg)
  }
  rstudioapi::getSourceEditorContext()
}

#' Lint and Fix JavaScript file with StandardJS
#'
#' This addin lints and fixes selected JavaScript code or the currently open
#' file in RStudio. The addin can be helpful for linting JavaScript code
#' embedded in R Markdown or Shiny apps, in addition to linting whole JavaScript
#' files. The underlying functions are not exported from \pkg{js4shiny}. If you
#' want to programmatically lint multiple files, it would be better to use `npm`
#' scripts or another JavaScript task running system to lint your files.
#'
#' @section Installing StandardJS: [standardjs](https://standardjs.com/) is a
#'   style guide, code linter, and beautifier in one. It is also a command line
#'   tool (`standard`) for automatically formatting JavaScript code in the
#'   [JavaScript Standard Style](https://standardjs.com/). The command line tool
#'   will also alert users to common style and programmer errors.
#'
#'   Using `standard` and this addin requires that `node`, `npm`, and `standard`
#'   be installed on your system. To install `node` and `npm`, you need to
#'   install Node.js (they come together). Follow [the instructions from
#'   Node.js](https://nodejs.org/en/download/) to install these tools. Confirm
#'   that your installation was successful by running `npm -v` in a new terminal
#'   session. Once `npm` is available, install `standard` globally by running
#'   this command in the terminal.
#'
#'   ```
#'   npm install standard --global
#'   ```
#'
#' @references https://standardjs.com/
#' @name lint_js_addin
#' @rdname lint_js_addin
NULL

lint_js_addin <- function(path = NULL) {
  ctx <- get_source_context("The linter addin only works in RStudio.")
  if (is.null(ctx) && is.null(path)) {
    message("No file to lint. Open the file you want to lint and try again.")
    return(invisible())
  }
  if (!js_lint_has_standard()) {
    js_lint_requires_standard()
  }
  code <- ctx$selection[[1]]$text
  msgs <- NULL
  if (is.null(ctx) && !is.null(path)) {
    msgs <- js_lint_file(path)
  } else if (is_null_or_nothing(code)) {
    rstudioapi::documentSave(ctx$id)
    msgs <- js_lint_file(ctx$path)
    rstudioapi::navigateToFile(ctx$path)
  } else {
    res <- js_lint(code, "standard", fs::path_ext_remove(fs::path_file(ctx$path)))
    if (length(res$warnings)) msgs <- res$warnings
    rstudioapi::modifyRange(ctx$selection[[1]]$range, collapse(res$code), id = ctx$id)
  }
  if (!is.null(msgs) && length(msgs)) purrr::walk(msgs, message) else {
    message("\u2714 JavaScript Standard Style")
  }
}

#' Choose Launch Location for Shiny Apps
#'
#' This function sets the `shiny.launch.browser` option to launch Shiny apps in
#' an `"external"` browser, the RStudio viewer `"pane"`, or a new `"window"` in
#' RStudio.
#'
#' @param where One of `"external"`, `"pane"`, or `"window"`.
#' @export
launch_shiny_in <- function(where = NULL) {
  requires_pkg("rstudioapi")
  if (!isTRUE(rstudioapi::hasFun("getSourceEditorContext"))) {
    stop("Must be called from RStudio")
  }

  where <- where %||% ask_where_to_launch()
  message(glue("Shiny apps will launch in {where} viewer"))

  options(shiny.launch.browser = switch(
    match.arg(where, c("external", "pane", "window")),
    external = get(".rs.invokeShinyWindowExternal", "tools:rstudio"),
    pane = get(".rs.invokeShinyPaneViewer", "tools:rstudio"),
    window = get(".rs.invokeShinyWindowViewer", "tools:rstudio")
  ))
}

ask_where_to_launch <- function() {
  if (!interactive()) {
    return("external")
  }
  where <- utils::askYesNo(
    "Launch Shiny apps in [E]xternal Browser or RStudio [W]indow or [P]ane?",
    prompts = "E/W/P"
  )
  where <- if (isTRUE(where)) {
    "external"
  } else if (is.na(where)) {
    "pane"
  } else if (!isTRUE(where)) {
    "window"
  }
}

#' @describeIn repl Launch a \pkg{js4shiny} exercise or example using the
#'   example slug, or the full filename. If none provided, `repl_example()`
#'   launches an interactive example browser.
#' @export
repl_example <- function(example = NULL) {
  chose_example <- FALSE
  if (is.null(example)) {
    example <- choose_examples()
    chose_example <- TRUE
  }
  if (!file.exists(example) && !chose_example) {
    example <- search_for_example(basename(example))
  }

  if (is.null(example)) {
    return()
  }

  # Choose runtime for example (repl() or repl_js())
  run_fn <- choose_runtime(example)

  run_cmd <- glue('js4shiny::{run_fn}(example = "{example}")')
  if (!chose_example) {
    eval(parse(text = run_cmd))
  } else {
    # Can't launch a Shiny app from a running Shiny gadget. Instead, we send the
    # repl() command to console to launch.
    # Thanks to Joris Meys: https://stackoverflow.com/a/44891545
    rstudioapi::sendToConsole(run_cmd, execute = TRUE)
  }
}

choose_runtime <- function(example) {
  if (identical(basename(example), "app.R")) {
    run_fn <- ":open_app_example"
  } else if (identical(basename(example), "index.html")) {
    run_fn <- ":open_html_example"
  } else {
    if (!fs::is_dir(example)) {
      info <- extract_yaml(example)
      run_fn <- info$example$runtime %||% "repl"
      if (!run_fn %in% c("repl", "repl_js")) {
        warning(glue("Unkown runtime in example yaml: '{run_fn}'"))
        run_fn <- "repl"
      }
    } else {
      # get info from registry in directory
      info <- read_registry_yaml(example)
      run_fn <- switch(
        info$type %||% "repl",
        shiny = , "shiny-starter" = , "shiny-run" = ":open_app_example",
        html = , "html-external" = ":open_html_example",
        "repl"
      )
    }
  }
  run_fn
}

open_app_example <- function(example) {
  if (fs::is_dir(example)) {
    example <- fs::path(example, "app.R")
    stopifnot(fs::file_exists(example))
  }
  info <- read_registry_yaml(dirname(example))
  type <- info$type %||% "shiny-run"
  type <- match.arg(type, c("shiny", "shiny-starter", "shiny-run"))
  if (type %in% c("shiny", "shiny-starter")) {
    message(glue(
      "Opening {basename(dirname(example))} as new R script, ",
      "save the file as app.R"
    ))
    text <- collapse(read_lines(example))
    text <- paste0(glue("# {basename(dirname(example))}/app.R"), "\n\n", text)
    rstudioapi::documentNew(text = text, type = "r", execute = FALSE)
  } else {
    shiny::runApp(example)
  }
}

open_html_example <- function(example) {
  if (fs::is_dir(example)) {
    example <- fs::path(example, "index.html")
  }
  if (grepl("index[.]html$", example)) {
    stopifnot(file.exists(example))
    info <- read_registry_yaml(dirname(example))
    external <- grepl("external", info$type %||% "")
    live_preview(dirname(example), external = external)
  } else {
    stop("Not sure how to open example: ", example)
  }
}

search_for_example <- function(example) {
  # assume that example is a slug, i.e. the file name of an example without .Rmd
  all_examples <- dir(
    js4shiny_file("examples"),
    pattern = "[.]Rmd$",
    full.names = TRUE,
    recursive = TRUE
  )
  all_example_slugs <- sub("(.+)[.][Rr]md", "\\1", basename(all_examples))

  example_found <- all_examples[which(example == all_example_slugs)]
  if (length(example_found)) {
    example_found[1]
  } else {
    search_for_example_dir(example)
  }
}

search_for_example_dir <- function(example) {
  all_examples <- dir(
    js4shiny_file("examples"),
    pattern = "app[.][rR]|index[.]html?|[.][Rr]md|registry.yml",
    full.names = TRUE,
    recursive = TRUE
  )
  all_example_slugs <- c(
    dirname(all_examples),
    dirname(dirname(all_examples))
  )
  all_example_slugs <- sort(unique(all_example_slugs))
  names(all_example_slugs) <- basename(all_example_slugs)

  example <- all_example_slugs[which(example == names(all_example_slugs))]
  if (length(example) > 0) {
    if (all(grepl(".rmd", tolower(example)))) {
      return(dirname(example[1]))
    } else {
      return(example[1])
    }
  }
}

choose_examples <- function(
  ...,
  viewer = shiny::dialogViewer("js4shiny", height = 450)
) {
  ex_dir <- list_examples(js4shiny_file("examples"))

  ex_level_1 <- example_path_info(ex_dir$dirs)

  ui <- shiny::basicPage(
    shiny::h2("Choose an example or exercise"),
    shiny::selectInput(
      "category",
      "Category",
      choices = c(
        list("Choose Example/Exercise Category" = ""),
        ex_level_1
      ),
      width = "100%"
    ),
    shiny::uiOutput("ui_category_description"),
    shiny::uiOutput("ui_group"),
    shiny::uiOutput("ui_group_description"),
    shiny::uiOutput("ui_example"),
    shiny::div(
      class = "btn-group",
      shiny::actionButton("cancel", "Cancel"),
      shiny::actionButton(
        "done",
        label = "Choose",
        class = "btn-primary disabled"
      )
    ),
    shiny::tags$style(shiny::HTML("
      .description {
          color: #777;
          font-style: italic;
          margin-top: -15px;
          margin-bottom: 15px;
      }
      ")),
    shiny::tags$script(shiny::HTML("
    function enableDoneBtn(enable) {
      document.getElementById('done').classList.toggle('disabled', !enable);
    }
    Shiny.addCustomMessageHandler('enableDone', enableDoneBtn);
    "))
  )

  server <- function(input, output, session) {
    ex_level_2 <- shiny::reactive({
      shiny::req(input$category)
      exs <- list_examples(input$category)
      if (!is.null(exs$dirs)) {
        example_path_info(exs$dirs)
      }
    })

    examples <- shiny::reactive({
      shiny::req(input$category)
      exs <- if (!is.null(input$group) && input$group != "") {
        list_examples(input$group, recurse = 1)
      } else {
        list_examples(input$category, recurse = 1)
      }
    })

    output$ui_category_description <- shiny::renderUI({
      shiny::req(input$category)
      shiny::p(class = "description", read_registry_yaml(input$category)$description)
    })

    output$ui_group <- shiny::renderUI({
      shiny::req(ex_level_2())
      shiny::selectInput(
        "group",
        "Group",
        choices = c(
          list("Groups" = ""),
          ex_level_2()
        ),
        width = "100%"
      )
    })

    output$ui_group_description <- shiny::renderUI({
      shiny::req(input$group)
      shiny::p(class = "description", read_registry_yaml(input$group)$description)
    })

    output$ui_example <- shiny::renderUI({
      shiny::req(examples())

      ex_files <- examples()$files %>% purrr::map(read_file_info)

      is_shiny_group <- identical(examples()$info$type, "shiny-apps")

      default_choice <- if (!is_shiny_group) c("All" = "all")

      shiny::tagList(
        shiny::selectInput(
          "examples",
          "Examples",
          choices = c(
            default_choice,
            purrr::set_names(examples()$files, ex_files)
          ),
          width = "100%"
        ),
        shiny::p(
          class = "description",
          paste(
            "Choose",
            if (!is_shiny_group) "all examples or",
            "a specific example."
          )
        )
      )
    })

    shiny::observe({
      has_category <- !is_null_or_nothing(input$category)
      session$sendCustomMessage("enableDone", has_category)
    })

    shiny::observeEvent(input$done, {
      choice <- if (is_null_or_nothing(input$category)) {
        NULL
      } else if (identical(input$examples, "all")) {
        if (is_null_or_nothing(input$group)) input$category else input$group
      } else {
        input$examples
      }
      shiny::stopApp(choice)
    })

    shiny::observeEvent(input$cancel, shiny::stopApp(NULL))
  }

  shiny::runGadget(ui, server, viewer = viewer, ...)
}

list_examples <- function(path, recurse = 0L) {
  registry_info <- read_registry_yaml(path)
  has_subdir <- length(fs::dir_ls(path, type = "dir")) > 0

  list(
    path = path,
    info = registry_info,
    files = as.character(fs::dir_ls(path, regexp = "((app[.]R)|(.[Rr][Mm][Dd]))$", recurse = recurse)),
    dirs = if (has_subdir && recurse < 2) {
      unname(purrr::map(
        as.character(fs::dir_ls(path, type = "dir")),
        list_examples,
        recurse = recurse + 1L
      ))
    }
  )
}

read_file_info <- function(path) {
  if (grepl("rmd$", tolower(path))) {
    extract_yaml(path)$example$title %||% fs::path_file(path)
  } else if (grepl("app.r", tolower(path), fixed = TRUE)) {
    read_registry_yaml(fs::path_dir(path))$title %||% fs::path_dir(path)
  }
}

example_path_info <- function(x) {
  if (identical(setdiff(c("info", "path"), names(x)), character(0))) {
    x <- list(x)
  }
  x %>%
    purrr::map(`[`, c("info", "path")) %>%
    purrr::map(~ purrr::modify_at(.x, .at = "info", .f = ~ .$title)) %>%
    purrr::map(~ purrr::set_names(.x$path, .x$info)) %>%
    purrr::flatten_chr()
}

find_registry_yaml <- function(path) {
  path <- path[1]
  if (grepl("registry.ya?ml", path, ignore.case = TRUE)) {
    return(path)
  }
  fs::dir_ls(path, type = "file", regexp = "registry[.]ya?ml")
}

read_registry_yaml <- function(path) {
  path <- find_registry_yaml(path)
  if (!length(path)) return()
  x <- yaml::yaml.load_file(path)
  if (length(x)) x
}
