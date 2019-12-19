live_preview <- function(
  path = getwd(),
  pattern = "[.](js|css|[Rr][Mm][Dd]|html?|s[ca]ss)$",
  ...,
  external = FALSE
) {
  requires_pkg("servr")
  path_dir <- dirname(path)
  path_file <- if (grepl("[.]rmd", path, ignore.case = TRUE)) "/" else basename(path)
  viewer <- if (external) {
    utils::browseURL
  } else {
    getOption("viewer", utils::browseURL)
  }
  render <- function(path) {
    rmd_paths <- path[grepl("[.]rmd", path, ignore.case = TRUE)]
    if (length(rmd_paths)) {
      for (rmd_path in rmd_paths) {
        rmarkdown::render(rmd_path, envir = new.env())
      }
    }
    path
  }
  x <- servr::httw(
    dir = path_dir,
    pattern = pattern,
    initpath = path_file,
    browser = FALSE,
    handler = render,
    ...
  )
  viewer(x$url)
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
  is_dir <- dir.exists(example)
  if (is_dir || !file.exists(example)) {
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
  if (basename(example) == "app.R") {
    run_fn <- ":open_app_example"
  } else if (basename(example) == "index.html") {
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
      run_fn <- "repl"
    }
  }
  run_fn
}

open_app_example <- function(example) {
  rstudioapi::documentNew(
    text = paste(readLines(example, warn = FALSE), collapse = "\n"),
    type = "r",
    execute = FALSE
  )
}

open_html_example <- function(example) {
  stop("not yet implemented")
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
    pattern = "app[.][rR]|index[.]html?",
    full.names = TRUE,
    recursive = TRUE
  )
  all_example_slugs <- basename(dirname(all_examples))

  example <- all_examples[which(example == all_example_slugs)]
  if (length(example)) example[1] else NULL
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
        list_examples(input$group)
      } else {
        list_examples(input$category)
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

      ex_files <- examples()$files %>%
        purrr::map(~ extract_yaml(.x)$example$title %||% fs::path_file(.x))

      shiny::tagList(
        shiny::selectInput(
          "examples",
          "Examples",
          choices = c(
            "All" = "all",
            purrr::set_names(examples()$files, ex_files)
          ),
          width = "100%"
        ),
        shiny::p(class = "description", "Choose all examples or a specific example.")
      )
    })

    shiny::observe({
      has_category <- !(is.null(input$category) || input$category == "")
      session$sendCustomMessage("enableDone", has_category)
    })

    shiny::observeEvent(input$done, {
      choice <- if (input$category %||% "" == "") {
        NULL
      } else if (input$examples == "all") {
        if (input$group %||% "" == "") input$category else input$group
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
    files = as.character(fs::dir_ls(path, regexp = ".[Rr][Mm][Dd]$", recurse = recurse)),
    dirs = if (has_subdir && recurse < 2) {
      unname(purrr::map(
        as.character(fs::dir_ls(path, type = "dir")),
        list_examples,
        recurse = recurse + 1L
      ))
    }
  )
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
  x <- x[c("title", "description")]
  if (length(x)) x
}
