#' js4shiny HTML Dependencies
#'
#' Include the various HTML JavaScript and CSS assets created for
#' \pkg{js4shiny}.
#'
#' @param redirectConsole Include JS and CSS assets to enable literate
#'   programming with JavaScript by redirecting `console.log()` to a chunk-
#'   specific output div.
#' @param jsonview Include JS and CSS assets to enable tree view display of
#'   JSON objects for the \pkg{knitr} json engine.
#' @param stylize One of "none", "all", "fonts", "variables", "table",
#'   "utility", "code", "pandoc-line-numbers" to include the CSS styles
#'   developed for \pkg{js4shiny}.
#' @inheritParams html_document_js
#' @family js4shiny HTML dependencies
#' @export
html_dependency_js4shiny <- function(
  redirectConsole = TRUE,
  jsonview = TRUE,
  stylize = "all",
  use_google_fonts = FALSE
) {
  stylize <- match.arg(
    stylize,
    choices = c("none", "all", "fonts", "variables", "table", "utility", "code",
                "stylize", "pandoc-line-numbers"),
    several.ok = TRUE
  )

  deps <- htmltools::htmlDependency(
    name = "js4shiny",
    package = "js4shiny",
    version = utils::packageVersion("js4shiny"),
    src = "template-html",
    script = c(
      if (jsonview) "jsonview/jsonview.min.js"
    ),
    stylesheet = c(
      if (!"none" %in% stylize) stylize_bundle(stylize),
      if (jsonview) "jsonview/jsonview.css"
    ),
    all_files = FALSE
  )
  deps <- list(deps)

  if (redirectConsole) {
    deps <- c(deps, list(html_dependency_redirectConsoleLog()))
  }

  if (use_google_fonts) {
    deps <- c(list(html_dependency_google_fonts()), deps)
  }

  deps
}

stylize_bundle <- function(
  include= c("all", "fonts", "variables", "table", "utility", "code",
             "stylize", "pandoc-line-numbers"),
  bundle = FALSE,
  rel = TRUE
) {
  include <- match.arg(include, several.ok = TRUE)

  stylize_components <- c(
    "fonts" = "_variables-fonts.css",
    "variables" = "_variables.css",
    "stylize" = "stylize.css",
    "table" = "table.css",
    "utility" = "utility.css",
    "code" = "code.css",
    "pandoc-line-numbers" = "pandoc-line-numbers.css"
  )

  if (!"all" %in% include) {
    stylize_components <- stylize_components[include]
  }

  if (!bundle) {
    if (rel) {
      return(paste0("css/", unname(stylize_components)))
    } else {
      return(js4shiny_file("template-html", "css", stylize_components))
    }
  }

  js4shiny_file("template-html", "css", stylize_components) %>%
    purrr::map(read_lines, warn = FALSE) %>%
    purrr::map_chr(paste, collapse = "\n") %>%
    paste(collapse = "\n")
}

#' @describeIn html_dependency_js4shiny Include just the console redirection
#'   dependencies.
#' @export
html_dependency_redirectConsoleLog <- function() {
  htmltools::htmlDependency(
    name = "js4shiny-redirectConsoleLog",
    package = "js4shiny",
    version = utils::packageVersion("js4shiny"),
    src = "redirect",
    script = "redirectConsoleLog.js",
    stylesheet = "jslog.css",
    all_files = FALSE
  )
}

#' @describeIn html_dependency_js4shiny Include the full or partial
#'   \pkg{js4shiny} CSS styles.
#' @param ... Arguments passed to `html_dependency_js4shiny()`.
#' @export
html_dependency_stylize <- function(...) {
  htmltools::htmlDependency(
    name = "js4shiny-stylize",
    package = "js4shiny",
    version = utils::packageVersion("js4shiny"),
    src = "template-html",
    stylesheet = stylize_bundle(..., bundle = FALSE),
    all_files = FALSE
  )
}

html_dependency_google_fonts <- function() {
  htmltools::htmlDependency(
    name = "js4shiny-google-fonts",
    version = utils::packageVersion("js4shiny"),
    package = "js4shiny",
    src = "template-html",
    stylesheet = "css/_google-fonts.css",
    all_files = FALSE
  )
}

#' Load js4shiny HTML assets and knitr dependencies
#'
#' Overrides the JavaScript knitr engine, registers knitr output hooks, and
#' declares the JS and CSS dependencies that are required to enable the
#' literate JavaScript code chunks inside R Markdown formats that write to HTML.
#'
#' @inheritParams html_dependency_js4shiny
#' @family js4shiny HTML dependencies
#' @export
html_setup <- function(stylize = "none") {
  register_knitr_js_engine()
  register_knitr_output_hooks()
  htmltools::tagList(
    html_dependency_js4shiny(stylize = stylize)
  )
}

#' @describeIn html_setup A blogdown-specific HTML setup that includes styles
#'   for `<pre>` code blocks, tables, some utility functions, and the CSS
#'   variables declaring the \pkg{js4shiny} colors.
#' @export
html_setup_blogdown <- function(
  stylize = c("fonts", "variables", "table", "code", "utility")
) {
  html_setup(stylize)
}
