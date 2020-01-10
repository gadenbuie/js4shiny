#' js4shiny Example Document
#'
#' This document type is built on the [html_document_plain()], but is configured
#' to render the example documents created by the [repl()]. You may choose to
#' render the solution or the example's initial state by setting the output
#' option `version`.
#'
#' @examples
#' css_ex <- system.file(
#'   "examples", "css", "css-basics", "css-basics-appearance.Rmd",
#'   package = "js4shiny"
#' )
#'
#' tmp_html_init <- tempfile("initial", fileext = ".html")
#' tmp_html_sol <- tempfile("solution", fileext = ".html")
#'
#' rmarkdown::render(
#'   input = css_ex,
#'   output_file = tmp_html_init,
#'   output_options = list(version = "initial"),
#'   quiet = TRUE
#' )
#' rmarkdown::render(
#'   input = css_ex,
#'   output_file = tmp_html_sol,
#'   output_options = list(version = "solution"),
#'   quiet = TRUE
#' )
#'
#' # View tmp_html_init/sol
#' # browseURL(tmp_html_init)
#' # browseURL(tmp_html_sol)
#'
#' @seealso [html_document_plain()] [html_document_js()]
#' @param version Which version of the example to render. One of `"solution"`
#'   (default) or `"initial"`.
#' @param ... Additional arguments passed to [html_document_plain()]
#' @export
html_document_js4shiny <- function(version = c("solution", "initial"), ...) {
  version <- match.arg(version)

  rmarkdown::output_format(
    knitr = NULL,
    pandoc = NULL,
    pre_processor = function(
      metadata, input_file, runtime, knit_meta, files_dir, output_dir
    ) {
      yml <- extract_yaml(input_file)
      title <- yml$example$title %||% "js4shiny document"
      if (!dir.exists(files_dir)) dir.create(files_dir)

      ex_js_file <- tempfile("script", files_dir, ".js")
      ex_css_file <- tempfile("style", files_dir, ".css")

      js  <- yml$example[[version]]$js  %||% "/* Write your JavaScript here */"
      css <- yml$example[[version]]$css %||% "/* Write you CSS here */"

      cat(js,  file = ex_js_file,  sep = "\n")
      cat(css, file = ex_css_file, sep = "\n")
      c(
        "--variable", glue("script-after={ex_js_file}"),
        "--css", glue("{ex_css_file}"),
        if (is.null(yml$title)) c("--metadata", glue("title={title}"))
      )
    },
    base_format = html_document_plain(...)
  )
}

render_html_js4shiny <- function(input, output_dir = NULL, prefix = NULL, ...) {
  if (fs::is_dir(input)) {
    input <- fs::dir_ls(input, regexp = "[.][Rr]md$")
    stopifnot(length(input) > 0)
  } else {
    stopifnot(fs::file_exists(input))
  }
  render_html_js4shiny__ <- purrr::partial(
    render_html_js4shiny_,
    output_dir = output_dir,
    prefix = prefix,
    ...
  )
  purrr::walk(input, render_html_js4shiny__)
}

render_html_js4shiny_ <- function(input, output_file = NULL, output_dir = NULL, prefix = NULL, ...) {
  yml <- extract_yaml(input)
  if (!(identical(yml$output, "js4shiny::html_document_js4shiny") ||
      identical(names(yml$output)[1], "js4shiny::html_document_js4shiny"))) {
    stop(input, " is not a js4shiny html document.")
  }
  if (is.null(output_file)) output_file <- fs::path_file(input)
  output_file_base <- fs::path_ext_remove(output_file)
  has_solution <- !is.null(yml$example$solution) &&
    purrr::some(yml$example$solution, purrr::negate(is.null))
  output_initial_base <- paste0(
    prefix, output_file_base, if (has_solution) "_initial.html" else ".html"
  )

  if (is_outdated(output_initial_base, input, output_dir)) {
    message(input, " -> ", fs::path(output_dir, output_initial_base))
    rmarkdown::render(
      input = input,
      output_file = output_initial_base,
      output_dir = output_dir,
      output_options = list(version = "initial"),
      ...
    )
  } else {
    message(
      fs::path(output_dir, output_initial_base),
      " is up to date"
    )
  }
  if (has_solution) {
    output_solution_base <- paste0(prefix, output_file_base, "_solution.html")
    if (is_outdated(output_solution_base, input, output_dir)) {
      message(input, " -> ", fs::path(output_dir, output_solution_base))
      rmarkdown::render(
        input = input,
        output_file = output_solution_base,
        output_dir = output_dir,
        output_options = list(version = "solution"),
        ...
      )
    } else {
      message(
        fs::path(output_dir, output_solution_base),
        " is up to date"
      )
    }
  }
}

is_outdated <- function(path, ref, path_dir = NULL) {
  if (!is.null(path_dir)) path <- fs::path(path_dir, path)
  if (!fs::file_exists(path)) return(TRUE)
  fs::file_info(path)$change_time < fs::file_info(ref)$change_time
}
