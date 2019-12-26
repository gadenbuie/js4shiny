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
#'   output_options = list(version = "initial")
#' )
#' rmarkdown::render(
#'   input = css_ex,
#'   output_file = tmp_html_sol,
#'   output_options = list(version = "solution")
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
