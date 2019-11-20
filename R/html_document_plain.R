#' Plain (Minimal) HTML Document
#'
#' This RMarkdown output format provides a minimal HTML5 template and minimal
#' features for including CSS and JavaScript files in the output source.
#'
#' @inheritParams html_document_js
#' @param css A list of css files to include in the document's `<head>`. Include
#'   `"normalize"` in the list of css files to include `normalize.css`, which
#'   provides basic style resetting.
#' @param script A list of `.js` files to include in the document. Use
#'   `include_script()` to choose where in the HTML the script should be
#'   sourced.
#' @export
html_document_plain <- function(
  ...,
  css = "normalize",
  script = NULL,
  highlight = "haddock",
  fig_width = 10,
  fig_height = 7,
  fig_retina = 2,
  keep_md = FALSE,
  dev = "png",
  pandoc_args = NULL,
  extra_dependencies = NULL
) {

  deps <- c(extra_dependencies)

  if ("normalize" %in% css) {
    css[which(css == "normalize")] <- js4shiny_file("template-plain", "normalize.css")
  }

  rmarkdown::output_format(
    knitr = rmarkdown::knitr_options(
      opts_chunk = list(
        fig_width = fig_width,
        fig_height = fig_height,
        fig_retina = fig_retina,
        keep_md = keep_md,
        dev = dev
      ) #,
      # knit_hooks = knit_hooks
    ),
    pandoc = rmarkdown::pandoc_options(
      to = "html5",
      from = "markdown+ascii_identifiers+tex_math_single_backslash+raw_html-markdown_in_html_blocks-native_divs-native_spans",
      args = c(
        if (!is.null(css)) args_css(css),
        pandoc_args,
        args_scripts(script),
        "--template",
        js4shiny_file("template-plain", "plain-template.html")
      ),
    ),
    clean_supporting = FALSE,
    base_format = rmarkdown::html_document_base(
      template = NULL,
      theme = NULL,
      mathjax = NULL,
      extra_dependencies = deps,
      ...
    )
  )
}

args_css <- function(css = NULL) {
  if (is.null(css)) return()
  css_arg <- function(x) list(c("--css", x))
  unlist(vapply(css, css_arg, list(1), USE.NAMES = FALSE))
}

#' @describeIn html_document_plain Helper function for including JS scripts
#' @param head,before,after A character vector of source files, each to be
#'   included in the `<head>`, or *before* or *after* the HTML content in
#'   `<body>`.
#' @export
include_script <- function(head = NULL, before = NULL, after = NULL) {
  x <- purrr::compact(
    list(head = head, before = before, after = after)
  )
  if (length(x)) x
}

args_scripts <- function(script) {
  args <- c()
  for (item in c("head", "before", "after")) {
    if (is.null(script[[item]])) next
    for (src in rev(script[[item]])) {
      args <- c(args, "-V", paste0("script-", item, "=", src))
    }
  }
  args
}
