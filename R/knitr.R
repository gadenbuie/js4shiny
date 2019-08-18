#' A JavaScript Engine for knitr
#'
#' @export
knitr_js_engine <- function() {
  function(options) {
    js_escape <- function(x) {
      x <- gsub('([`$])', '\\\\\\1', x)
      paste0("`", paste(x, collapse = "\n"), "`")
    }

    out <- if (options$eval && knitr::is_html_output(excludes = 'markdown')) {
      out_id <- paste0("out-", options$label)
      out_logger <- paste0("log_", gsub("[^a-zA-Z0-9]", "_", out_id))
      paste(c(
        paste0('<div><pre id="', out_id, '"></pre></div>'),
        '<script type="text/javascript">',
        paste0(
          'const ', out_logger, ' = redirectLogger(document.getElementById("', out_id, '"))'
        ),
        paste0(out_logger, '(', js_escape(options$code), ')'),
        '</script>\n'
      ), sep = "\n", collapse = "\n")
    }

    options$results <- 'asis'
    knitr::engine_output(options, options$code, out)
  }
}

#' Register js4shiny knitr output hooks
#'
#' @export
register_knitr_output_hooks <- function() {
  chunk_hook <- knitr::knit_hooks$get("chunk")
  chunk_name_hook <- function(x, options) {
    is_html <- knitr::is_html_output(excludes = "markdown")
    has_name <- !is.null(options$name)
    if (options$echo && is_html && has_name) {
      x <- paste0('<div class="pre-name">', options$name, '</div>\n', x)
    }
    chunk_hook(x, options)
  }
  knitr::knit_hooks$set(chunk = chunk_name_hook)
}
