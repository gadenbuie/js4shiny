#' A JavaScript Engine for knitr
#'
#' @export
knitr_js_engine <- function() {
  function(options) {
    js_escape <- function(x) {
      x <- gsub('([`$])', '\\\\\\1', x)
      paste0("`", paste(x, collapse = "\n"), "`")
    }

    eval_live <- options$eval && (is.null(options$js_live) || options$js_live)

    out <- if (eval_live && knitr::is_html_output(excludes = 'markdown')) {
      out_id <- glue("out-{options$label}")
      out_logger <- glue('log_{gsub("[^a-zA-Z0-9]", "_", out_id)}')
      js_code <- js_escape(options$code)
      browser(expr = getOption("js4shiny.js_engine_debug", FALSE))
      paste(c(
        glue('<div><pre id="{out_id}"></pre></div>'),
        '<script type="text/javascript">',
        glue('const {out_logger} = redirectLogger(document.getElementById("{out_id}"))'),
        paste0(
          'document.addEventListener("DOMContentLoaded", function() {\n',
          out_logger, "(", js_code, ")\n",
          '})'
        ),
        '</script>\n'
      ), sep = "\n", collapse = "\n")
    } else if (has_node()) {
      paste(
        "```",
        paste(run_node(options$code), collapse = "\n"),
        "```",
        sep = "\n"
      )
    }

    options$results <- 'asis'
    knitr::engine_output(options, options$code, out)
  }
}

has_node <- function() {
  tryCatch({
    system("node -v", intern = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

run_node <- function(code) {
  browser(expr = getOption("js4shiny.debug_run_node", FALSE))
  tmp_file <- tempfile(fileext = "js")
  cat(code, file = tmp_file, sep = "\n")
  tryCatch({
    system(glue('node {tmp_file}'), intern = TRUE)
  }, error = function(e) e$message)
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
