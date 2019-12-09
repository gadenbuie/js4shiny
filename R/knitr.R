#' A JavaScript Engine for knitr
#'
#' @export
knitr_js_engine <- function() {
  function(options) {
    js_escape <- function(x) {
      x <- gsub('([`$])', '\\\\\\1', x)
      x <- gsub("\\\\n", "\\\\\\\\n", x)
      x <- gsub("\\\\t", "\\\\\\\\t", x)
      x <- gsub("\\\\r", "\\\\\\\\r", x)
      paste0("`", paste(x, collapse = "\n"), "`")
    }

    browser(expr = getOption("js4shiny.js_engine_debug", FALSE))
    redirect <- is.null(options$js_redirect) || isTRUE(options$js_redirect)

    if (!redirect) return(default_js_engine(options))

    eval_live <- options$eval && (is.null(options$js_live) || options$js_live)

    out <- if (eval_live && knitr::is_html_output(excludes = 'markdown')) {
      out_id <- glue("out-{options$label}")
      out_logger <- glue('log_{gsub("[^a-zA-Z0-9]", "_", out_id)}')
      js_code <- js_escape(options$code)
      paste(c(
        glue('<div id="{out_id}"><pre></pre></div>'),
        '<script type="text/javascript">',
        glue('const {out_logger} = redirectLogger(document.querySelector("#{out_id} > pre"))'),
        paste0(
          'document.addEventListener("DOMContentLoaded", function() {\n',
          out_logger, "(", js_code, ")\n",
          '})'
        ),
        '</script>\n'
      ), sep = "\n", collapse = "\n")
    } else if (options$eval && has_node()) {
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

default_js_engine <- function(options) {
  # From https://github.com/yihui/knitr/blob/master/R/engine.R
  prefix = '<script type="text/javascript">'
  postfix = "</script>"
  out = if (options$eval && knitr::is_html_output(excludes = 'markdown')) {
    paste(c(prefix, options$code, postfix), collapse = "\n", sep = "\n")
  }
  options$results = 'asis'
  knitr::engine_output(options, options$code, out)
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


knitr_json_engine <- function() {
  function(options) {

    out <- if (options$eval && knitr::is_html_output(excludes = 'markdown')) {
      label <- gsub("[^a-zA-Z0-9_.]", "_", options$label)
      code <- paste(options$code, collapse = "\n")
      paste(
        glue('<div id="json-{label}"></div>'),
        "<script>",
        glue("let data_{label} = {code}"),
        'document.addEventListener("DOMContentLoaded", function() {',
        glue('  window.jsonView.format(data_{label}, "#json-{label}")'),
        "})",
        "</script>",
        sep = "\n"
      )
    }

    options$results <- "asis"
    knitr::engine_output(options, options$code, htmltools::HTML(out))
  }
}

knitr_html_engine <- function() {
  function(options) {
    out <- if (options$eval && knitr::is_html_output()) {
      paste0(
        '\n```{=html}\n',
        options$code,
        "\n```"
      )
    }
    options$results <- "asis"
    knitr::engine_output(options, options$code, out)
  }
}

#' Register js4shiny knitr components
#'
#' Register the js4shiny knitr JavaScript engine or the output hooks. Generally,
#' you will not need to use these. Instead, see [html_document_js()] or
#' [html_setup()] for methods that cover most use-cases.
#'
#' @name register_knitr
NULL

#' @rdname register_knitr
#' @param set If `FALSE` the output hook or JS engine are returned rather than
#'   setting via knitr directly.
#' @export
register_knitr_output_hooks <- function(set = TRUE) {
  if (set) chunk_hook <- knitr::knit_hooks$get("chunk")
  chunk_name_hook <- function(x, options) {
    is_html <- knitr::is_html_output(excludes = "markdown")
    has_name <- !is.null(options$name)
    if (options$echo && is_html && has_name) {
      x <- paste0('<div class="pre-name">', options$name, '</div>', x)
    }
    if (!set) x else chunk_hook(x, options)
  }
  if (!set) return(chunk_name_hook)
  knitr::knit_hooks$set(chunk = chunk_name_hook)
}

#' @rdname register_knitr
#' @export
register_knitr_js_engine <- function(set = TRUE) {
  # message("over-riding knitr js engine!")
  if (!set) return(knitr_js_engine)
  knitr::knit_engines$set(js = knitr_js_engine())
  knitr::knit_engines$set(json = knitr_json_engine())
  knitr::knit_engines$set(html = knitr_html_engine())
}
