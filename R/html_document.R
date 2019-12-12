#' An HTML Document with Support for Literate JavaScript Programming
#'
#' @description An R Markdown format for literate JavaScript programming. With
#' default settings, each JavaScript chunk is run in it's own environment and
#' any output written with `console.log()` is inserted in the HTML document as
#' the code runs. In this setting, the JavaScript is rendered directly in the
#' browser at view time.
#'
#' A similar effect can be achieved by using the `js_live = FALSE` chunk option
#' to instead run the JavaScript code using `node` at compile time. In this
#' setting, the results printed by the `node` process are captured and stored in
#' the document, resulting in a non-dynamic output that captures the results of
#' the JavaScript runtime code.
#'
#' In both of the above settings, each code chunk is run separately. You can use
#' the `js_redirect = FALSE` knitr chunk option to disable the `console.log()`
#' redirect and use the standard JavaScript engine included in the \pkg{knitr}
#' package. Logged statements will still be available in the browser's
#' devolper tools console, as this engine is equivalent to having entered
#' the JavaScript code directly into the HTML source within a `<script>` tag.
#'
#' @param highlight One of the pandoc highlight styles.
#' @param use_fontawesome Should FontAwesome be included? Default is `FALSE`.
#' @inheritParams rmarkdown::html_document
#' @export
html_document_js <- function(
  ...,
  theme = NULL,
  css = NULL,
  toc = FALSE,
  toc_depth = 3,
  mathjax = NULL,
  use_fontawesome = FALSE,
  highlight = "haddock",
  fig_width = 10,
  fig_height = 7,
  fig_retina = 2,
  keep_md = FALSE,
  dev = "png",
  pandoc_args = NULL,
  extra_dependencies = NULL
) {
  if (!is.null(theme)) {
    warning("theme is ignored in html_document_js()", call. = FALSE)
  }

  deps <- c(html_dependency_js4shiny(), extra_dependencies)

  # disable fontawesome if !use_fontawesome
  # add to pandoc_args rmarkdown::pandoc_toc_args(toc, toc_depth)
  pandoc_args <- c(
    pandoc_args,
    if (!use_fontawesome) c("--variable", "disable-fontawesome"),
    c("--highlight-style", highlight),
    rmarkdown::pandoc_toc_args(toc, toc_depth)
  )

  mathjax_url <- if (!is.null(mathjax) && mathjax %in% c("default", "local")) {
    mathjax_local <- Sys.getenv("RMARKDOWN_MATHJAX_PATH", unset = NA)
    if (mathjax == "local" && is.na(mathjax_local)) {
      warning(
        paste(
          "Please use `Sys.setenv('RMARKDOWN_MATHJAX_PATH')` to set local mathjax location.",
          "Falling back to online mathjax from https://mathjax.rstudio.com"
        )
      )
    }
    mathjax_path <- ifelse(
      mathjax == "default" || is.na(mathjax_local),
      "https://mathjax.rstudio.com/latest",
      mathjax_local
    )
    file.path(mathjax_path, "MathJax.js?config=TeX-AMS-MML_HTMLorMML")
  } else {
    mathjax
  }

  if (!is.null(mathjax_url)) {
    pandoc_args <- c(
      pandoc_args,
      "--mathjax",
      "--variable",
      paste0("mathjax-url:", mathjax_url)
    )
  }

  # knit_hooks <- knitr::knit_hooks$get()
  # knit_hooks$chunk <- register_knitr_output_hooks(set = FALSE)
  register_knitr_js_engine()

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
      from = "markdown+ascii_identifiers+tex_math_single_backslash",
      args = c(
        if (!is.null(css)) paste("--css", css),
        pandoc_args,
        "--template",
        js4shiny_file("template-html", "js4shiny.html")
      ),
    ),
    clean_supporting = FALSE,
    base_format = rmarkdown::html_document_base(
      template = NULL,
      theme = NULL,
      mathjax = mathjax,
      extra_dependencies = deps,
      ...
    )
  )
}

