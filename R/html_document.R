#' An HTML Document with Support for Literate JavaScript Programming
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

  deps <- c(
    list(
      htmltools::htmlDependency(
        name = "js4shiny",
        package = "js4shiny",
        version = utils::packageVersion("js4shiny"),
        src = "template-html",
        script = "redirectConsoleLog.js",
        stylesheet = c("stylize.css", "jslog.css")
      )
    ),
    extra_dependencies
  )

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

  rmarkdown::output_format(
    knitr = rmarkdown::knitr_options_html(
      fig_width,
      fig_height,
      fig_retina,
      keep_md,
      dev
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

