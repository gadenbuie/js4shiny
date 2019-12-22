`%||%` <- function(x, y) if (is.null(x)) y else x

null_if_nothing <- function(x) {
  if (is.null(x) || x == "") NULL else x
}

js4shiny_file <- function(...) {
  system.file(..., package = "js4shiny", mustWork = TRUE)
}

requires_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue("`{pkg}` is required: install.packages('{pkg}')"))
  }
}

collapse <- function(..., sep_c = '\n') paste(..., collapse = sep_c)
collapse0 <- function(..., sep_c = "\n") paste(..., sep = "", collapse = sep_c)

read_lines <- function(path, ..., warn = FALSE) readLines(path, warn = warn, ...)

escape_html_example <- function(...) {
  x <- list(...)
  x <- purrr::map_chr(x, ~ if (inherits(.x, "AsIs")) paste(.x) else htmltools::htmlEscape(.x))
  x <- gsub("\n", "<br>", x)
  x <- gsub(" ", "&nbsp;", x)
  collapse(x, sep_c = "")
}
