`%||%` <- function(x, y) if(is.null(x)) y else x

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

read_lines <- function(path, ...) readLines(path, warn = FALSE, ...)
