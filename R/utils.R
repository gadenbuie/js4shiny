
js4shiny_file <- function(...) {
  system.file(..., package = "js4shiny", mustWork = TRUE)
}

requires_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue("`{pkg}` is required: install.packages('{pkg}')"))
  }
}
