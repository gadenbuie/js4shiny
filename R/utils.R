
js4shiny_file <- function(...) {
  system.file(..., package = "js4shiny", mustWork = TRUE)
}
