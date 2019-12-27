`%||%` <- function(x, y) if (is.null(x)) y else x

null_if_nothing <- function(x) {
  if (is.null(x) || x == "") NULL else x
}

tabs2spaces <- function(x, spaces = 2) {
  if (is.null(x)) return(NULL)
  gsub("\t", strrep(" ", spaces), x)
}

js4shiny_file <- function(...) {
  system.file(..., package = "js4shiny", mustWork = TRUE)
}

requires_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue("`{pkg}` is required: install.packages('{pkg}')"))
  }
}

collapse <- function(..., sep_c = "\n") paste(..., collapse = sep_c)
collapse0 <- function(..., sep_c = "\n") paste(..., sep = "", collapse = sep_c)

read_lines <- function(path, ..., warn = FALSE) readLines(path, warn = warn, ...)

escape_html_example <- function(...) {
  x <- list(...)
  x <- purrr::map_chr(x, ~ if (inherits(.x, "AsIs")) paste(.x) else htmltools::htmlEscape(.x))
  x <- gsub("\n", "<br>", x)
  x <- gsub(" ", "&nbsp;", x)
  collapse(x, sep_c = "")
}

write_registry_yaml <- function(
  path,
  title,
  description = title,
  type = c("default", "shiny", "shiny-starter", "shiny-run", "html", "html-external")
) {
  if (!fs::is_dir(path)) {
    stop("`path` should be a directory where 'registry.yml' will be written")
  }
  type <- match.arg(type)
  info <- list(
    title = title,
    type = type,
    description = description
  )
  if (type == "default") info$type <- NULL
  yaml::write_yaml(info, file = fs::path(path, "registry.yml"))
}
