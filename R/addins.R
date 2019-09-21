live_preview <- function(path = getwd(), pattern = "[.](js|css|html?|s[ca]ss)$", ..., external = FALSE) {
  requires_pkg("servr")
  path_dir <- dirname(path)
  path_file <- basename(path)
  viewer <- if (external) browseURL else getOption("viewer", browseURL)
  x <- servr::httw(path_dir, pattern = pattern, initpath = path_file, browser = FALSE, ...)
  viewer(x$url)
}

live_preview_addin <- function() {
  ctx <- get_source_context("The live preview addin only works in RStudio.")
  live_preview(ctx$path, external = FALSE)
}

live_preview_external_addin <- function() {
  ctx <- get_source_context("The live preview addin only works in RStudio.")
  live_preview(ctx$path, external = TRUE)
}

get_source_context <- function(error_msg = "Requires RStudio") {
  requires_pkg("rstudioapi")
  if (!isTRUE(rstudioapi::hasFun("getSourceEditorContext"))) {
    stop(error_msg)
  }
  rstudioapi::getSourceEditorContext()
}
