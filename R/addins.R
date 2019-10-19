live_preview <- function(
  path = getwd(),
  pattern = "[.](js|css|[Rr][Mm][Dd]|html?|s[ca]ss)$",
  ...,
  external = FALSE
) {
  requires_pkg("servr")
  path_dir <- dirname(path)
  path_file <- if (grepl("[.]rmd", path, ignore.case = TRUE)) "/" else basename(path)
  viewer <- if (external) {
    utils::browseURL
  } else {
    getOption("viewer", utils::browseURL)
  }
  render <- function(path) {
    if (grepl("[.]rmd", path, ignore.case = TRUE)) {
      rmarkdown::render(path, envir = new.env())
    }
    path
  }
  x <- servr::httw(
    dir = path_dir,
    pattern = pattern,
    initpath = path_file,
    browser = FALSE,
    handler = render,
    ...
  )
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
