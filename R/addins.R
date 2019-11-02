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

#' Choose Launch Location for Shiny Apps
#'
#' This function sets the `shiny.launch.browser` option to launch Shiny apps in
#' an `"external"` browser, the RStudio viewer `"pane"`, or a new `"window"` in
#' RStudio.
#'
#' @param where One of `"external"`, `"pane"`, or `"window"`.
#' @export
launch_shiny_in <- function(where = c("external", "pane", "window")) {
  requires_pkg("rstudioapi")
  if (!isTRUE(rstudioapi::hasFun("getSourceEditorContext"))) {
    stop("Must be called from RStudio")
  }
  options(shiny.launch.browser = switch(
    match.arg(where),
    external = get(".rs.invokeShinyWindowExternal", "tools:rstudio"),
    pane = get(".rs.invokeShinyPaneViewer", "tools:rstudio"),
    window = get(".rs.invokeShinyWindowViewer", "tools:rstudio")
  ))
}
