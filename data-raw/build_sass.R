build_sass <- function() {
  sass_files <- css_files <- c("inst/template-html/css/colors.scss")
  fs::path_ext(css_files) <- "css"
  for (i in seq_along(sass_files)) {
    sass::sass(
      sass::sass_file(here::here(sass_files[i])),
      output = here::here(css_files[i])
    )
  }
}

build_sass()
