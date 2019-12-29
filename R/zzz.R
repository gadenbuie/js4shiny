# Override the knitr js engine
.onLoad <- function(libname, pkgname) {
  warn_rmarkdown_2()
}
