# Override the knitr js engine
.onLoad <- function(libname, pkgname) {
  # packageStartupMessage("Overriding js engine with js4shiny::knitr_js_engine")
  register_knitr_js_engine()
}
