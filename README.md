# js4shiny <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->
<!-- badges: end -->

`js4shiny` is a companion package for the **JavaScript for Shiny Users** rstudio::conf(2020) workshop.

&#x1F6A7; This package is a work in progress but feel free to install and try it out. Just please be prepared to update it frequently and [report any issues](https://github.com/gadenbuie/js4shiny/issues)!

## Installation

You can install js4shiny from GitHub with

``` r
devtools::install_github("gadenbuie/js4shiny")
```

...more details soon

## Literate JavaScript Programming with R Markdown

This package includes several components that enable the use of R Markdown for literate programming with JavaScript. The first of these is the R Markdown HTML format, `js4shiny::html_document_js()`. In this R Markdown format, JavaScript chunks are executed in the browser and logged messages and outputs are displayed in the document, much like a standard R chunk.

For more information see `vignette("literate-javascript")`.
