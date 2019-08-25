# js4shiny

<!-- badges: start -->
<!-- badges: end -->

`js4shiny` is a companion package for the **JavaScript for Shiny Users** rstudio::conf(2020) workshop.

## Installation

You can install js4shiny from GitHub with

``` r
devtools::install_github("gadenbuie/js4shiny")
```

...more details soon

## Literate JavaScript Programming with R Markdown

This package includes several components that enable the use of R Markdown for literate programming with JavaScript. The first of these is the R Markdown HTML format, `js4shiny::html_document_js()`.

This creates an R Markdown document that renders HTML, with a new JavaScript engine
that, when used with the default settings, each JavaScript chunk is run in it's own environment and any output written with `console.log()` is inserted in the HTML document as the code runs. In this setting, the JavaScript is rendered directly in the browser at view time.

````
---
output: js4shiny::html_document_js
---

```{r setup, include=FALSE}
js4shiny::register_knitr_output_hooks()
```

```{js}
let x = 10
console.log(x * 20)
```
````

A similar effect can be achieved by using the `js_live = FALSE` chunk option to instead run the JavaScript code using `node` at compile time. In this setting, the results printed by the `node` process are captured and stored in the document, resulting in a non-dynamic output that captures the results of the JavaScript runtime code. 

````
```{js, js_live = FALSE}
let x = 10
console.log(x * 20)
```
````

In both of the above settings, each code chunk is run separately. You can use the `js_redirect = FALSE` knitr chunk option to disable the `console.log()` redirect and use the standard JavaScript engine included in the \pkg{knitr} package. Logged statements will still be available in the browser's devolper tools console, as this engine is equivalent to having entered the JavaScript code directly into the HTML source within a `<script>` tag.

````
```{js, js_redirect = FALSE}
const globalVariable = 'this is a global variable'
console.log(globalVariable) // goes to browser console
```

```{js}
console.log(globalVariable) // outputs in document
```
````
