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

This package includes several components that enable the use of R Markdown for literate programming with JavaScript. The first of these is the R Markdown HTML format, `js4shiny::html_document_js()`.

This creates an R Markdown document that renders HTML, with a new JavaScript engine
that, when used with the default settings, each JavaScript chunk is run in it's own environment and any output written with `console.log()` is inserted in the HTML document as the code runs. In this setting, the JavaScript is rendered directly in the browser at view time.

````
---
output: js4shiny::html_document_js
---

```{js}
let x = 10
console.log("multiplying x times 10...")
x * 20
```
````

When the document is viewed in a browser, 
each JavaScript chunk is evaluated in its own block scope 
and the return value of the block is automatically printed to the chunk's output `<div>`,
unless that value is `undefined`.

Because each chunk is block-scoped,
variables created in one block may not be available to other chunks.
However, you can create global chunks by temporarily disabling the console redirect
by adding the `js_redirect = FALSE` to the chunk that you would like to be evaluated in the global scope.
This option disables the `console.log()` redirect 
and uses the standard JavaScript engine included in the `knitr` package. 
Logged statements will still be available in the browser's developer tools console, 
and variables created in the global scope are thereafter available to all chunks.

````
```{js, js_redirect = FALSE}
let globalVariable = 'this is a global variable'
console.log(globalVariable) // goes to browser console
```

```{js}
console.log(globalVariable) // outputs in document
```
````

Within the document, 
it may not be clear which chunks are evaluated globally,
so I recommend declaring global variables with `let` instead of `const`.

It's also possible to send the JavaScript code to `node` by setting the `js_live = FALSE` chunk option. 
The JavaScript code is then run using `node` at compile time 
and each chunk evaluated in a separate process.
In this setting, the results printed by the `node` process are captured and stored in the document, 
resulting in a static output that captures the results of the JavaScript run-time code.

````
```{js, js_live = FALSE}
let x = 10
console.log(x * 20)
```
````
