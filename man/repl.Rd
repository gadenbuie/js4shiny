% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addins.R, R/repl.R
\name{repl_example}
\alias{repl_example}
\alias{repl}
\alias{repl_js}
\title{REPL for live JS, CSS, and HTML development}
\usage{
repl_example(example = NULL)

repl(
  example = NULL,
  js_repl_only = FALSE,
  theme_app = NULL,
  theme_editor = "textmate",
  autocomplete = c("css", "html"),
  render_dir = NULL,
  options = list(),
  ...
)

repl_js(..., render_dir = NULL)
}
\arguments{
\item{example}{The short name of the exercise or example, e.g.
\code{ride-share-fares}. Alternatively, the path to a folder containing examples
or the path to an example file directly. \code{repl_example(example = NULL)}
opens an interactive browser to select an example, otherwise \code{repl()} and
\code{repl_js()} will open with blank editors.}

\item{js_repl_only}{When \code{TRUE}, the app is simplified to contain only a
JavaScript source editor and a console output. \code{repl_js()} is an alias to
launch \code{repl()} with \code{js_repl_only = TRUE}.}

\item{theme_app}{The theme of the app, using \pkg{shinythemes}. See
\code{\link[shinythemes:shinytheme]{shinythemes::shinytheme()}} for a list of valid themes.}

\item{theme_editor}{The theme of the \pkg{shinyAce} source code editors. See
\code{\link[shinyAce:getAceThemes]{shinyAce::getAceThemes()}} for a list of valid themes.}

\item{autocomplete}{Ace Editor language modes for which autocomplete will be
enabled. One or more of \code{"js"}, \code{"css"}, or \code{"html"}. By default
autocomplete is enabled in all but the JavaScript mode. "Disabling"
autocomplete here actually doesn't mean disabling all together.
Autocomplete will still be available by pressing \code{Ctrl} + \code{space}.}

\item{render_dir}{Where to render temporary files, defaults to \code{tempdir()}}

\item{options}{Options passed to \code{\link[shiny:runApp]{shiny::runApp()}}.}

\item{...}{Arguments passed from \code{repl_js()} to \code{repl()} or from \code{repl()} to
\code{\link[shiny:shinyApp]{shiny::shinyApp()}}.}
}
\value{
A shiny app
}
\description{
Launches an interactive Shiny app for live editing of frontend JavaScript,
CSS, and HTML/Markdown/R Markdown. The app allows users to write JS, CSS and
HTML, preview the final product, observe the JavaScript console (specifically
items printed to the console via \code{console.log()}), and download a zip file
containing the source files.
}
\section{Functions}{
\itemize{
\item \code{repl_example}: Launch a \pkg{js4shiny} exercise or example using the
example slug, or the full filename. If none provided, \code{repl_example()}
launches an interactive example browser.
}}

\section{Examples for js4shiny workshop}{

The app was developed for the \strong{js4shiny} rstudio::conf workshop and can be
used to load examples for practicing and learning JavaScript and web
development concepts.
}

