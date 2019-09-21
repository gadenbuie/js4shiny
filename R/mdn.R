#' Search the Mozilla Developers Network
#'
#' Searches the MDN Web Docs for a search term. Includes an RStudio addin for
#' quick and seamless searching: highlight a term in the source code and run the
#' addin to search MDN. If no text is highlighted, the addin opens a Shiny
#' gadget for you to enter your search term.
#'
#' @param term Search term
#' @param browse Should the search results be opened in a browser window? If
#'   not, the URL is returned instead.
#' @param topics A selection of topics to search, choosing from `"js"`, `"api"`,
#'   `"css"`, `"html"`, `"svg"`, `"mobile"`, `"canvas"`, `"webdev"`, and
#'   `"standards"`.
#' @param locale The locale string for the search query. Default is `"en-US"`,
#'   or set to `NULL` to try searching based on the system locale.
#' @return The search URL
#' @export
mdn_search <- function(
  term,
  browse = TRUE,
  topics = c("js", "api", "css", "html", "svg"),
  locale = "en-US"
) {
  term <- utils::URLencode(term)
  topics <- if (!is.null(topics)) {
    topics <- match.arg(topics, unname(mdn_topics), several.ok = TRUE)
    paste0("&topic=", topics, collapse = "")
  } else {
    ""
  }
  url <- glue("https://developer.mozilla.org/{locale}/search?q={term}&highlight=true{topics}")
  if (browse) utils::browseURL(url)
  url
}

mdn_topics <- c(
  "JavaScript" = "js",
  "API & DOM" = "api",
  "CSS" = "css",
  "HTML" = "html",
  "SVG" = "svg",
  "Mobile" = "mobile",
  "Canvas" = "canvas",
  "Web Dev" = "webdev",
  "Web Standards" = "standards"
)

mdn_addin <- function() {
  ctx <- get_source_context("Are you using RStudio?")

  if (length(ctx$selection) > 1) {
    stop("Please select a single text string")
  } else if (ctx$selection[[1]]$text == "") {
    mdn_gadget(browse = TRUE)
  } else {
    term <- ctx$selection[[1]]$text
    topics <- c("js", "api", "css", "html", "svg")
    mdn_browse(term, browse = TRUE, topics = topics, locale = NULL)
  }
}

mdn_browse <- function(term, browse, topics, locale) {
  if (is.null(term)) {
    return()
  }

  if (term == "") {
    if (browse) utils::browseURL("https://developer.mozilla.org")
    return()
  }

  if (is.null(locale)) locale <- guess_locale()
  if (browse) {
    invisible(mdn_search(term, TRUE, topics, locale = locale))
  } else {
    mdn_search(term, FALSE, topics, locale)
  }
}

guess_locale <- function() {
  locale <- Sys.getenv("LANG")
  locale <- strsplit(locale, "\\.")[[1]][1]
  gsub("_", "-", locale)
}

#' @describeIn mdn_search A Shiny gadget for searching the MDN docs.
#' @export
mdn_gadget <- function(browse = TRUE, locale = NULL) {
  ui <- shiny::fluidPage(
    shiny::tags$div(
      class = "search-container",
      shiny::tags$div(class = "logo"),
      shiny::textInput(
        "term",
        "Search Mozilla Developers Network",
        width = "100%"
      ),
      shiny::tags$div(
        shiny::checkboxGroupInput(
          inputId = "topics",
          label = "Topics",
          inline = TRUE,
          choices = mdn_topics,
          selected = c("api", "css", "html", "js")
        )
      ),
      shiny::tags$div(
        class = "form-group",
        shiny::actionButton("cancel", "Cancel"),
        shiny::actionButton(
          "search",
          "Search",
          icon = shiny::icon("search"),
          class = "btn-primary pull-right"
        )
      )
    ),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "mdn-addin/mdn.css"),
    shiny::tags$script(type = "text/javascript", src = "mdn-addin/mdn.js")
  )

  server <- function(input, output) {
    shiny::observeEvent(input$cancel, shiny::stopApp())
    shiny::observeEvent(input$search, shiny::stopApp(
      list(term = input$term, topics = input$topics)
    ))
    shiny::observeEvent(input$keyEvent, {
      switch(
        input$keyEvent,
        cancel = shiny::stopApp(),
        search = shiny::stopApp(list(term = input$term, topics = input$topics))
      )
    })
  }

  shiny::addResourcePath("mdn-addin", js4shiny_file("mdn-addin"))
  res <- shiny::runGadget(
    app = ui,
    server = server,
    viewer = shiny::dialogViewer("Search MDN", width = 500, height = 300)
  )

  mdn_browse(res$term, browse, res$topics, locale)
}
