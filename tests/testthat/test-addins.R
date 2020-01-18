# test_that("addins")

skip_if_not_in_rstudio <- function() {
  testthat::skip_if_not(
    has_rstudio("documentNew"),
    "testing inside an interactive RStudio session"
  )
}

skip_if_not_interactive_console <- function() {
  testthat::skip_if_not(
    interactive() && !has_rstudio("versionInfo"),
    "testing inside an interactive non-RStudio session"
  )
}

# Run these test interactively
#
# Once in RStudio by selecting and Cmd + Enter
#
# Once in the terminal with
# > devtools::test_file("R/addins.R")

describe("open_or_save_example()", {
  ex <- search_for_example("shiny-starter-app")
  ex_html <- search_for_example("pkg-calendar")

  it("opens shiny app examples in an editor", {
    skip_if_not_in_rstudio()

    open_app_example(ex)
    Sys.sleep(1)
    ctx <- rstudioapi::getActiveDocumentContext()
    expect_equal(ctx$contents[1], "# shiny-starter-app/app.R")
    expect_equal(ctx$contents[-1:-2], read_lines(fs::path(ex, "app.R")))
    rstudioapi::documentClose(ctx$id)
  })

  it("opens shiny app examples in a file", {
    skip_if_not_interactive_console()

    ex <- fs::path(ex, "app.R")
    test_file <- open_or_save_file(ex, "app.R", open = FALSE)
    on.exit(unlink(test_file))
    test_text <- read_lines(test_file)
    expect_equal(test_text[1], "# shiny-starter-app/app.R")
    expect_equal(test_text[-1:-2], read_lines(ex))
  })

  it("opens HTML examples in an editor", {
    skip_if_not_in_rstudio()

    tmpfile <- fs::file_temp("index", ext = "html")
    ex_html <- fs::path(ex_html, "index.html")
    ex_html_file <- open_or_save_file(ex_html, tmpfile)
    on.exit(unlink(ex_html_file))
    expect_true(fs::file_exists(ex_html_file))
    Sys.sleep(1)
    ctx <- rstudioapi::getActiveDocumentContext()
    expect_equal(ctx$contents, read_lines(ex_html))
    expect_equal(read_lines(ex_html_file), read_lines(ex_html))
    rstudioapi::documentClose(ctx$id)
  })

  it("opens HTML examples in a file", {
    skip_if_not_interactive_console()

    ex_html <- fs::path(ex_html, "index.html")
    test_file <- open_or_save_file(ex_html, "index.html", open = FALSE)
    on.exit(unlink(test_file))
    test_text <- read_lines(test_file)
    expect_equal(test_text, read_lines(ex_html))
  })

  it("opens live preview of html examples", {
    ex <- search_for_example("css-box-size")
    with_mock(
      live_preview = function(x, external) if (external) x,
      expect_equivalent(ex, open_html_example(ex))
    )
  })
})
