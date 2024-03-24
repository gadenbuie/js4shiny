local_edition(2)

skip_if_not_on_my_machine <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("MY_MACHINE", FALSE), "TRUE"),
    "Hey, this test worked on my machine ¯\\_(ツ)_/¯"
  )
}

test_that("the js_lint option doesn't break things", {
  skip_if_not_on_my_machine()
  js_eng <- knitr_js_engine()
  with_mock(
    "js4shiny:::js_lint" = function(code, x, y) list(code = code),
    has_node = function() FALSE,
    expect_equal(
      js_eng(list(
        js_redirect = TRUE,
        eval = TRUE,
        echo = TRUE,
        js_lint = NULL,
        code = "console.log('test')",
        label = "test",
        engine = "js"
      )),
      "console.log('test')"
    )
  )

  with_mock(
    "js4shiny:::js_lint" = function(code, x, y) list(code = code),
    has_node = function() FALSE,
    expect_equal(
      js_eng(list(
        js_redirect = TRUE,
        eval = TRUE,
        echo = TRUE,
        js_lint = "standard",
        code = "console.log('test')",
        label = "test",
        engine = "js"
      )),
      "console.log('test')"
    )
  )
})


describe("knitr_html_engine()", {
  chunk <- list(
    eval = TRUE,
    echo = TRUE,
    code = "<span>TEST</span>",
    label = "test",
    engine = "html"
  )
  html <- knitr_html_engine()

  it("creates a raw html chunk as output", {
    with_mock(
      "knitr::is_html_output" = function(...) TRUE, {
        expect_equal(html(chunk), paste(
          chunk$code,
          "\n```{=html}\n<div id=\"out-test\">\n<span>TEST</span>\n</div>\n```",
          "", sep = "\n"
        ))
      }
    )
  })

  it("creates a raw html chunk without the source when echo=FALSE", {
    with_mock(
      "knitr::is_html_output" = function(...) TRUE, {
        chunk$echo <- FALSE
        expect_equal(html(chunk), paste(
          "\n```{=html}\n<div id=\"out-test\">\n<span>TEST</span>\n</div>\n```",
          "", sep = "\n"
        ))
      }
    )
  })

  it("creates an output div if html_raw = FALSE", {
    with_mock(
      "knitr::is_html_output" = function(...) TRUE, {
        chunk$html_raw <- FALSE
        expect_equal(html(chunk), paste(
          chunk$code,
          "\n<div id=\"out-test\">\n<span>TEST</span>\n</div>",
          "", sep = "\n"
        ))
      }
    )
  })

  it("creates just the html chunk if eval=FALSE", {
    with_mock(
      "knitr::is_html_output" = function(...) TRUE, {
        chunk$eval <- FALSE
        expect_equal(html(chunk), chunk$code)
      }
    )
  })

  it("uses the class.output option", {
    with_mock(
      "knitr::is_html_output" = function(...) TRUE, {
        chunk$html_raw <- FALSE
        chunk$class.output <- "MYCLASS"
        expect_equal(html(chunk), paste(
          chunk$code,
          "\n<div id=\"out-test\" class=\"MYCLASS\">\n<span>TEST</span>\n</div>",
          "", sep = "\n"
        ))
      }
    )
  })
})

test_that("knitr html() engine", {
  skip_if_not(rmarkdown::pandoc_available("1.12.3"))

  tmpfile <- tempfile(fileext = ".html")
  on.exit(unlink(tmpfile))
  rmarkdown::render(
    input = test_path("html-engine/html-engine.Rmd"),
    output_file = tmpfile,
    quiet = TRUE,
    envir = new.env()
  )
  result <- read_lines(tmpfile)

  expect_detect <- function(pattern, fixed = TRUE, ...) {
    expect_true(sum(grepl(!!pattern, result, fixed = fixed, ...)) == 1)
  }
  expect_missing <- function(pattern, fixed = TRUE, ...) {
    expect_false(sum(grepl(!!pattern, result, fixed = fixed, ...)) == 1)
  }

  expect_detect("&gt;</span>TEST ONE<span")
  expect_detect('<div id="out-TEST">')
  expect_detect("&gt;</span>TEST EVAL FALSE")
  expect_missing("<span>TEST EVAL FALSE</span>")
  expect_detect("&gt;</span>TEST RAW FALSE")
  expect_detect("<span>TEST RAW FALSE</span>")
  expect_detect("&gt;&lt;/.*?>script<.*&gt;", fixed = FALSE)
  expect_missing('<script src="script.js"></script>')
})
