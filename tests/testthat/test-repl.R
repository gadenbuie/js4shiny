ex_yaml <- "
---
output:
  js4shiny::html_document_plain:
    stand_alone: true
    css:
      - bootstrap@3.4.1/dist/css/bootstrap.min.css
      - bootstrap@3.3.7/dist/css/bootstrap.min.css
    script:
      head:
        - script-head.js
      before:
        - script-before.js
      after:
        - script-after-1.js
        - script-after-2.js
---
"

test_that("resources round trip through yaml with js4shiny::html_document_plain", {
  tmp <- tempfile()
  cat(ex_yaml, file = tmp)

  yaml <- extract_yaml(tmp)
  res_from_yaml <- extract_resources(tmp)
  yaml_from_res <- resource_to_js4shiny_yaml(res_from_yaml)

  expect_equal(res_from_yaml[[1]]$path, "bootstrap@3.4.1/dist/css/bootstrap.min.css")
  expect_equal(res_from_yaml[[2]], list(path = "bootstrap@3.3.7/dist/css/bootstrap.min.css", type = "css", where = "head"))
  expect_equal(res_from_yaml[[3]], list(path = "script-head.js", type = "javascript", where = "head"))
  expect_equal(res_from_yaml[[4]], list(path = "script-before.js", type = "javascript", where = "before"))

  expect_equal(yaml_from_res, yaml$output[["js4shiny::html_document_plain"]][c("css", "script")])
})

test_that("resources round trip through yaml with js4shiny::html_document_js4shiny", {
  ex_yaml <- sub("html_document_plain", "html_document_js4shiny", ex_yaml, fixed = TRUE)
  tmp <- tempfile()
  cat(ex_yaml, file = tmp)

  yaml <- extract_yaml(tmp)
  res_from_yaml <- extract_resources(tmp)
  yaml_from_res <- resource_to_js4shiny_yaml(res_from_yaml)

  expect_equal(res_from_yaml[[1]]$path, "bootstrap@3.4.1/dist/css/bootstrap.min.css")
  expect_equal(res_from_yaml[[2]], list(path = "bootstrap@3.3.7/dist/css/bootstrap.min.css", type = "css", where = "head"))
  expect_equal(res_from_yaml[[3]], list(path = "script-head.js", type = "javascript", where = "head"))
  expect_equal(res_from_yaml[[4]], list(path = "script-before.js", type = "javascript", where = "before"))

  expect_equal(yaml_from_res, yaml$output[["js4shiny::html_document_js4shiny"]][c("css", "script")])
})


test_that("repl() and repl_example() force external browser when needed", {
  suppressWarnings(s <- repl("bootstrap"))

  expect_equal(s, repl_example("bootstrap"))
  expect_identical(s$options$launch.browser, TRUE)

  # set shiny.launch.browser to a fixed but nonsense value
  opt <- options("shiny.launch.browser" = function() "apple")
  s_internal <- repl("css-variables")
  expect_equal(s_internal, repl_example("css-variables"))
  expect_identical(s_internal$options$launch.browser(), "apple")
  options(opt)
})

test_that("repl() warns if multiple examples are requested", {
  expect_warning(s <- repl(c("bootstrap", "css-variables")))
})

test_that("repl() warns or stops when the example isn't repl format", {
  expect_warning(s <- repl("shiny"), "isn't designed for repl()")
  expect_warning(s <- repl("apple"), "does not exist")
  expect_error(
    s <- repl(js4shiny_file("examples", "shiny", "shiny-demo-apps", "shiny-starter-app", "app.R")),
    "isn't in a format .+ expect"
  )
})

describe("is_repl_foramt()", {
  file_repl <- js4shiny_file("examples", "css", "css-basics", "css-variables.Rmd")
  file_not <- js4shiny_file("examples", "shiny", "shiny-demo-apps", "shiny-starter-app", "app.R")
  dir_repl <- js4shiny_file("examples", "css", "css-basics")

  it ("returns TRUE for files in correct format", {
    expect_true(is_repl_format(file_repl))
  })

  it ("returns FALSE for file not in correct format", {
    expect_false(is_repl_format(file_not))
    expect_false(is_repl_format(dir_repl))
  })

  it ("returns vector TRUE/FALSE", {
    expect_identical(
      is_repl_format(c(file_repl, file_not)),
      c(TRUE, FALSE)
    )
  })
})

test_that("choose_example('css')", {
  expect_equal(
    search_for_example("css"),
    c(css = js4shiny_file("examples", "css"))
  )
})
