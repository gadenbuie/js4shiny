test_that("the js_lint option doesn't break things", {
  js_eng <- knitr_js_engine()
  with_mock(
    js_lint = function(code, x, y) list(code = code),
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
    js_lint = function(code, x, y) list(code = code),
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
