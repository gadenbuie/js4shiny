test_that("resources round trip through yaml", {
  ex_yaml <- '
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
'

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
