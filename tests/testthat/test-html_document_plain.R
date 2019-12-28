test_that("resources loaded in correct order", {
  rmd <- "
---
pagetitle: test html document plain
output:
  js4shiny::html_document_plain:
    self_contained: false
    script:
      head:
        - script-head-1.js
        - script-head-2.js
      before:
        - script-before-1.js
        - script-before-2.js
      after:
        - script-after-1.js
        - script-after-2.js
---
"
  tmprmd <- tempfile(fileext = ".Rmd")
  cat(rmd, file = tmprmd)
  tmpout <- tempfile(fileext = ".html")
  rmarkdown::render(tmprmd, output_file = tmpout, quiet = TRUE)

  rendered <- read_lines(tmpout)
  expect_true(
    which(grepl("script-head-1", rendered)) <
      which(grepl("script-head-2", rendered))
  )

  expect_true(
    which(grepl("script-before-1", rendered)) <
      which(grepl("script-before-2", rendered))
  )

  expect_true(
    which(grepl("script-after-1", rendered)) <
      which(grepl("script-after-2", rendered))
  )
})
