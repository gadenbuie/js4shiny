test_that("knitr chunk hooks aren't duplicated", {
  tmphtml <- tempfile(fileext = ".html")
  rmarkdown::render("register/register.Rmd", output_file = tmphtml, quiet = TRUE)
  x <- readLines(tmphtml)
  expect_equal(sum(grepl("test[.]R", x)), 1)
})
