test_that("knitr chunk hooks aren't duplicated", {
  skip_if_not(rmarkdown::pandoc_available("1.12.3"))
  tmphtml <- tempfile(fileext = ".html")
  rmarkdown::render("register/register.Rmd", output_file = tmphtml, quiet = TRUE)
  x <- read_lines(tmphtml)
  expect_equal(sum(grepl("test[.]R", x)), 1)
})
