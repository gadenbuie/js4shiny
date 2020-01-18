test_that("requires_pkg", {
  missing_pkg <- function() requires_pkg("flarf")
  has_pkg <- function() requires_pkg("shiny")
  missing_pkg_v <- function() requires_pkg_version("flarf", "1.2")
  outdated_pkg <- function() requires_pkg_version("shiny", "98.0")
  uptodate_pkg <- function() requires_pkg_version("shiny", "1.3.0")

  expect_silent(has_pkg())
  expect_silent(uptodate_pkg())

  expect_error(missing_pkg(), "missing_pkg.+requires.+flarf")
  expect_error(missing_pkg_v(), "missing_pkg_v.+requires.+flarf")
  expect_error(outdated_pkg(), "outdated_pkg.+requires.+shiny.+version.+98")
})

describe("is_markdown()", {
  tmp <- list(
    rmd = fs::file_temp(ext = "Rmd"),
    RMD = fs::file_temp(ext = "RMD"),
    md = fs::file_temp(ext = "md"),
    MD = fs::file_temp(ext = "MD")
  )

  purrr::walk(tmp, fs::file_create)

  it("returns TRUE for rmarkdown files", {
    expect_true(is_markdown(tmp$rmd))
    expect_true(is_markdown(tmp$rmd, rmd_only = TRUE))

    expect_true(is_markdown(tmp$RMD))
    expect_true(is_markdown(tmp$RMD, rmd_only = TRUE))
  })

  it("returns TRUE for markdown files", {
    expect_true(is_markdown(tmp$md))
    expect_false(is_markdown(tmp$md, rmd_only = TRUE))

    expect_true(is_markdown(tmp$MD))
    expect_false(is_markdown(tmp$MD, rmd_only = TRUE))
  })

  it("returns a vector", {
    tmps <- unname(unlist(tmp))
    expect_equivalent(is_markdown(tmps), rep(TRUE, 4))
    expect_equivalent(is_markdown(tmps, rmd_only = TRUE), c(TRUE, TRUE, FALSE, FALSE))
  })

  purrr::walk(tmp, fs::file_delete)
})
