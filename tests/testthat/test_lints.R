if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    lint <- TRUE
    if (interactive()) {
      lint <- FALSE
      answer <- readline("Test linting (y/N)? ")
      if (is.character(answer) && answer %in% c("y", "Y")) lint <- TRUE
    }
    if (lint) lintr::expect_lint_free()
  })
}
