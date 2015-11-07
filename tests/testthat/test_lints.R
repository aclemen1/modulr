if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    #skip("TODO: remove this skip") # Exclude Linting
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    lintr::expect_lint_free()
  })
}
