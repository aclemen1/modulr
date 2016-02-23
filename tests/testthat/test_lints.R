if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    # Since testthat-0.11.0.9000, lintr::expect_lint_free is not working
    # TODO uncomment when lintr is updated
    # lintr::expect_lint_free() # Exclude Linting
  })
}
