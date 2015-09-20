if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    #skip("TODO: remove this skip") # Exclude Linting
    lintr::expect_lint_free()
  })
}
