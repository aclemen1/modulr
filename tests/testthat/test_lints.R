if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    # Since testthat-0.11.0.9000, lintr::expect_lint_free is not working
    # TODO adapt when lintr is updated
    if (packageVersion("testthat") >= package_version("0.11.0.9000")) {
      tryCatch({
        lintr::expect_lint_free()
      },
      error = function(e) {
        skip(paste("Skipping linting with testthat >= 0.11.0.9000.",
                   "Should be fixed in a future release of lintr."))
      })
    } else {
      lintr::expect_lint_free()
    }
  })
}
