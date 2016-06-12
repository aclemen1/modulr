if (requireNamespace("covr", quietly = TRUE)) {
  context("coverage")
  test_that("Package Code Coverage", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    cover <- FALSE
    if (interactive()) {
      cover <- FALSE
      answer <- readline("Test code coverage (y/N)? ")
      if (is.character(answer) && answer %in% c("y", "Y")) cover <- TRUE
    }
    if (cover) {
      cov <- covr::package_coverage()
      zeroes <- covr::zero_coverage(cov)
      has_zeroes <- nrow(zeroes) > 0L
      zeroes_output <- NULL
      if (has_zeroes) {
        zeroes_output <- paste(collapse = "\n", capture.output(print(zeroes)))
      }
      testthat::expect(!has_zeroes, paste(sep = "\n",
        "Not fully covered", zeroes_output))
    } else testthat::skip("Skip linting.")
  })
}
