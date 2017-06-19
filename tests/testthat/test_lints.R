if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    lint <- FALSE
    if (interactive()) {
      lint <- FALSE
      answer <- readline("Test linting (y/N)? ")
      if (is.character(answer) && answer %in% c("y", "Y")) lint <- TRUE
    }
    if (lint) {

      # It seems that lintr doesn't handle exclusions as expected.
      # As a temporary workaround, we mock lintr::lint_package in order
      # that the "inst/" directory is not linted (it contains a lot of
      # R files from all the packages installed to demonstrate the
      # package isolation feature).
      testthat::with_mock(
        "lintr::lint_package" =
          function (path = ".", relative_path = TRUE, ...) {
            path <- lintr:::find_package(path)
            lintr:::read_settings(path)
            on.exit(lintr:::clear_settings, add = TRUE)
            files <- dir(path = file.path(path, c("R", "tests")),
                         pattern = rex::rex(".", one_of("Rr"), end),
                         recursive = TRUE,
                         full.names = TRUE)
            files <- normalizePath(files)
            lints <- lintr:::flatten_lints(lapply(files, function(file) {
              if (interactive()) {
                message(".", appendLF = FALSE)
              }
              lintr::lint(file, ..., parse_settings = FALSE)
            }))
            if (interactive()) {
              message()
            }
            lints <- lintr:::reorder_lints(lints)
            if (relative_path == TRUE) {
              lints[] <- lapply(lints, function(x) {
                x$filename <- rex::re_substitutes(
                  x$filename, rex::rex(normalizePath(path),
                                       one_of("/", "\\")), "")
                x
              })
              attr(lints, "path") <- path
            }
            class(lints) <- "lints"
            lints
          },
        lintr::expect_lint_free()
      )

    } else testthat::skip("Skip linting.")
  })
}
