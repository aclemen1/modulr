# Test helpers for cross-version stability (R 3.6.3 -> R 4.4.x+).
#
# Behaviour of `deparse(function, control = "useSource")` and the presence of
# `srcref`/`srcfile`/`wholeSrcref` attributes on parsed function literals
# changed between R versions. The same hash/equality assertions written under
# one R version can spuriously fail under another. The helpers below normalise
# those differences so that tests express *semantic* equality of providers
# rather than byte-equality of their source representation.

# Strip srcref-related attributes and the enclosing environment from a closure
# so it can be compared by structure (formals + body) alone.
.strip_provider <- function(fun) {
  if (!is.function(fun)) return(fun)
  attr(fun, "srcref") <- NULL
  attr(fun, "wholeSrcref") <- NULL
  attr(fun, "srcfile") <- NULL
  # Body and formals may carry their own srcrefs.
  b <- body(fun)
  attr(b, "srcref") <- NULL
  attr(b, "wholeSrcref") <- NULL
  attr(b, "srcfile") <- NULL
  body(fun) <- b
  # The closure environment is irrelevant to provider equivalence.
  environment(fun) <- emptyenv()
  fun
}

# Compare two providers ignoring srcref/srcfile and closure environment.
expect_provider_equal <- function(actual, expected, ...) {
  testthat::expect_equal(.strip_provider(actual), .strip_provider(expected), ...)
}

# testthat 3.2.0 made `with_mock()` defunct (with no in-place replacement
# capable of mocking *foreign* package functions such as `httr::GET` from a
# memoised wrapper). Tests that rely on it are skipped on those versions
# rather than refactored to depend on `mockery`, which is a Suggests-only
# dependency we cannot mandate here. Once those tests are migrated to
# mockery::stub() (or modulr exposes an HTTP transport hook), this guard can
# be removed.
skip_if_with_mock_defunct <- function() {
  if (utils::packageVersion("testthat") >= "3.2.0") {
    testthat::skip("with_mock() is defunct since testthat 3.2.0")
  }
}
