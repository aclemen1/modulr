context("info")

test_that(".docstring returns the docstring of a function, if any", {
  foo <- function() {
    #' docstring
  }
  expect_match(.docstring(foo), "docstring")

  foo <- function() NULL
  expect_identical(.docstring(foo), character(0))

  foo <- function() {
    NULL
    #' this is not a docstring (preceded by NULL)
  }
  expect_identical(.docstring(foo), character(0))
})

test_that(".docstring strips empty lines and comments before docstring", {
  foo <- function() {

    #' docstring
  }
  expect_equal(.docstring(foo), "docstring\n")
  foo <- function() {
    # comment
    #' docstring
  }
  expect_equal(.docstring(foo), "docstring\n")
  foo <- function() {

    # comment

    # comment

    #' docstring
  }
  expect_equal(.docstring(foo), "docstring\n")
})

test_that(".docstring justifies on left", {
  foo <- function() {
    #' line 1
    #' line 2
  }
  expect_equal(.docstring(foo), "line 1\nline 2\n")
  foo <- function() {
    #' line 1
    #'  line 2
  }
  expect_equal(.docstring(foo), "line 1\n line 2\n")
  foo <- function() {
    #'line 1
    #'line 2
  }
  foo <- function() {
    #'  line 1
    #'  line 2
  }
  expect_equal(.docstring(foo), "line 1\nline 2\n")
  foo <- function() {
    #'    line 1
    #'     line 2
    #'   line 3
  }
  expect_equal(.docstring(foo), " line 1\n  line 2\nline 3\n")
})

test_that("info returns a module docstring, if any", {
  reset()
  define("module", NULL, function() {
    #' docstring
  })
  expect_match(capture.output(info("module")), "docstring")

  reset()
  define("module", NULL, function() NULL)
  expect_identical(capture.output(info("module")), character(0))
})

test_that("info calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    info("module_1", load = TRUE)
  })
  expect_warning(make("module"))

  reset()
  define("module", NULL, function() {
    #' docstring
    info("module", load = FALSE)
  })
  expect_match(capture.output(make("module")), "docstring")
})
