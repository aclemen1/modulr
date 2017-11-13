context("info")

test_that(".docstring returns the docstring of a function, if any", {
  foo <- function() {
    #' docstring
  }
  expect_match(.docstring(foo), "docstring")

  foo <- function() NULL
  expect_identical(.docstring(foo), character(0))

})

test_that(".docstring strips empty lines and comments before docstring", {
  foo <- function() {

    #' docstring
  }
  expect_equal(.docstring(foo), "docstring")
  foo <- function() {
    # comment
    #' docstring
  }
  expect_equal(.docstring(foo), "docstring")
  foo <- function() {

    # comment

    # comment

    #' docstring
  }
  expect_equal(.docstring(foo), "docstring")
})

test_that(".docstring justifies on left", {
  foo <- function() {
    #' line 1
    #' line 2
  }
  expect_equal(.docstring(foo), "line 1\nline 2")
  foo <- function() {
    #' line 1
    #'  line 2
  }
  expect_equal(.docstring(foo), "line 1\n line 2")
  foo <- function() {
    #'line 1
    #'line 2
  }
  foo <- function() {
    #'  line 1
    #'  line 2
  }
  expect_equal(.docstring(foo), "line 1\nline 2")
  foo <- function() {
    #'    line 1
    #'     line 2
    #'   line 3
  }
  expect_equal(.docstring(foo), " line 1\n  line 2\nline 3")
})

test_that(".docstring outputs all blocs", {
  foo <- function() {
    #' bloc 1
    NULL
    #' bloc 2
  }
  expect_equal(.docstring(foo, sep = "\n"), "bloc 1\n\nbloc 2")
})

test_that(".docstring separates blocs correctly", {
  foo <- function() {
    #' bloc 1
    NULL
    #' bloc 2
  }
  expect_equal(.docstring(foo, sep = "\n---"), "bloc 1\n---\nbloc 2")
})

test_that(".docstring shows line numbers", {
  foo <- function() {
    #' bloc 1
    NULL
    #' bloc 2
  }
  expect_match(.docstring(foo, line_numbers = TRUE, sep = "\n"), "^\\[\\d+\\]")
})

test_that("info returns a module docstring, if any", {

  reset()
  define("module", NULL, {
  })
  expect_equal(capture.output(info("module")), character(0))

  reset()
  define("module", NULL, {
    #' docstring
  })
  expect_match(capture.output(info("module")), "docstring")

  reset()
  define("module", NULL, {
    #' docstring
    NULL
  })
  expect_match(capture.output(info("module")), "docstring")

  reset()
  define("module", NULL, function() {
    #' docstring
  })
  expect_match(capture.output(info("module")), "docstring")

  reset()
  define("module", list(foo = "foo"), {
    #' docstring
  })
  expect_match(capture.output(info("module")), "docstring")

  reset()
  define("module", list(foo = "foo"), function() {
    #' docstring
  })
  expect_match(capture.output(info("module")), "docstring")

  reset()
  define("module", list(foo = "foo"), function(foo) {
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
  set_verbosity(0L)
  define("module", NULL, function() {
    #' docstring
    capture.output(info("module", load = FALSE))
  })
  expect_match(make("module"), "docstring")
  reset()
})
