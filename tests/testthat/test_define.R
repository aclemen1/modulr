context("define")

test_that(".signature detects changes", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })
  sig_1 <- .signature("some/module")

  define(
    "some/module",
    list(dep = "foo/baz"),
    function(dep) {
      return(dep)
    })
  sig_2 <- .signature("some/module")

  define(
    "some/module",
    list(dep = "foo/baz"),
    function(dep) {
      return(sprintf("%s", dep))
    })
  sig_3 <- .signature("some/module")

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })
  sig_4 <- .signature("some/module")

  testthat::expect_false(sig_1 == sig_2)
  testthat::expect_false(sig_1 == sig_3)
  testthat::expect_true(sig_1 == sig_4)
})

test_that("define does not re-define special modules", {
  status <- define("modulr", list(), function() NULL)
  expect_null(status)
})

test_that("define writes to the register", {
  timestamp <- Sys.time()

  undefine("some/module")

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$factory, function(dep) {
    return(dep)
  })
  expect_equal(module$signature, .signature("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)
})

test_that("re-define doesn't write to the register when no changes occur", {
  undefine("some/module")

  timestamp_1 <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  timestamp_2 <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$factory, function(dep) {
    return(dep)
  })
  expect_equal(module$signature, .signature("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp_1)
  expect_less_than(module$timestamp, timestamp_2)
})

test_that("get_factory returns the body of the module", {
  undefine("some/module")

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_equal(get_factory("some/module"),
               function(dep) {
                 return(dep)
               })

})

test_that("re-define writes to the register when changes occur", {
  undefine("some/module")

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  timestamp <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(sprintf("%s", dep))
    })

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$factory, function(dep) {
    return(sprintf("%s", dep))
  })
  expect_equal(module$signature, .signature("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)
})

test_that("reset purges the register", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  reset()

  register <- get("register", pos = modulr_env)

  expect_equal(names(register), "modulr")

  })

test_that("undefine removes the module definition from the register", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  register <- get("register", pos = modulr_env)

  expect_true("some/module" %in% names(register))

  status <- undefine("some/module")
  expect_true(status)

  register <- get("register", pos = modulr_env)

  expect_false("some/module" %in% names(register))

  })

test_that("undefine removes only registered modules", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_true(undefine("some/module"))
  expect_null(undefine("some/unregistered/module"))

})

test_that("undefine removes only non special modules", {
  reset()

  expect_null(undefine("modulr"))

})

test_that("touch updates the register", {
  undefine("some/module")

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  timestamp <- Sys.time()

  status <- touch("some/module")
  expect_true(status)

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_null(module$instance)
  expect_false(module$instanciated)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

  })

test_that("touch updates only registered modules", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_true(touch("some/module"))
  expect_null(touch("some/unregistered/module"))

})

test_that("touch updates only non special modules", {
  reset()

  expect_null(touch("modulr"))

})
