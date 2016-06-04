context("special")

test_that("$get_module_name returns the module name", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_module_name())
  expect_equal(make("my/great/module"), "my/great/module")
})

test_that("$get_module_options returns the module options", {
  reset()
  module_options("my/great/module")$set(list(opt1 = "value1", opt2 = "value2"))
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_module_options())
  expect_equal(make("my/great/module"), list(opt1 = "value1", opt2 = "value2"))
})

test_that("$get_filename returns the module filename, if any", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_filename())
  expect_null(make("my/great/module"))

  expect_equal(
    make("module_4"),
    list(setNames(normalizePath("lib/module_4.R"), "module_4"),
         setNames("lib/module_4.R", "module_4")))
})

test_that("$get_dirname returns the module directory, if any", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_dirname())
  expect_null(make("my/great/module"))

  expect_equal(make("module_5"), list(normalizePath("lib"), "lib"))
})

test_that("$message_info outputs the module name", {
  reset()
  define("module_foobar", list(modulr = "modulr"),
         function(modulr) function() modulr$message_info("hello world"))
  module <- make("module_foobar")
  expect_message(module(), regexp = "module_foobar")
})

test_that("$message_warn outputs the module name", {
  reset()
  define("module_foobar", list(modulr = "modulr"),
         function(modulr) function() modulr$message_warn("hello world"))
  module <- make("module_foobar")
  expect_warning(module(), regexp = "module_foobar")
})

test_that("$message_stop outputs the module name", {
  reset()
  define("module_foobar", list(modulr = "modulr"),
         function(modulr) function() modulr$message_stop("hello world"))
  module <- make("module_foobar")
  expect_error(module(), regexp = "module_foobar")
})

test_that("options_provider accepts an empty list of arguments", {
  provider <- options_provider()
  expect_true(is.function(provider))
  expect_true(is.environment(provider()))
})

test_that("options_provider accepts a named list of arguments", {
  provider <- options_provider(foo = "foo", bar = "bar")
  env <- provider()
  expect_true(is.function(provider))
  expect_true(is.environment(env))
  expect_equal(env$foo, "foo")
  expect_equal(env$bar, "bar")
  expect_equal(ls(env, sorted = TRUE, all.names = TRUE), sort(c("foo", "bar")))
})

test_that("options_provider'provider doesn't accept non-named arguments", {
  expect_error(options_provider("foo")())
  expect_error(options_provider("foo", "bar")())
  expect_error(options_provider(foo = "foo", "bar")())
})

test_that("`%provides_options%` calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    "foo" %provides_options% list(bar = "bar")
  })
  expect_warning(make("module"))
})

test_that("`%provides_options%` is a syntactic sugar", {
  reset()
  "foo" %provides_options% NULL
  foo <- make("foo")
  expect_true(is.environment(foo))
  expect_equal(ls(foo, sorted = TRUE, all.names = TRUE), character(0))

  reset()
  "foo" %provides_options% list()
  foo <- make("foo")
  expect_true(is.environment(foo))
  expect_equal(ls(foo, sorted = TRUE, all.names = TRUE), character(0))

  reset()
  "foo" %provides_options% c()
  foo <- make("foo")
  expect_true(is.environment(foo))
  expect_equal(ls(foo, sorted = TRUE, all.names = TRUE), character(0))

  reset()
  "foo" %provides_options% list(a = 1, b = "beta")
  foo <- make("foo")
  expect_true(is.environment(foo))
  expect_equal(foo$a, 1)
  expect_equal(foo$b, "beta")
  expect_equal(ls(foo, sorted = TRUE, all.names = TRUE), sort(c("a", "b")))

  reset()
  "foo" %provides_options% c(a = 1, b = 2)
  foo <- make("foo")
  expect_true(is.environment(foo))
  expect_equal(foo$a, 1)
  expect_equal(foo$b, 2)
  expect_equal(ls(foo, sorted = TRUE, all.names = TRUE), sort(c("a", "b")))

  reset()
  "foo" %provides_options% list(a = 1, "beta")
  expect_error(make("foo"))

  reset()
  "foo" %provides_options% c(a = 1, 2)
  expect_error(make("foo"))
})

test_that("`%provides_options%` acts as a module option", {
  reset()
  "foo/config" %provides_options% list(opt_1 = "foo", opt_2 = "bar")
  "foo" %requires% list(config = "foo/config") %provides% {
    function() {
      paste0(config$opt_1, config$opt_2)
    }
  }
  foo <- make("foo")
  expect_equal(foo(), "foobar")
  config <- make("foo/config")
  config$opt_2 <- "BAR"
  expect_equal(foo(), "fooBAR")
  "foo/config" %provides_options% list(opt_1 = "foo", opt_2 = "bar")
  foo <- make("foo")
  expect_equal(foo(), "fooBAR")
  touch("foo/config")
  "foo/config" %provides_options% list(opt_1 = "foo", opt_2 = "bar")
  foo <- make("foo")
  expect_equal(foo(), "foobar")
})

test_that("options see dependencies, if any", {
  reset()
  "foo" %provides% "FOO"
  "foobar" %requires% list(foo = "foo") %provides% options_provider(
    foobar = paste0(foo, "BAR"))
  foobar <- make("foobar")
  expect_equal(foobar$foobar, "FOOBAR")

  reset()
  "foo" %provides% "FOO"
  "foobar" %requires% list(foo = "foo") %provides_options% list(
    foobar = paste0(foo, "BAR"))
  foobar <- make("foobar")
  expect_equal(foobar$foobar, "FOOBAR")
})

suppressMessages(reset())
