context("special")

test_that("$get_module_name returns the module name", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_module_name())
  expect_equal(make("my/great/module"), "my/great/module")
})

test_that("$get_module_options returns the module options", {
  reset()
  module_option("my/great/module")$set(list(opt1 = "value1", opt2 = "value2"))
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_module_options())
  expect_equal(make("my/great/module"), list(opt1 = "value1", opt2 = "value2"))
})

test_that("$get_filename returns the module filename, if any", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_filename())
  expect_null(make("my/great/module"))

  expect_equal(make("module_4"), "lib/module_4.R")
})

test_that("$get_dirname returns the module directory, if any", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$get_dirname())
  expect_null(make("my/great/module"))

  expect_equal(make("module_5"), "lib")
})

test_that("$resolve_path gives access to .resolve_path", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$resolve_path)
  expect_equal(make("my/great/module"), .resolve_path)
})

test_that("$resolve_mapping gives access to .resolve_mapping", {
  reset()
  define("my/great/module", list(modulr = "modulr"),
         function(modulr) modulr$resolve_mapping)
  expect_equal(make("my/great/module"), .resolve_mapping)
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

suppressMessages(reset())
