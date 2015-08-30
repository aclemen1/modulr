context("messages")

test_that(".message_meta outputs messages", {
  set_verbosity(+Inf)
  expect_message(.message_meta("hello world"), regexp = "hello world")
  })

test_that(".message_meta outputs messages according to verbosity", {
  set_verbosity(+Inf)
  expect_message(.message_meta("hello world", verbosity = 1000),
                 regexp = "hello world")
  set_verbosity(2)
  expect_message(.message_meta("hello world", verbosity = 1),
                 regexp = "hello world")
  set_verbosity(1)
  expect_message(.message_meta("hello world", verbosity = 1),
                 regexp = "hello world")
  set_verbosity(0)
  expect_null(.message_meta("hello world", verbosity = 1))
})

test_that(".message_meta evaluates expressions", {
  expect_equal(.message_meta("hello world", expr = {return("foo")}), "foo")
})

test_that(".message_meta evaluates expressions in the calling env", {
  foo <- "foo"
  expect_equal(.message_meta("hello world", expr = {return(foo)}), "foo")
  .message_meta("hello world", expr = {foo <- "bar"})
  expect_equal(foo, "bar")
})

test_that(".message_meta increments and decrements level with nested calls", {
  expect_message(.message_meta("level0"), regexp = "[^*]\\ level0" )
  expect_message(.message_meta("level0", expr = {
    expect_message(.message_meta("level1", expr = {
      expect_message(.message_meta("level2"), regexp = "[^*]\\*\\*\\ level2")
      expect_message(.message_meta("level2bis"),
                     regexp = "[^*]\\*\\*\\ level2bis")
    }), regexp = "[^*]\\*\\ level1")
    expect_message(.message_meta("level1bis"), regexp = "[^*]\\*\\ level1bis")
  }), regexp = "[^*]\\ level0")
  expect_message(.message_meta("level0bis"), regexp = "[^*]\\ level0bis" )
})

test_that(".parse_message_args parse core args and other args", {
  expect_equal(
    .parse_message_args("core1", "core2"),
    list(core = c("core1", "core2")))
  expect_equal(
    .parse_message_args(other1 = "foo", other2 = "bar"),
    list(core = NULL, other1 = "foo", other2 = "bar"))
  expect_equal(
    .parse_message_args("core1", "core2", other1 = "foo", other2 = "bar"),
    list(core = c("core1", "core2"), other1 = "foo", other2 = "bar"))
})

test_that(".message can output messages, warnings and stops", {
  expect_message(.message("hello world", fun = message), regexp = "hello world")
  expect_warning(.message("hello world", fun = warning), regexp = "hello world")
  expect_error(.message("hello world", fun = stop), regexp = "hello world")
})

test_that(".message accepts multiple message arguments and concatenates", {
  expect_message(.message("hello", " ", "world", fun = message),
                 regexp = "hello world")
})

test_that(".message follows levels", {
  expect_message(.message("level0", fun = message), regexp = "[^*]\\ level0" )
  .message_meta("level0", expr = {
    expect_message(.message("level1", fun = message),
                   regexp = "[^*]\\*\\ level1" )
    .message_meta("level1", expr = {
      expect_message(.message("level2", fun = message),
                     regexp = "[^*]\\*\\*\\ level2" )
    })
    expect_message(.message("level1bis", fun = message),
                   regexp = "[^*]\\*\\ level1bis" )
  })
  expect_message(.message("level0bis", fun = message),
                 regexp = "[^*]\\ level0bis" )
})

test_that(".message outputs module name if arg is passed on", {
  expect_message(
    .message("hello world", module_name = "module", fun = message),
    "module")
})

test_that(".message_info calls .message(..., fun = message)", {
  expect_message(.message_info("hello world"), regexp = "hello world")
})

test_that(".message_warn calls .message(..., fun = warning)", {
  expect_warning(.message_warn("hello world"), regexp = "hello world")
})

test_that(".message_stop calls .message(..., fun = stop)", {
  expect_error(.message_stop("hello world"), regexp = "hello world")
})
