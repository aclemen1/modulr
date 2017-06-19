context("gears")

test_that(".function_to_string returns a string", {
  expect_is(.function_to_string(function() NULL), "character")
  expect_equal(length(.function_to_string(function() NULL)), 1)
})

test_that(".function_to_string returns a correct string", {
  expect_equal(.function_to_string(function() NULL), "NULL")
  expect_equal(.function_to_string(function() "Hello World"),
               "\"Hello World\"")
  expect_equal(.function_to_string(
# Keep indentation like this!
# Begin Exclude Linting
function() {
  # This is a comment
  "Hello World"
}
# End Exclude Linting
  ),
  "{\n  # This is a comment\n  \"Hello World\"\n}")
})

test_that(".function_to_string trims last line for closures", {
  f <- NULL
  (function() {
    foo <- "bar"
    f <<- function() foo
  })()
  expect_true(grepl("environment", paste(capture.output(f), collapse = "\n")))
  expect_equal(.function_to_string(f), "foo")
})

test_that(".module_to_string returns a string", {
  reset()
  define("module", NULL, function() NULL)
  expect_is(.module_to_string("module"), "character")
  expect_equal(length(.module_to_string("module")), 1)
})

test_that(".import_to_string shows name, digest and url", {
  reset()
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("foo", NULL, function() NULL)\n',
    import_module("foo", "fake_url"),
    tmp <- .import_to_string("foo"),
    expect_match(tmp, "^\"foo\""),
    expect_match(tmp, sprintf("%%digests%%.*\"%s\"", get_digest("foo"))),
    expect_match(tmp, "%imports%.*\"fake_url\"")
  )
})

test_that(".module_to_string shows name", {
  reset()
  define("module_foobar", list(other = "other_module"), function(other) NULL)
  expect_match(.module_to_string("module_foobar"), "module_foobar")
  expect_match(.module_to_string("module_foobar"), "%provides%")
})

test_that(".module_to_string shows options", {
  reset()
  define("module_foobar", NULL, options_provider(foo = 1, bar = "bar"))
  expect_match(.module_to_string("module_foobar"), "module_foobar")
  expect_match(.module_to_string("module_foobar"), "%provides_options%")
  expect_match(.module_to_string("module_foobar"), "foo = 1")
  expect_match(.module_to_string("module_foobar"), "bar = \"bar\"")
})

test_that(".module_to_string shows dependencies", {
  reset()
  define("module", list(other = "other_module"), function(other) NULL)
  expect_match(.module_to_string("module"), "list\\(other = \"other_module")
  expect_match(.module_to_string("module"), "%provides%")
  expect_match(.module_to_string("module"), "%requires%")
  reset()
  define("module", list("other_module"), function(other) NULL)
  expect_match(.module_to_string("module"), "list\\(other = \"other_module")
  expect_match(.module_to_string("module"), "%provides%")
  expect_match(.module_to_string("module"), "%requires%")
  reset()
  define("module", list(o1 = "o_mod_1", o2 = "o_mod_2"), function(o1, o2) NULL)
  expect_match(.module_to_string("module"), "o1 = \"o_mod_1")
  expect_match(.module_to_string("module"), "o2 = \"o_mod_2")
  expect_match(.module_to_string("module"), "%provides%")
  expect_match(.module_to_string("module"), "%requires%")
  reset()
  define("module", list("o_mod_1", "o_mod_2"), function(o1, o2) NULL)
  expect_match(.module_to_string("module"), "\"o_mod_1")
  expect_match(.module_to_string("module"), "\"o_mod_2")
  expect_match(.module_to_string("module"), "%provides%")
  expect_match(.module_to_string("module"), "%requires%")
})

test_that(".module_to_string shows provider", {
  reset()
  define("module", list(other = "other_module"), function(other) "foobar")
  expect_match(.module_to_string("module"), "foobar")
  expect_match(.module_to_string("module"), "%provides%")
})

test_that(".module_to_string detects identical factories for mocks", {
  reset()
  define("module", NULL, function() "foobar")
  define("module/mock", NULL, get_provider("module"))
  expect_match(.module_to_string("module/mock", base = "module"),
               "get_provider\\(\"module\"")
})

test_that("prepare_gear calls are warned from within a module", {
  reset()
  define("outer_module", NULL, function() NULL)
  define("module", NULL, function() {
    prepare_gear("outer_module")
  })
  expect_warning(make("module"))
})

test_that("prepare_gear loads modules", {
  reset()
  expect_is(prepare_gear("module_1", load = TRUE), "modulr_gear")
  reset()
  expect_error(prepare_gear("module_1", load = FALSE))
})

test_that("prepare_gear shows installation section", {
  reset()
  define("module", NULL, function() NULL)
  expect_match(prepare_gear("module"), "## Installation")
})

test_that("prepare_gear shows imports section", {
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("foo", NULL, function() NULL)\n',
    import_module("foo", "fake_url"),
    define("module", list(foo = "foo"), function(foo) NULL),
    expect_match(prepare_gear("module"), "```\\{r imports\\}")
  )
})

test_that("prepare_gear shows docstring info", {
  reset()
  define("module", NULL, function() {
    #' docstring
  })
  expect_match(prepare_gear("module"), "docstring")
})

test_that("prepare_gear shows definition section", {
  reset()
  define("module", NULL, function() NULL)
  expect_match(prepare_gear("module"), "## Definition")
})

test_that("prepare_gear shows tests section", {
  reset()
  define("module/mock", NULL, function() NULL)
  expect_match(prepare_gear("module/mock"), "## Definition")

  reset()
  define("module/test", NULL, function() NULL)
  expect_match(prepare_gear("module/test"), "## Definition")

  reset()
  define("module", NULL, function() NULL)
  define("module/mock", NULL, function() NULL)
  expect_match(prepare_gear("module"), "## Testing")
  expect_match(prepare_gear("module"), "### Mocks")
  expect_match(prepare_gear("module"), "```\\{r mocks\\}")

  reset()
  define("module", NULL, function() NULL)
  define("module/test", NULL, function() NULL)
  expect_match(prepare_gear("module"), "## Testing")
  expect_match(prepare_gear("module"), "### Tests")
  expect_match(prepare_gear("module"), "```\\{r tests\\}")

  reset()
  define("module", NULL, function() NULL)
  define("module/mock", NULL, function() NULL)
  define("module/test", NULL, function() NULL)
  expect_match(prepare_gear("module"), "## Testing")
  expect_match(prepare_gear("module"), "### Mocks")
  expect_match(prepare_gear("module"), "```\\{r mocks\\}")
  expect_match(prepare_gear("module"), "### Tests")
  expect_match(prepare_gear("module"), "```\\{r tests\\}")
})

test_that("prepare_gear shows example section", {
  reset()
  define("module/example", NULL, function() NULL)
  expect_match(prepare_gear("module/example"), "## Definition")

  reset()
  define("module", NULL, function() NULL)
  define("module/example", NULL, function() NULL)
  expect_match(prepare_gear("module"), "## Examples")
  expect_match(prepare_gear("module"), "```\\{r examples\\}")
})
