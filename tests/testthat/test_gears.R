context("gears")

test_that(".function_to_string returns a string", {
  expect_is(.function_to_string(function() NULL), "character")
  expect_equal(length(.function_to_string(function() NULL)), 1)
})

test_that(".function_to_string returns a correct string", {
  expect_equal(.function_to_string(function() NULL), "function() NULL")
  expect_equal(.function_to_string(function() "Hello World"),
               "function() \"Hello World\"")
  expect_equal(.function_to_string(
# Keep indentation like this!
# Begin Exclude Linting
function() {
  # This is a comment
  "Hello World"
}
# End Exclude Linting
  ),
  "function() {\n  # This is a comment\n  \"Hello World\"\n}")
})

test_that(".function_to_string trims last line for closures", {
  f <- NULL
  (function() {
    foo <- "bar"
    f <<- function() foo
  })()
  expect_true(grepl("environment", paste(capture.output(f), collapse = "\n")))
  expect_equal(.function_to_string(f), "function() foo")
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

test_that(".module_to_string shows dependencies", {
  reset()
  define("module", list(other = "other_module"), function(other) NULL)
  expect_match(.module_to_string("module"), "list\\(other = \"other_module")
  expect_match(.module_to_string("module"), "%provides%")
  expect_match(.module_to_string("module"), "%requires%")
  reset()
  define("module", list(o1 = "o_mod_1", o2 = "o_mod_2"), function(o1, o2) NULL)
  expect_match(.module_to_string("module"), "o1 = \"o_mod_1")
  expect_match(.module_to_string("module"), "o2 = \"o_mod_2")
  expect_match(.module_to_string("module"), "%provides%")
  expect_match(.module_to_string("module"), "%requires%")
})

test_that(".module_to_string shows factory", {
  reset()
  define("module", list(other = "other_module"), function(other) "foobar")
  expect_match(.module_to_string("module"), "function\\(other")
  expect_match(.module_to_string("module"), "foobar")
  expect_match(.module_to_string("module"), "%provides%")
})

test_that(".module_to_string detects identical factories for mocks", {
  reset()
  define("module", NULL, function() "foobar")
  define("module/mock", NULL, get_factory("module"))
  expect_match(.module_to_string("module/mock", base = "module"),
               "get_factory\\(\"module\"")
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
  expect_is(prepare_gear("module_1", load = TRUE), "character")
  reset()
  expect_error(prepare_gear("module_1", load = FALSE))
})

test_that("prepare_gear shows installation section", {
  reset()
  define("module", NULL, function(other) NULL)
  expect_match(prepare_gear("module"), "## Installation")
})

test_that("prepare_gear shows imports section", {
  with_mock(
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("foo", NULL, function() NULL)\n',
    import_module("foo", "fake_url"),
    define("module", list(foo = "foo"), function(foo) NULL),
    expect_match(prepare_gear("module"), "```\\{r imports\\}")
  )
})

test_that("prepare_gear shows definition section", {
  reset()
  define("module", NULL, function(other) NULL)
  expect_match(prepare_gear("module"), "## Definition")
})

test_that("prepare_gear shows tests section", {
  reset()
  define("module/mock", NULL, function(other) NULL)
  expect_match(prepare_gear("module/mock"), "## Definition")

  reset()
  define("module/test", NULL, function(other) NULL)
  expect_match(prepare_gear("module/test"), "## Definition")

  reset()
  define("module", NULL, function(other) NULL)
  define("module/mock", NULL, function(other) NULL)
  expect_match(prepare_gear("module"), "## Testing")
  expect_match(prepare_gear("module"), "### Mocks")
  expect_match(prepare_gear("module"), "```\\{r mocks\\}")

  reset()
  define("module", NULL, function(other) NULL)
  define("module/test", NULL, function(other) NULL)
  expect_match(prepare_gear("module"), "## Testing")
  expect_match(prepare_gear("module"), "### Tests")
  expect_match(prepare_gear("module"), "```\\{r tests\\}")

  reset()
  define("module", NULL, function(other) NULL)
  define("module/mock", NULL, function(other) NULL)
  define("module/test", NULL, function(other) NULL)
  expect_match(prepare_gear("module"), "## Testing")
  expect_match(prepare_gear("module"), "### Mocks")
  expect_match(prepare_gear("module"), "```\\{r mocks\\}")
  expect_match(prepare_gear("module"), "### Tests")
  expect_match(prepare_gear("module"), "```\\{r tests\\}")
})

test_that("prepare_gear shows example section", {
  reset()
  define("module/example", NULL, function(other) NULL)
  expect_match(prepare_gear("module/example"), "## Definition")

  reset()
  define("module", NULL, function(other) NULL)
  define("module/example", NULL, function(other) NULL)
  expect_match(prepare_gear("module"), "## Examples")
  expect_match(prepare_gear("module"), "```\\{r examples\\}")
})

test_that("gist_gear calls are warned from within a module", {
  reset()
  with_mock(
    `base::dir.create` = function(...) NULL,
    `base::unlink` = function(...) NULL,
    `base::cat` = function(...) NULL,
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = 5000)),
    `gistr::gist_create` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::update_files` = function(...) NULL,
    `gistr::update` = function(...) NULL,
    `gistr::browse` = function(...) NULL,
    define("outer_module", NULL, function() NULL),
    define("module", NULL, function() {
      gist_gear("outer_module")
    }),
    expect_warning(make("module"))
  )
})

test_that("gist_gear loads modules", {
  reset()
  with_mock(
    `base::dir.create` = function(...) NULL,
    `base::unlink` = function(...) NULL,
    `base::cat` = function(...) NULL,
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = 5000)),
    `gistr::gist_create` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::update_files` = function(...) NULL,
    `gistr::update` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::browse` = function(...) NULL,
    expect_error(gist_gear("module_1", load = FALSE)),
    expect_is(gist_gear("module_1", load = TRUE), "list")
  )
})

test_that("gist_gear tests rate limits", {
  reset()
  with_mock(
    `base::dir.create` = function(...) NULL,
    `base::unlink` = function(...) NULL,
    `base::cat` = function(...) NULL,
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = 5000)),
    `gistr::gist_create` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::update_files` = function(...) NULL,
    `gistr::update` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::browse` = function(...) NULL,
    define("module", NULL, function() NULL),
    expect_is(gist_gear("module"), "list")
  )
  reset()
  with_mock(
    `base::dir.create` = function(...) NULL,
    `base::unlink` = function(...) NULL,
    `base::cat` = function(...) NULL,
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = 1)),
    `gistr::gist_create` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::update_files` = function(...) NULL,
    `gistr::update` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::browse` = function(...) NULL,
    define("module", NULL, function() NULL),
    expect_error(gist_gear("module"))
  )
  reset()
  remaining <- function() 2
  with_mock(
    `base::dir.create` = function(...) NULL,
    `base::unlink` = function(...) NULL,
    `base::cat` = function(...) NULL,
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = remaining())),
    `gistr::gist_create` = function(...) {
      remaining <<- function() 1
      list(html_url = "fake_url", files = list(list(raw_url = "fake_url")))
    },
    `gistr::update_files` = function(...) NULL,
    `gistr::update` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::browse` = function(...) NULL,
    define("module", NULL, function() NULL),
    expect_is(gist_gear("module"), "list")
  )
  reset()
  remaining <- function() 2
  with_mock(
    `base::dir.create` = function(...) NULL,
    `base::unlink` = function(...) NULL,
    `base::cat` = function(...) NULL,
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = remaining())),
    `gistr::gist_create` = function(...) {
      remaining <<- function() 0
      list(html_url = "fake_url", files = list(list(raw_url = "fake_url")))
      },
    `gistr::update_files` = function(...) NULL,
    `gistr::update` = function(...)
      list(files = list(list(raw_url = "fake_url"))),
    `gistr::browse` = function(...) NULL,
    define("module", NULL, function() NULL),
    expect_error(gist_gear("module"), regexp = "dangling")
  )
})

test_that("gist_gear publishes a gist", {
  reset()
  with_mock(
    `gistr::gist_auth` = function(...) NULL,
    `gistr::rate_limit` = function(...)
      list(rate = list(reset = 60 * 1e6, remaining = 5000)),
    `gistr::gist_create` = function(...)
      list(files = list(list(raw_url = "fake_url/hash/raw/module.Rmd"))),
    `gistr::update_files` = function(g, filename) {
      con <- file(filename, "r", blocking = FALSE)
      on.exit(close(con))
      paste(
        readLines(con = con, n = -1L, ok = TRUE, warn = FALSE),
        collapse = "\n")
    },
    `gistr::update` = identity,
    `gistr::browse` = function(...) NULL,
    define("module", NULL, function() NULL),
    expect_match(gist_gear("module"), "# `module`")
  )
})
