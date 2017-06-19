context("import")

test_that("import_module imports modules", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module", NULL, function() NULL)\n"remote"',
    expect_equal(import_module("module", "fake_url"), "remote")
  )
  reset()
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      '```{r}\ndefine("module", NULL, function() NULL)\n"remote"\n```',
    expect_equal(import_module("module", "fake_url"), "remote")
  )
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      '<<>>=\ndefine("module", NULL, function() NULL)\n"remote"\n@',
    expect_equal(import_module("module", "fake_url"), "remote")
  )})

test_that("import_module stores url to the registry", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module", NULL, function() NULL)\n"remote"',
    import_module("module", "fake_url"),
    expect_equal(.modulr_env$injector$registry[[c("module", "url")]],
                 "fake_url")
  )
})

test_that("import_module doesn't import defined modules, unless forced", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  define("module1", NULL, function() NULL)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\nTRUE',
    expect_null(import_module("module1", "fake_url"))
  )
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\nTRUE',
    expect_true(import_module("module1", "fake_url", force = T))
  )
})

test_that("import_module loads local modules if they exist", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module_1", NULL, function() NULL)\n"remote"',
    expect_null(import_module("module_1", "fake_url"))
  )
})

test_that("import_module fails on non-existing modules", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) stop(),
    `httr::content` = function(...) NULL,
    expect_error(import_module("module1", "fake_url"))
  )
})

test_that("import_module fails on existing modules with different name", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module_not_1", NULL, function() NULL)\nreturn(TRUE)',
    expect_error(import_module("module1", "fake_url")),
    expect_true(import_module("module_not_1", "fake_url"))
  )

  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  pre_list <- list_modules(wide = F)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module_not_1", NULL, function() NULL)\nreturn(TRUE)',
    expect_error(import_module("module1", "fake_url"))
  )
  expect_equal(list_modules(wide = F), pre_list)

})

test_that("import_module fails on existing modules with different digest", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)

  define("module1_local", NULL, function() NULL)
  with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\nreturn(TRUE)',
    expect_error(import_module("module1", "fake_url",
                               digest = "unprobable")),
    expect_true(import_module("module1", "fake_url",
                              digest = get_digest("module1_local")))
  )

  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  pre_list <- list_modules(wide = F)
  with_mock(
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\nreturn(TRUE)',
    expect_error(import_module("module1", "fake_url",
                               digest = "unprobable"))
  )
  expect_equal(list_modules(wide = F), pre_list)

})

test_that("import_module fails on modules with errors", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  with_mock(
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\nstop()',
    expect_error(import_module("module1", "fake_url"))
  )

  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  pre_list <- list_modules(wide = F)
  with_mock(
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\nstop()',
    expect_error(import_module("module1", "fake_url"))
  )
  expect_equal(list_modules(wide = F), pre_list)

})

test_that("import_module calls are warned from within a module", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  define("module", NULL, function() {
    import_module("module_1", "fake_url")
  })
  expect_warning(make("module"))
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  define("module", NULL, function() {
    "module_1" %imports% "fake_url"
  })
  expect_warning(make("module"))
})

test_that("%imports% is a syntactic sugar for `import_module`", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  m1 <- with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\n"foo"',
    import_module("module1", "fake_url")
  )
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  m2 <- with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\n"foo"',
    "module1" %imports% "fake_url"
  )
  expect_equal(m1, m2)

})

test_that("%digests% %imports% are syntactic sugars for `import_module`", {
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  define("module", NULL, function() NULL)
  m1 <- with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\n"foo"',
    import_module("module1", "fake_url", digest = get_digest("module"))
  )
  reset()
  unlink(DEFAULT_GEARS_PATH, recursive = TRUE)
  define("module", NULL, function() NULL)
  m2 <- with_mock(
    `httr::parse_url` = function(...) list(scheme = "http"),
    `httr::GET` = function(...) NULL,
    `httr::content` = function(...)
      'define("module1", NULL, function() NULL)\n"foo"',
    "module1" %digests% get_digest("module") %imports% "fake_url"
  )
  expect_equal(m1, m2)

})
