context("import")

test_that("import_module doesn't import defined modules, unless forced", {
  reset()
  define("module1", NULL, function() {})
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
      {'define("module1", NULL, function() {})\nTRUE'},
    expect_null(import_module("module1", "fake_url"))
  )
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
      {'define("module1", NULL, function() {})\nTRUE'},
    expect_true(import_module("module1", "fake_url", force = T))
  )
})

test_that("import_module loads local modules if they exist", {
  reset()
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module_1", NULL, function() {})\n"remote"'},
    expect_null(import_module("module_1", "fake_url"))
  )
})

test_that("import_module fails on non-existing modules", {
  reset()
  with_mock(
    `httr::GET` = function(...) stop(),
    `httr::content` = function(...) NULL,
    expect_error(import_module("module1", "fake_url"))
  )
})

test_that("import_module fails on existing modules with different name", {
  reset()
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
      {'define("module_not_1", NULL, function() {})\nreturn(TRUE)'},
    expect_error(import_module("module1", "fake_url")),
    expect_true(import_module("module_not_1", "fake_url"))
  )

  reset()
  pre_list <- list_modules(wide = F)
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module_not_1", NULL, function() {})\nreturn(TRUE)'},
    expect_error(import_module("module1", "fake_url"))
  )
  expect_equal(list_modules(wide = F), pre_list)

})

test_that("import_module fails on existing modules with different signature", {
  reset()
  define("module1_local", NULL, function() {})
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\nreturn(TRUE)'},
    expect_error(import_module("module1", "fake_url",
                               signature = "unprobable")),
    expect_true(import_module("module1", "fake_url",
                              signature = get_signature("module1_local")))
  )

  reset()
  pre_list <- list_modules(wide = F)
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\nreturn(TRUE)'},
    expect_error(import_module("module1", "fake_url",
                               signature = "unprobable"))
  )
  expect_equal(list_modules(wide = F), pre_list)

})

test_that("import_module fails on modules with errors", {
  reset()
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\nstop()'},
    expect_error(import_module("module1", "fake_url"))
  )

  reset()
  pre_list <- list_modules(wide = F)
  with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\nstop()'},
    expect_error(import_module("module1", "fake_url"))
  )
  expect_equal(list_modules(wide = F), pre_list)

})

test_that("%imports% is a syntactic sugar for `import_module`", {
  reset()
  m1 <- with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\n"foo"'},
    import_module("module1", "fake_url")
  )
  reset()
  m2 <- with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\n"foo"'},
    "module1" %imports% "fake_url"
  )
  expect_equal(m1, m2)

})

test_that("%signed% %imports% are syntactic sugars for `import_module`", {
  reset()
  define("module", NULL, function() {})
  m1 <- with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\n"foo"'},
    import_module("module1", "fake_url", signature = get_signature("module"))
  )
  reset()
  define("module", NULL, function() {})
  m2 <- with_mock(
    `httr::GET` = function(...) {},
    `httr::content` = function(...)
    {'define("module1", NULL, function() {})\n"foo"'},
    "module1" %signed% get_signature("module") %imports% "fake_url"
  )
  expect_equal(m1, m2)

})
