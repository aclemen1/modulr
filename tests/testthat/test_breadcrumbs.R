context("breadcrumbs")

test_that(".__breadcrumbs__ returns null when called from outside a module", {
  expect_null(.__breadcrumbs__())
})

test_that(".__breadcrumbs__ returns module name for singletons", {
  reset()

  define("module1", NULL, function() {.__breadcrumbs__()})
  expect_equal("module1", make("module1"))

  define("module2", list(m1 = "module1"), function(m1) {m1})
  expect_equal("module1", make("module2"))

  define("module3", list(m2 = "module2"), function(m2) {m2})
  expect_equal("module1", make("module3"))

})

test_that(".__breadcrumbs__ returns breadcrumbs for nested calls", {
  reset()

  define("module1", NULL, function() {
    function() .__breadcrumbs__()
  })
  define("module2", list(m1 = "module1"), function(m1) {m1()})
  expect_equal(c("module2", "module1"), make("module2"))

  define("module2", list(m1 = "module1"), function(m1) {function() m1()})
  define("module3", list(m2 = "module2"), function(m2) {m2()})
  expect_equal(c("module3", "module2", "module1"), make("module3"))
})

test_that(".is_installed_bc detects breadcrumbs in error handler", {
  error_bak <- getOption("error")
  on.exit(options(error = error_bak))

  options(error = NULL)
  expect_false(.is_installed_bc())

  options(error = function() {
    .__breadcrumbs__('installed')
  })
  expect_true(.is_installed_bc())
})

test_that("activate_breadcrumbs installs and preserves error handler", {
  error_bak <- getOption("error")
  on.exit(options(error = error_bak))

  options(error = NULL)
  expect_false(.is_installed_bc())

  activate_breadcrumbs()
  expect_true(.is_installed_bc())

  foo <- NULL
  options(error = function() {assign("foo", "bar", envir = parent.frame())})
  expect_false(.is_installed_bc())

  activate_breadcrumbs()
  expect_true(.is_installed_bc())

  eval(getOption("error"))
  expect_equal(foo, "bar")
})
