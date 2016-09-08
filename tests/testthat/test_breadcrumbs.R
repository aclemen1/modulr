context("breadcrumbs")

test_that("get_breadcrumbs returns null when called from outside a module", {
  expect_equal("__main__", get_breadcrumbs())
  expect_equal("__main__", get_breadcrumbs("foo"))
})

test_that("get_breadcrumbs returns module name for singletons", {
  reset()

  define("module1", NULL, function() get_breadcrumbs())
  expect_equal(c("__main__", "module1"), make("module1"))

  define("module2", list(m1 = "module1"), function(m1) m1)
  expect_equal(c("__main__", "module1"), make("module2"))

  define("module3", list(m2 = "module2"), function(m2) m2)
  expect_equal(c("__main__", "module1"), make("module3"))

})

test_that("get_breadcrumbs returns breadcrumbs for nested calls", {
  reset()

  define("module1", NULL, function() {
    function() get_breadcrumbs()
  })
  define("module2", list(m1 = "module1"), function(m1) m1())
  expect_equal(c("__main__", "module2", "module1"), make("module2"))

  define("module2", list(m1 = "module1"), function(m1) function() m1())
  define("module3", list(m2 = "module2"), function(m2) m2())
  expect_equal(c("__main__", "module3", "module2", "module1"), make("module3"))
})

test_that(".is_installed_bc detects breadcrumbs in error handler", {
  error_bak <- getOption("error")
  on.exit(options(error = error_bak))

  options(error = NULL)
  expect_false(.is_installed_bc())

  options(error = function() {
    modulr::get_breadcrumbs("installed")
  })
  expect_true(.is_installed_bc())
})

test_that("reactivate_breadcrumbs installs and preserves error handler", {
  error_bak <- getOption("error")
  on.exit(options(error = error_bak))

  options(error = NULL)
  expect_false(.is_installed_bc())

  reactivate_breadcrumbs()
  expect_true(.is_installed_bc())

  foo <- NULL
  options(error = function() {
    assign("foo", "bar", envir = parent.frame())
    stop()
  })
  expect_false(.is_installed_bc())

  reactivate_breadcrumbs()
  expect_true(.is_installed_bc())

  try(eval(getOption("error")), silent = T)
  expect_equal(foo, "bar")

})
