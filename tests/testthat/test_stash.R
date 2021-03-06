context("stash")

test_that("stash() stashes internals to the modulr environment", {
  reset(all = T)
  define("module", NULL, function() NULL)
  expect_equal(length(.modulr_env$injector$stash), 0)
  stash()
  expect_equal(length(.modulr_env$injector$stash), 1)
  stashed <- .modulr_env$injector$stash[[1]]
  expect_equal(stashed$registry, .modulr_env$injector$registry)
  expect_equal(stashed$.Last.name, .modulr_env$injector$.Last.name) # Exclude Linting
  expect_equal(stashed$config, .modulr_env$injector$config)
  expect_equal(stashed$verbosity, .modulr_env$injector$verbosity)
})

test_that("stash() returns an incrementing ID", {
  reset(all = T)
  id <- stash()
  expect_equal(id, 1L)
  id <- stash()
  expect_equal(id, 2L)
})

test_that("stash() accepts comment", {
  reset(all = T)
  stash(comment = "this is a comment")
  stashed <- .modulr_env$injector$stash[[1]]
  expect_equal(stashed$comment, "this is a comment")
})

test_that("unstash() unstashes the last stash on the stack", {
  reset(all = T)
  stash()
  define("module_in_stash_1", NULL, function() NULL)
  stash()
  define("module_in_stash_2", NULL, function() NULL)
  stash()
  reset(all = F)
  expect_equal(length(.modulr_env$injector$stash), 3)
  expect_false("module_in_stash_1" %in% list_modules(wide = F))
  expect_false("module_in_stash_2" %in% list_modules(wide = F))
  unstash()
  expect_equal(length(.modulr_env$injector$stash), 2)
  expect_true("module_in_stash_2" %in% list_modules(wide = F))
  expect_true("module_in_stash_1" %in% list_modules(wide = F))
  unstash()
  expect_equal(length(.modulr_env$injector$stash), 1)
  expect_true("module_in_stash_1" %in% list_modules(wide = F))
  expect_false("module_in_stash_2" %in% list_modules(wide = F))
  unstash()
  expect_equal(length(.modulr_env$injector$stash), 0)
  expect_false("module_in_stash_1" %in% list_modules(wide = F))
  expect_false("module_in_stash_2" %in% list_modules(wide = F))
})

test_that("unstash(id=) unstashes the correct stash on the stack", {
  reset(all = T)
  stash()
  define("module_in_stash_1", NULL, function() NULL)
  stash()
  define("module_in_stash_2", NULL, function() NULL)
  stash()
  reset(all = F)
  expect_equal(length(.modulr_env$injector$stash), 3)
  expect_false("module_in_stash_1" %in% list_modules(wide = F))
  expect_false("module_in_stash_2" %in% list_modules(wide = F))
  unstash(1)
  expect_equal(length(.modulr_env$injector$stash), 2)
  expect_false("module_in_stash_2" %in% list_modules(wide = F))
  expect_false("module_in_stash_1" %in% list_modules(wide = F))
  unstash(2)
  expect_equal(length(.modulr_env$injector$stash), 1)
  expect_true("module_in_stash_1" %in% list_modules(wide = F))
  expect_true("module_in_stash_2" %in% list_modules(wide = F))
  unstash(1)
  expect_equal(length(.modulr_env$injector$stash), 0)
  expect_true("module_in_stash_1" %in% list_modules(wide = F))
  expect_false("module_in_stash_2" %in% list_modules(wide = F))
})

test_that("list_stashes() returns a data frame", {
  reset(all = T)
  stash()
  define("module_in_stash_1", NULL, function() NULL)
  stash()
  define("module_in_stash_2", NULL, function() NULL)
  stash()
  ls_stash <- list_stashes()
  expect_true(is.data.frame(ls_stash))
  expect_equal(nrow(ls_stash), 2 + length(RESERVED_NAMES))
  expect_equal(names(ls_stash), c("id", "timestamp", "comment"))
})

test_that("list_stash() returns NULL if nothing has been stashed", {
  reset(all = T)
  expect_message(x <- list_stashes())
  expect_null(x)
})

test_that("remove_stash() removes stashes on the stack", {
  reset(all = T)
  stash("first")
  define("module_in_stash_1", NULL, function() NULL)
  stash("second")
  define("module_in_stash_2", NULL, function() NULL)
  stash("third")
  expect_equal(length(.modulr_env$injector$stash), 3)
  remove_stash(1)
  expect_equal(length(.modulr_env$injector$stash), 2)
  expect_equal(list_stashes()$comment, c("second", "third"))
  remove_stash(2)
  expect_equal(length(.modulr_env$injector$stash), 1)
  expect_equal(list_stashes()$comment, c("second"))
  remove_stash(1)
  expect_equal(length(.modulr_env$injector$stash), 0)
})

test_that("remove_stash(all = T) removes all stashes on the stack", {
  reset(all = T)
  stash("first")
  define("module_in_stash_1", NULL, function() NULL)
  stash("second")
  define("module_in_stash_2", NULL, function() NULL)
  stash("third")
  expect_equal(length(.modulr_env$injector$stash), 3)
  remove_stash(all = T)
  expect_equal(length(.modulr_env$injector$stash), 0)
})
