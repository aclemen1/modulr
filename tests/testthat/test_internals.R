context("internals")

test_that("get_internals() returns a list of internals", {
  internals <- get_internals()
  expect_true(length(
    setdiff(names(internals),
            c("env", "register", "config", "verbosity"))) == 0)
  expect_true(is.environment(internals$env))
  expect_true(is.list(internals$register))
  expect_true(is.list(internals$config))
  expect_true(is.numeric(internals$verbosity))
})

test_that("get_internals()$env returns the modulr env", {
  expect_equal(get_internals()$env, modulr_env)
  })

test_that("get_internals()$register returns the register", {
  expect_equal(get_internals()$register, modulr_env$register)
})

test_that("get_internals()$config returns the configurations", {
  expect_equal(get_internals()$config, modulr_env$config)
})

test_that("get_internals()$verbosity returns the verbosity level", {
  expect_equal(get_internals()$verbosity, modulr_env$verbosity)
})
