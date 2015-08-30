context("internals")

test_that(".internals returns a list of internals", {
  internals <- .internals()
  expect_true(length(
    setdiff(names(internals),
            c("env", "register", "config", "verbosity"))) == 0)
  expect_true(is.environment(internals$env))
  expect_true(is.list(internals$register))
  expect_true(is.list(internals$config))
  expect_true(is.numeric(internals$verbosity))
})

test_that(".internals$env returns the modulr env", {
  expect_equal(.internals()$env, modulr_env)
  })

test_that(".internals$register returns the register", {
  expect_equal(.internals()$register, modulr_env$register)
})

test_that(".internals$config returns the configurations", {
  expect_equal(.internals()$config, modulr_env$config)
})

test_that(".internals$verbosity returns the verbosity level", {
  expect_equal(.internals()$verbosity, modulr_env$verbosity_level)
})
