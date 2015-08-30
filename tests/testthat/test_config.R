context("config")

test_that(".config requires a scope", {
  expect_null(.config())
  })

test_that(".config returns a bunch of accessors", {
  reset()
  cfg <- .config("scope")
  expect_true(
    length(setdiff(names(cfg), c("unset", "set", "get", "get_all"))) == 0)
  expect_true(all(unlist(lapply(cfg, is.function))))
  })

test_that(".config alterates the configuration variable in the modulr env", {
  reset()
  configuration <- get("configuration", pos = modulr_env)
  expect_null(configuration[["scope"]])
  cfg <- .config("scope")
  cfg$set("value")
  configuration <- get("configuration", pos = modulr_env)
  expect_equal(configuration[["scope"]], list("value"))
})

test_that(".config$set is a setter", {
  reset()
  cfg <- .config("scope")
  cfg$set(list("c1" = "v1", "c2" = "v2"))
  configuration <- get("configuration", pos = modulr_env)
  expect_equal(configuration[["scope"]], list("c1" = "v1", "c2" = "v2"))
})

test_that(".config$set can drop and undrop values", {
  reset()
  cfg <- .config("scope")
  cfg$set(list("c1" = "v1", "c2" = "v2"))
  configuration <- get("configuration", pos = modulr_env)
  expect_equal(configuration[["scope"]], list("c1" = "v1", "c2" = "v2"))
  cfg$set(list("c1" = "v1bis"), drop = T)
  configuration <- get("configuration", pos = modulr_env)
  expect_equal(configuration[["scope"]], list("c1" = "v1bis", "c2" = "v2"))
  cfg$set(list("c1" = "v1ter"), drop = F)
  configuration <- get("configuration", pos = modulr_env)
  expect_equal(configuration[["scope"]], list("c1" = "v1bis", "c2" = "v2"))
})

test_that(".config$get is a getter", {
  reset()
  cfg <- .config("scope")
  cfg$set(list("c1" = "v1", "c2" = "v2"))
  expect_equal(cfg$get("c1"), "v1")
  expect_equal(cfg$get("c2"), "v2")
  expect_null(cfg$get("c3"))
})

test_that(".config$get_all is a getter", {
  reset()
  cfg <- .config("scope")
  cfg$set(list("c1" = "v1", "c2" = "v2"))
  expect_equal(cfg$get_all(), list("c1" = "v1", "c2" = "v2"))
})

test_that(".config$unset is a destructor", {
  reset()
  cfg <- .config("scope")
  cfg$set("value")
  configuration <- get("configuration", pos = modulr_env)
  expect_equal(configuration[["scope"]], list("value"))
  cfg$unset()
  configuration <- get("configuration", pos = modulr_env)
  expect_null(configuration[["scope"]])
})

test_that("get_configs returns the whole configuration state", {
  reset()
  cfg <- .config("scope")
  cfg$set("value")
  cfgs <- get_configs()
  expect_equal(cfgs, get("configuration", pos = modulr_env))
  expect_equal(cfgs$scope, list("value"))
})

test_that("%has_default_option% sets default module option", {
  reset()
  `%has_default_option%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
  })

test_that("%has_default_option% sets only unset module options", {
  reset()
  `%has_default_option%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
  `%has_default_option%`("module", list(c1 = "v1", c2 = "v2bis", c3 = "v3"))
  expect_equal(module_option("module")$get_all(),
               list(c1 = "v1", c2 = "v2", c3 = "v3"))
})

test_that("%has_option% sets module option", {
  reset()
  `%has_option%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
})

test_that("%has_option% sets already set module options", {
  reset()
  `%has_option%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
  `%has_option%`("module", list(c1 = "v1", c2 = "v2bis", c3 = "v3"))
  expect_equal(module_option("module")$get_all(),
               list(c1 = "v1", c2 = "v2bis", c3 = "v3"))
})

test_that("%has_default_options% sets default module option", {
  reset()
  `%has_default_options%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
})

test_that("%has_default_options% sets only unset module options", {
  reset()
  `%has_default_options%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
  `%has_default_options%`("module", list(c1 = "v1", c2 = "v2bis", c3 = "v3"))
  expect_equal(module_option("module")$get_all(),
               list(c1 = "v1", c2 = "v2", c3 = "v3"))
})

test_that("%has_options% sets module option", {
  reset()
  `%has_options%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
})

test_that("%has_options% sets already set module options", {
  reset()
  `%has_options%`("module", list(c1 = "v1", c2 = "v2"))
  expect_equal(module_option("module")$get_all(), list(c1 = "v1", c2 = "v2"))
  `%has_options%`("module", list(c1 = "v1", c2 = "v2bis", c3 = "v3"))
  expect_equal(module_option("module")$get_all(),
               list(c1 = "v1", c2 = "v2bis", c3 = "v3"))
})