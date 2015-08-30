context("make")

test_that("mappings are resolved", {
  reset()
  maps_config$set(
    "some/old_module" = list(
      "foo/bar" = "foo/bar_V2"
    ))

  "foo/bar" %provides% function() return("bar")
  "foo/bar_V2" %provides% function() return("bar_V2")

  "some/module" %requires%
    list(bar = "foo/bar") %provides% function(bar) {
      return(bar)
    }

  "some/old_module" %requires%
    list(bar = "foo/bar") %provides% function(bar) {
      return(bar)
    }

  module %<=% "some/module"
  old_module %<=% "some/old_module"

  expect_equal(module, "bar")
  expect_equal(old_module, "bar_V2")

})

test_that("make writes to the register", {
  reset()

  define(
    "some/module",
    list(),
    function() {
      return("foo")
    })

  timestamp <- Sys.time()

  some_module <- make("some/module")

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list())
  expect_equal(module$factory, function() {
    return("foo")
  })
  expect_equal(module$signature, .signature("some/module"))
  expect_equal(module$instance, "foo")
  expect_true(module$instanciated)
  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)
})

test_that("make instanciates dependencies", {
  reset()

  make("module_2")
  register <- get("register", pos = modulr_env)

  expect_true("module_2" %in% names(register))
  expect_true("module_1" %in% names(register))
})

test_that("make reinstanciates touched dependency, only once", {
  reset()

  make("module_2")

  timestamp <- Sys.time()
  touch("module_1")

  module_timestamp <- get("register", pos = modulr_env)[["module_2"]]$timestamp
  expect_less_than(module_timestamp, timestamp)

  make("module_2")

  module_timestamp <- get("register", pos = modulr_env)[["module_2"]]$timestamp
  expect_more_than(module_timestamp, timestamp)

  make("module_2")

  module_timestamp_2 <- get("register", pos = modulr_env)[["module_2"]]$timestamp
  expect_equal(module_timestamp_2, module_timestamp)
})

test_that("make reinstanciates touched dependencies for each child module", {
  reset()

  # m_2 <- m_1, m_2bis <- m_1
  make("module_2")
  make("module_2bis")

  timestamp <- Sys.time()
  touch("module_1")
  make("module_2")

  module_timestamp <- get("register", pos = modulr_env)[["module_2"]]$timestamp
  expect_more_than(module_timestamp, timestamp)

  make("module_2bis")

  module_timestamp_2 <-
    get("register", pos = modulr_env)[["module_2bis"]]$timestamp
  expect_more_than(module_timestamp_2, timestamp)
  expect_more_than(module_timestamp_2, module_timestamp)
})

test_that("make reinstanciates touched dependencies for chained modules", {
  reset()

  # m_3 <- m_2 <- m_1
  make("module_3")

  timestamp <- Sys.time()
  touch("module_1")
  make("module_2")

  module_timestamp <- get("register", pos = modulr_env)[["module_2"]]$timestamp
  expect_more_than(module_timestamp, timestamp)

  make("module_3")

  module_timestamp_2 <-
    get("register", pos = modulr_env)[["module_3"]]$timestamp
  expect_more_than(module_timestamp_2, timestamp)
  expect_more_than(module_timestamp_2, module_timestamp)
})

test_that("make reinstanciates touched deps for chained modules, scenario 2", {
  reset()

  # m_3 <- m_2 <- m_1
  make("module_3")

  timestamp <- Sys.time()
  touch("module_1")
  make("module_3")

  module_timestamp <- get("register", pos = modulr_env)[["module_3"]]$timestamp
  expect_more_than(module_timestamp, timestamp)

  make("module_2")

  module_timestamp_2 <-
    get("register", pos = modulr_env)[["module_2"]]$timestamp
  expect_less_than(module_timestamp_2, module_timestamp)
})

test_that("%<=% is a syntactic sugar for `<- make`", {
  reset()
  m1 <- make("module_1")
  m1_sugar %<=% "module_1"
  expect_equal(m1_sugar, m1)
  })

test_that("%=>% is a syntactic sugar for `make() ->`", {
  reset()
  m1 <- make("module_1")
  "module_1" %=>% m1_sugar
  expect_equal(m1_sugar, m1)
})

test_that("%<=% assigns value in current frame only", {
  reset()
  if(exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  (function() {
    m1_sugar %<=% "module_1"
    expect_true(exists("m1_sugar"))
  })()
  expect_false(exists("m1_sugar"))
})

test_that("%=>% assigns value in current frame only", {
  reset()
  if(exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  (function() {
    "module_1" %=>% m1_sugar
    expect_true(exists("m1_sugar"))
  })()
  expect_false(exists("m1_sugar"))
})

test_that("%<<=% assigns value in global environment", {
  reset()
  if(exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  (function() {
    m1_sugar %<<=% "module_1"
    expect_true(exists("m1_sugar"))
  })()
  expect_true(exists("m1_sugar"))
  expect_true(exists("m1_sugar", envir = .GlobalEnv))
  rm("m1_sugar", pos = .GlobalEnv)
})

test_that("%=>>% assigns value in global environment", {
  reset()
  if(exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  (function() {
    "module_1" %=>>% m1_sugar
    expect_true(exists("m1_sugar"))
  })()
  expect_true(exists("m1_sugar"))
  expect_true(exists("m1_sugar", envir = .GlobalEnv))
  rm("m1_sugar", pos = .GlobalEnv)
})

test_that("%<<=% assigns value in parent frame", {
  reset()
  if(exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  m1_sugar <- NULL
  (function() {
    m1_sugar %<<=% "module_1"
    expect_true(exists("m1_sugar"))
  })()
  expect_true(exists("m1_sugar"))
  expect_false(exists("m1_sugar", envir = parent.frame()))
  expect_false(exists("m1_sugar", envir = .GlobalEnv))
})

test_that("%=>>% assigns value in parent frame", {
  reset()
  if(exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  m1_sugar <- NULL
  (function() {
    "module_1" %=>>% m1_sugar
    expect_true(exists("m1_sugar"))
  })()
  expect_true(exists("m1_sugar"))
  expect_false(exists("m1_sugar", envir = parent.frame()))
  expect_false(exists("m1_sugar", envir = .GlobalEnv))
})

test_that("make calls are prohibited from within a module", {
  reset()
  define("module", NULL, function() {
    modulr <- make("modulr")
  })
  expect_error(make(define))
})

test_that("make returns an instance", {
  reset()
  define("module", NULL, function() {function() "foo"})
  expect_equal(make("module")(), "foo")
})
