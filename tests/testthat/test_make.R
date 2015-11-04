context("make")

test_that("mappings are resolved", {
  reset()

  maps_config$set(
    "some/old_module" = list(
      "foo/bar" = "foo/bar_V2"
    ))

  define("foo/bar", NULL, function() return("bar"))
  define("foo/bar_V2", NULL, function() return("bar_V2"))

  define("some/module",
    list(bar = "foo/bar"), function(bar) {
      return(bar)
    })

  define("some/old_module",
    list(bar = "foo/bar"), function(bar) {
      return(bar)
    })

  module <- make("some/module")
  old_module <- make("some/old_module")

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
  expect_equal(module$provider, function() {
    return("foo")
  })
  expect_equal(module$digest, get_digest("some/module"))
  expect_equal(module$instance$value, "foo")
  expect_true(module$instanciated)
  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)
})

test_that("make handles invisibility correctly", {
  reset()

  define("foo", NULL, function() invisible("phantom"))
  define("bar", NULL, function() "incarned")

  expect_false(withVisible(make("foo"))$visible)
  expect_equal(make("foo"), "phantom")
  expect_true(withVisible(make("bar"))$visible)
  expect_equal(make("bar"), "incarned")
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

  module_timestamp_2 <-
    get("register", pos = modulr_env)[["module_2"]]$timestamp
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

test_that("make calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    modulr <- make("modulr")
  })
  expect_warning(make("module"))
  reset()
  define("module", NULL, function() {
    make_all("foo")
  })
  expect_warning(make("module"))
  reset()
  define("module", NULL, function() {
    make_tests()
  })
  expect_warning(make("module"))
  reset()
  define("module", NULL, function() {
    tmp_dir <- tempfile("modulr_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE))
    make_all_tests(tmp_dir)
  })
  expect_warning(make("module"))
  reset()
  define("module", NULL, function() {
    modulr %<=% "modulr"
  })
  expect_warning(make("module"))
  reset()
  foo <- "bar"
  define("module", NULL, function() {
    foo %<<=% "modulr"
  })
  expect_warning(make("module"))
})

test_that("make returns an instance", {
  reset()
  define("module", NULL, function() function() "foo")
  expect_equal(make("module")(), "foo")
})

test_that("make assigns the last regular module name to .Last.name", {
  reset()
  expect_null(.Last.name) # Exclude Linting
  define("module_1", NULL, function() NULL)
  make("module_1")
  expect_equal(.Last.name, "module_1") # Exclude Linting
  define("module_1/test/a/dependency", NULL, function() NULL)
  make("module_1/test/a/dependency")
  expect_equal(.Last.name, "module_1/test/a/dependency") # Exclude Linting
})

test_that("make_all makes all regular defined modules and returns results", {
  reset()
  define("module1", NULL, function() "m1")
  define("module2", NULL, function() "m2")
  expect_equal(make_all(), list("module1" = "m1", "module2" = "m2"))
})

test_that("make_tests makes all tests", {
  reset()

  "test_1" %provides% function() "hello world"

  "test_1/mock" %provides% get_provider("test_1")

  "test_1/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      library(testthat)
      test_that("it is exactly equal", {
        expect_equal(test_1, "hello world")
      })
    }

  "test_1/2/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      library(testthat)
      test_that("all is ok", {
        expect_equal(tolower(test_1), "hello world")
        expect_equal(toupper(test_1), "HELLO WORLD")
      })
    }

  expect_null(make_tests())
})

test_that("make_tests fails on error", {
  reset()

  "test_1" %provides% function() "hello world"

  "test_1/mock" %provides% get_provider("test_1")

  "test_1/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      return(T)
    }

  "test_1/2/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      return(F)
    }

  expect_error(make_tests())
})

test_that("make_tests fails on malformed tests", {
  reset()

  "test_1" %provides% function() "hello world"

  "test_1/mock" %provides% get_provider("test_1")

  "test_1/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      return("malformed")
    }

  "test_1/2/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      return(T)
    }

  expect_error(make_tests())
})
