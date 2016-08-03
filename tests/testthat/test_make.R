context("make")

test_that(".align_named_vectors aligns named vectors", {
  expect_equal(
    .align_named_vectors(
      c(a = "A"), c(A = "alpha")
    ),
    c(a = "alpha")
  )
  expect_equal(
    .align_named_vectors(
      c(a = "A", b = "B"), c(A = "alpha")
    ),
    c(a = "alpha")
  )
  expect_equal(
    .align_named_vectors(
      c(b = "B"), c(A = "alpha")
    ),
    setNames(character(0), character(0))
  )
  expect_equal(
    .align_named_vectors(
      c(a = "A"), c(A = "alpha", A = "alpha")
    ),
    c(a = "alpha")
  )
  expect_equal(
    .align_named_vectors(
      c(a = "A"), c(A = "alpha", A = "beta")
    ),
    c(a = "alpha")
  )
  expect_equal(
    .align_named_vectors(
      c(a = "A", b = "B"), c(A = "alpha", B = "beta")
    ),
    c(a = "alpha", b = "beta")
  )
})

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

test_that("make writes to the registry", {
  reset()

  define(
    "some/module",
    list(),
    function() {
      return("foo")
    })

  timestamp <- Sys.time()

  some_module <- make("some/module")

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list())
  expect_equal(module$provider, (function() {
    return("foo")
  }))
  expect_equal(module$digest, get_digest("some/module"))
  expect_equal(module$instance$value, "foo")
  expect_true(module$instanciated)
  expect_false(module$first_instance)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))
})

test_that("make handles invisibility correctly", {
  reset()

  define("foo", NULL, function() invisible("phantom"))
  define("bar", NULL, function() "incarned")
  define("tricky", NULL, function() invisible(function(a) a + 1))
  define("tricky2", NULL, function() function(a) invisible(a + 1))

  expect_false(withVisible(make("foo"))$visible)
  expect_equal(make("foo"), "phantom")
  expect_true(withVisible(make("bar"))$visible)
  expect_equal(make("bar"), "incarned")
  expect_false(withVisible(make("tricky"))$visible)
  expect_true(withVisible(make("tricky")(1))$visible)
  expect_true(withVisible(make("tricky2"))$visible)
  expect_false(withVisible(make("tricky2")(1))$visible)
})

test_that("make instanciates dependencies", {
  reset()

  make("module_2")
  registry <- get("registry", pos = .modulr_env$injector)

  expect_true("module_2" %in% names(registry))
  expect_true("module_1" %in% names(registry))
})

test_that("make reinstanciates touched dependency, only once", {
  reset()

  make("module_2")

  timestamp <- Sys.time()
  touch("module_1")

  module_timestamp <-
    get("registry", pos = .modulr_env$injector)[["module_2"]]$timestamp
  expect_lt(as.numeric(module_timestamp), as.numeric(timestamp))

  make("module_2")

  module_timestamp <-
    get("registry", pos = .modulr_env$injector)[["module_2"]]$timestamp
  expect_gt(as.numeric(module_timestamp), as.numeric(timestamp))

  make("module_2")

  module_timestamp_2 <-
    get("registry", pos = .modulr_env$injector)[["module_2"]]$timestamp
  expect_equal(as.numeric(module_timestamp_2), as.numeric(module_timestamp))
})

test_that("make reinstanciates touched dependencies for each child module", {
  reset()

  # m_2 <- m_1, m_2bis <- m_1
  make("module_2")
  make("module_2bis")

  timestamp <- Sys.time()
  touch("module_1")
  make("module_2")

  module_timestamp <-
    get("registry", pos = .modulr_env$injector)[["module_2"]]$timestamp
  expect_gt(as.numeric(module_timestamp), as.numeric(timestamp))

  make("module_2bis")

  module_timestamp_2 <-
    get("registry", pos = .modulr_env$injector)[["module_2bis"]]$timestamp
  expect_gt(as.numeric(module_timestamp_2), as.numeric(timestamp))
  expect_gt(as.numeric(module_timestamp_2), as.numeric(module_timestamp))
})

test_that("make reinstanciates touched dependencies for chained modules", {
  reset()

  make("module_3")

  timestamp <- Sys.time()
  touch("module_1")
  make("module_2")

  module_timestamp <-
    get("registry", pos = .modulr_env$injector)[["module_2"]]$timestamp
  expect_gt(as.numeric(module_timestamp), as.numeric(timestamp))

  make("module_3")

  module_timestamp_2 <-
    get("registry", pos = .modulr_env$injector)[["module_3"]]$timestamp
  expect_gt(as.numeric(module_timestamp_2), as.numeric(timestamp))
  expect_gt(as.numeric(module_timestamp_2), as.numeric(module_timestamp))
})

test_that("make reinstanciates touched deps for chained modules, scenario 2", {
  reset()

  make("module_3")

  timestamp <- Sys.time()
  touch("module_1")
  make("module_3")

  module_timestamp <-
    get("registry", pos = .modulr_env$injector)[["module_3"]]$timestamp
  expect_gt(as.numeric(module_timestamp), as.numeric(timestamp))

  make("module_2")

  module_timestamp_2 <-
    get("registry", pos = .modulr_env$injector)[["module_2"]]$timestamp
  expect_lt(as.numeric(module_timestamp_2), as.numeric(module_timestamp))
})

test_that("make applies function to arguments, if any", {
  reset()
  define("foo", NULL, function() function(a) a + 1)
  expect_true(is.function(make("foo")))
  expect_equal(make("foo")(1), make("foo", 1))
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
  if (exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  (function() {
    m1_sugar %<=% "module_1"
    expect_true(exists("m1_sugar"))
  })()
  expect_false(exists("m1_sugar"))
})

test_that("%=>% assigns value in current frame only", {
  reset()
  if (exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
                                          pos = .GlobalEnv)
  (function() {
    "module_1" %=>% m1_sugar
    expect_true(exists("m1_sugar"))
  })()
  expect_false(exists("m1_sugar"))
})

test_that("%<<=% assigns value in global environment", {
  reset()
  if (exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
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
  if (exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
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
  if (exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
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
  if (exists("m1_sugar", inherits = T)) rm("m1_sugar", inherits = T,
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
  reset()
  define("module", NULL, function() {
    modulr <- do_make("modulr")
  })
  expect_warning(make("module"))
  reset()
  define("module", NULL, function() {
    modulr <- make("modulr")
  })
  expect_warning(do_make("module"))
  reset()
  define("module", NULL, function() {
    modulr <- do_make("modulr")
  })
  expect_warning(do_make("module"))
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
  expect_equal(.Last.name, setNames("module_1", "module_1")) # Exclude Linting
  define("module_1/test/a/dependency", NULL, function() NULL)
  make("module_1/test/a/dependency")
  expect_equal(.Last.name,
               setNames("module_1/test/a/dependency",
                       "module_1/test/a/dependency")) # Exclude Linting
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

  "test_1/sub_2/test" %requires% list(test_1 = "test_1/mock") %provides%
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

  "test_1/sub_2/test" %requires% list(test_1 = "test_1/mock") %provides%
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

  "test_1/sub_2/test" %requires% list(test_1 = "test_1/mock") %provides%
    function(test_1) {
      return(T)
    }

  expect_error(make_tests())
})

test_that("make outputs a correct layer message", {
  reset()
  root_config$set(".")
  define("foo", NULL, NULL)
  define("bar", NULL, NULL)
  define("foobar", list(foo = "foo", bar = "bar"), NULL)
  expect_output(make("foobar"), "1 layer,")
  define("super", list(foobar = "foobar"), NULL)
  expect_output(make("super"), "2 layers,")
})

test_that("make keeps the provider environment", {
  reset()
  foo <- "foo"
  provider <- function() foo
  define("foo", NULL, provider)
  expect_true(isTRUE(make("foo") == "foo"))
  rm(foo)

  reset()
  provider <- function() {
    foo <- "foo"
    function() foo
  }
  define("foo", NULL, provider)
  expect_equal(make("foo")(), "foo")
})

test_that("make doesn't recurse infinitely when sourced", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  test_env <- new.env()
  assign("test_env", test_env, globalenv())
  on.exit(rm(list = c("test_env", "env"), pos = globalenv()))
  test_env$deep <- 1
  module_text <-
    sprintf(
      paste(
        "define('%s', NULL, function() NULL)",
        "env <- get('test_env', envir = globalenv())",
        "if (env$deep > 3) stop()",
        "env$deep <- env$deep + 1",
        "make()", sep = "\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)

  root_config$set(path)

  source(file)

  expect_lte(test_env$deep, 3)

  expect_true(.is_defined(name))
})

test_that("make doesn't recurse infinitely when called", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  test_env <- new.env()
  assign("test_env", test_env, globalenv())
  on.exit(rm(list = c("test_env", "env"), pos = globalenv()))
  test_env$deep <- 1
  module_text <-
    sprintf(
      paste(
        "define('%s', NULL, function() NULL)",
        "env <- get('test_env', envir = globalenv())",
        "if (env$deep > 2) stop()",
        "env$deep <- env$deep + 1",
        "make()", sep = "\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)

  root_config$set(path)

  module_file <- make(name)

  expect_lte(test_env$deep, 2)

  expect_true(.is_defined(name))
})

test_that("do_make doesn't recurse infinitely when called", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  test_env <- new.env()
  assign("test_env", test_env, globalenv())
  on.exit(rm(list = c("test_env", "env"), pos = globalenv()))
  test_env$deep <- 1
  module_text <-
    sprintf(
      paste(
        "define('%s', NULL, function() NULL)",
        "env <- get('test_env', envir = globalenv())",
        "if (env$deep > 2) stop()",
        "env$deep <- env$deep + 1",
        "do_make()", sep = "\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)

  root_config$set(path)

  module_file <- do_make(name)

  expect_lte(test_env$deep, 2)

  expect_true(.is_defined(name))
})
