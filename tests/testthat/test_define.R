context("define")

test_that(".hash computes a SHA-1 digest", {
  expect_equal(.hash(NULL), "8d9c05ec7ae28b219c4c56edbce6a721bd68af82")
  expect_equal(.hash("modulr"), "f478fe41ce85ad889b473813494b08c88989da19")
})

test_that("get_digest detects changes", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })
  sig_1 <- get_digest("some/module")

  define(
    "some/module",
    list(dep = "foo/baz"),
    function(dep) {
      return(dep)
    })
  sig_2 <- get_digest("some/module")

  define(
    "some/module",
    list(dep = "foo/baz"),
    function(dep) {
      return(sprintf("%s", dep))
    })
  sig_3 <- get_digest("some/module")

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })
  sig_4 <- get_digest("some/module")

  testthat::expect_false(sig_1 == sig_2)
  testthat::expect_false(sig_1 == sig_3)
  testthat::expect_true(sig_1 == sig_4)
})

test_that("get_digest calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    get_digest("module_1", load = T)
  })
  expect_warning(make("module"))
})

test_that("define does not re-define reserved modules", {
  reset()
  expect_error(define("modulr", list(), function() NULL))
})

test_that("define writes to the register", {
  reset()
  timestamp <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$factory, function(dep) {
    return(dep)
  })
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)
})

test_that("re-define doesn't write to the register when no changes occur", {
  reset()

  timestamp_1 <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  timestamp_2 <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$factory, function(dep) {
    return(dep)
  })
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp_1)
  expect_less_than(module$timestamp, timestamp_2)
})

test_that("re-define writes to the register when changes occur", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  timestamp <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(sprintf("%s", dep))
    })

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$name, "some/module")
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$factory, function(dep) {
    return(sprintf("%s", dep))
  })
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)
})

test_that("define calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    define("other/module", NULL, function() NULL)
  })
  expect_warning(make("module"))
  define("module", NULL, function() {
    "other/module" %provides% function() NULL
  })
  expect_warning(make("module"))
})

test_that("get_factory returns the body of the module", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_equal(get_factory("some/module"),
               function(dep) {
                 return(dep)
               })

  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_equal(get_factory("some/module"),
               function(dep) {
                 return(dep)
               })

  })

test_that("get_factory is able to find an undefined module", {
  reset()

  expect_error(get_factory("unexisting/module", load = F))

  expect_error(get_factory("unexisting/module", load = T))

  expect_error(get_factory("module_1", load = F))

  expect_equal(get_factory("module_1", load = T),
               function() "module_1")

})

test_that("get_factory calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    get_factory("module_1", load = T)
  })
  expect_warning(make("module"))
})

test_that("reset purges the register", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  reset()

  register <- get("register", pos = modulr_env)

  expect_equal(names(register), "modulr")

  })

test_that("reset(all=T) purges the stashes", {
  reset(all = T)
  expect_equal(length(modulr_env$stash), 0)
  stash()
  reset(all = F)
  expect_false(length(modulr_env$stash) == 0)
  reset(all = T)
  expect_equal(length(modulr_env$stash), 0)
})

test_that("reset calls generate an error from within a module", {
  reset()
  define("module", NULL, function() {
    reset()
  })
  expect_error(make("module"))
})

test_that("undefine removes the module definition from the register", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  register <- get("register", pos = modulr_env)

  expect_true("some/module" %in% names(register))

  status <- undefine("some/module")
  expect_null(status)

  register <- get("register", pos = modulr_env)

  expect_false("some/module" %in% names(register))

  })

test_that("undefine removes only registered modules", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_null(undefine("some/module"))
  expect_error(undefine("some/unregistered/module"))

})

test_that("undefine removes only non reserved modules", {
  reset()

  expect_error(undefine("modulr"))

})

test_that("undefine calls are warned from within a module", {
  reset()
  define("module_ex", NULL, function() NULL)
  define("module", NULL, function() {
    undefine("module_ex")
  })
  expect_warning(make("module"))
  define("module", NULL, function() {
    undefine("module")
  })
  expect_error(make("module"))
})

test_that("touch updates the register", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  timestamp <- Sys.time()

  status <- touch("some/module")
  expect_null(status)

  register <- get("register", pos = modulr_env)
  module <- register[["some/module"]]

  expect_null(module$instance)
  expect_false(module$instanciated)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

  })

test_that("touch updates only registered modules", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_null(touch("some/module"))
  expect_error(touch("some/unregistered/module"))

})

test_that("touch updates only non reserved modules", {
  reset()

  expect_error(touch("modulr"))

})

test_that("touch calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    touch("module")
  })
  expect_warning(make("module"))
})

test_that("define assigns the last regular core module name to .Last.name", {
  reset()
  expect_null(.Last.name) # Exclude Linting
  define("module_1", NULL, function() NULL)
  expect_equal(.Last.name, "module_1") # Exclude Linting
  define("module_1/test/a/dependency", NULL, function() NULL)
  expect_equal(.Last.name, "module_1") # Exclude Linting
  define("module_1/mock/a/dependency", NULL, function() NULL)
  expect_equal(.Last.name, "module_1") # Exclude Linting
})

test_that("reset resets .Last.name", {
  reset()
  expect_null(.Last.name) # Exclude Linting
  define("module_1", NULL, function() NULL)
  expect_equal(.Last.name, "module_1") # Exclude Linting
  reset()
  expect_null(.Last.name) # Exclude Linting
})

test_that("touch assigns the last regular module name to .Last.name", {
  reset()
  define("module_1", NULL, function() NULL)
  define("module_2", NULL, function() NULL)
  expect_equal(.Last.name, "module_2") # Exclude Linting
  touch("module_1")
  expect_equal(.Last.name, "module_1") # Exclude Linting
  define("module_1/test/a/dependency", NULL, function() NULL)
  expect_equal(.Last.name, "module_1") # Exclude Linting
  touch("module_1/test/a/dependency")
  expect_equal(.Last.name, "module_1/test/a/dependency") # Exclude Linting
})
