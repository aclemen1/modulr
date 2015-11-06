context("define")

test_that(".hash computes a SHA-1 digest", {
  expect_equal(.hash(NULL), "8d9c05ec7ae28b219c4c56edbce6a721bd68af82")
  expect_equal(.hash("modulr"), "f478fe41ce85ad889b473813494b08c88989da19")
})

test_that(".digest computes a module digest", {
  dependencies <- list(foo = "foo")

  provider <- function(foo) {
    # Hello World
    NULL
  }
  expect_equal(
    .digest(dependencies, provider),
    "536c8d1cabcc167884f75e11a6fb82f918025ea8")

  # without formals, it's safer if the digest reflects the change
  provider <- function() {
    # Hello World
    NULL
  }
  expect_equal(
    .digest(dependencies, provider),
    "a3389ff336408acc1b653190134e35578e5548c3")

  provider <- function(foo) {
    # HELLO WORLD
    NULL
  }
  expect_equal(
    .digest(dependencies, provider),
    "8a819ba8310f9bd5b00e161b44271af2dcb6c794")
})

test_that("get_digest detects changes", {
  reset()

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

test_that("get_digest detects comments", {
  reset()
  define("foo", NULL, {
    # comment
    NULL
  })
  sig_1 <- get_digest("foo")

  define("bar", NULL, {
    NULL
  })
  sig_2 <- get_digest("bar")

  expect_false(sig_1 == sig_2)

  reset()
  define("foo", NULL, function() {
    # comment
    NULL
  })
  sig_1 <- get_digest("foo")

  define("bar", NULL, function() {
    NULL
  })
  sig_2 <- get_digest("bar")

  expect_false(sig_1 == sig_2)

  reset()
  define("foobar", NULL, function() NULL)
  define("foo", list(foobar = "foobar"), function() {
    # comment
    NULL
  })
  sig_1 <- get_digest("foo")

  define("bar", list(foobar = "foobar"), function() {
    NULL
  })
  sig_2 <- get_digest("bar")

  expect_false(sig_1 == sig_2)

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
  expect_equal(module$provider, function(dep) {
    return(dep)
  })
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_null(module$url)
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
  expect_equal(module$provider, function(dep) {
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
  expect_equal(module$provider, function(dep) {
    return(sprintf("%s", dep))
  })
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_false(module$first_instance)
  expect_null(module$url)
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

test_that("define accepts provider as braced expression", {
  reset()

  define("foo", NULL, {
    "foo"
  })
  define("foobar", list(foo = "foo"), {
    paste0(foo, "bar")
  })

  expect_equal(make("foobar"), "foobar")

  reset()

  "foo" %provides% {
    "foo"
  }
  "foobar" %requires% list(foo = "foo") %provides% {
    paste0(foo, "bar")
  }

  expect_equal(make("foobar"), "foobar")
})

test_that("define accepts provider without formals", {
  reset()

  define("foo", NULL, function() "foo")
  define("foobar", list(foo = "foo"), function() paste0(foo, "bar"))

  expect_equal(make("foobar"), "foobar")
})

test_that("define requires named dependencies if provider has no formals", {
  reset()

  expect_error(define("foobar", list("foo"), function() paste0(foo, "bar")))
  expect_error(define("foobar", list(foo = "foo", bad),
                      function() paste0(foo, "bar")))

})

test_that("define doesn't require named dependencies if provider has formals", {
  reset()

  define("foo", NULL, function() "foo")
  define("foobar", list("foo"), function(foo) paste0(foo, "bar"))

  expect_equal(make("foobar"), "foobar")

})

test_that("define requires dependencies and provider to coincide", {
  reset()

  expect_silent(suppressMessages(define("foobar", list(foo = "foo"),
                       function(foo) paste0(foo, "bar"))))

  expect_error(define("foobar", list(foo = "foo"),
                      function(bad) paste0(foo, "bar")))

  expect_error(define("foobar", list(bad = "foo"),
                      function(foo) paste0(foo, "bar")))

  expect_error(define("foobar", list(foo = "foo", bad = "bad"),
                      function(foo) paste0(foo, "bar")))

  expect_error(define("foobar", list(foo = "foo"),
                      function(foo, bad) paste0(foo, "bar")))

})

test_that("get_provider returns the body of the module", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_equal(get_provider("some/module"),
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

  expect_equal(get_provider("some/module"),
               function(dep) {
                 return(dep)
               })

})

test_that("get_provider returns comments", {
  reset()
  define(
    "foo", NULL, {
      # comment
      NULL
    })

  expect_match(
    paste(deparse(get_provider("foo"), control = "useSource"),
          collapse = "\n"),
    "# comment"
  )

  reset()
  define(
    "foo",
    NULL,
    function() {
      # comment
      NULL
    })

  expect_match(
    paste(deparse(get_provider("foo"), control = "useSource"),
          collapse = "\n"),
    "# comment"
  )

  # also with non-trivial dependencies
  reset()
  define("bar", NULL, function() NULL)
  define(
    "foo",
    list(bar = "bar"),
    function() {
      # comment
      NULL
    })

  expect_match(
    paste(deparse(get_provider("foo"), control = "useSource"),
          collapse = "\n"),
    "# comment"
  )

})

test_that("get_provider is able to find an undefined module", {
  reset()

  expect_error(get_provider("unexisting/module", load = F))

  expect_error(get_provider("unexisting/module", load = T))

  expect_error(get_provider("module_1", load = F))

  expect_equal(get_provider("module_1", load = T),
               function() "module_1")

})

test_that("get_provider calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    get_provider("module_1", load = T)
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
