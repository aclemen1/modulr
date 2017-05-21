context("define")

test_that(".hash computes a xxHash64 digest", {
  expect_equal(.hash(NULL), "c85d88fc56f4e042")
  expect_equal(.hash("modulr"), "fdd28ac880bc00b7")
})

test_that(".digest computes a module digest", {
  dependencies <- list(foo = "foo")

  provider <- function(foo) {
    # Hello World
    NULL
  }
  expect_equal(
    .digest(dependencies, provider),
    "19d11fdf666db03b")

  # without formals, it's safer if the digest reflects the change
  provider <- function() {
    # Hello World
    NULL
  }
  expect_equal(
    .digest(dependencies, provider),
    "c9f5edbdf6348b2b")

  provider <- function(foo) {
    # HELLO WORLD
    NULL
  }
  expect_equal(
    .digest(dependencies, provider),
    "d947400f86c24b52")
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
  expect_error(define(MODULR_NAME, list(), function() NULL))
})

test_that("define writes to the registry", {
  reset()
  timestamp <- Sys.time()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_equal(module$aliases, list(dep = "foo/bar"))
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$provider, (function(dep) {
    return(dep)
  }))
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_true(is.na(module$along))
  expect_null(module$url)

  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))
})

test_that("re-define doesn't write to the registry when no changes occur", {
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

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_true(is.na(module$along))
  expect_equal(module$aliases, list(dep = "foo/bar"))
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$provider, (function(dep) {
    return(dep)
  }))
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_true(module$first_instance)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp_1))
  expect_lt(as.numeric(module$timestamp), as.numeric(timestamp_2))
})

test_that("re-define writes to the registry when changes occur", {
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

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[["some/module"]]

  expect_equal(module$name, "some/module")
  expect_true(is.na(module$along))
  expect_equal(module$aliases, list(dep = "foo/bar"))
  expect_equal(module$dependencies, list(dep = "foo/bar"))
  expect_equal(module$provider, (function(dep) {
    return(sprintf("%s", dep))
  }))
  expect_equal(module$digest, get_digest("some/module"))
  expect_true(is.null(module$instance))
  expect_false(module$instanciated)
  expect_false(module$first_instance)
  expect_null(module$url)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))
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

test_that("define accepts provider as constant", {
  reset()
  define("foo", NULL, "foo")
  expect_equal(make("foo"), "foo")

  define("foo", NULL, 1L)
  expect_equal(make("foo"), 1L)

  define("foo", NULL, 1.0)
  expect_equal(make("foo"), 1.0)

  define("foo", NULL, T)
  expect_equal(make("foo"), T)

  define("foo", NULL, NA)
  expect_equal(make("foo"), NA)

  define("foo", NULL, NULL)
  expect_equal(make("foo"), NULL)

  expect_error(define("foo", NULL, c("foo", "bar")))
  expect_error(define("foo", NULL, list("foo")))
  expect_error(define("foo", NULL, list(foo = "foo")))
  expect_error(define("foo", NULL, list("foo", "bar")))
  expect_error(define("foo", NULL, expression(x + 1)))
  expect_error(define("foo", NULL, quote(exp)))

  reset()
  "foo" %provides% "foo"
  expect_equal(make("foo"), "foo")

  "foo" %provides% 1L
  expect_equal(make("foo"), 1L)

  "foo" %provides% 1.0
  expect_equal(make("foo"), 1.0)

  "foo" %provides% T
  expect_equal(make("foo"), T)

  "foo" %provides% NA
  expect_equal(make("foo"), NA)

  "foo" %provides% NULL
  expect_equal(make("foo"), NULL)

  expect_error("foo" %provides% c("foo", "bar"))
  expect_error("foo" %provides% list("foo"))
  expect_error("foo" %provides% list(foo = "foo"))
  expect_error("foo" %provides% list("foo", "bar"))
  expect_error("foo" %provides% expression(x + 1))
  expect_error("foo" %provides% quote(exp))
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

  expect_output(define("foobar", list(foo = "foo"),
                       function(foo) paste0(foo, "bar")), "foobar")

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
               (function(dep) {
                 return(dep)
               }))

  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_equal(get_provider("some/module"),
               (function(dep) {
                 return(dep)
               }))

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

test_that("get_dependencies returns the dependencies of the module", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    NULL)

  expect_equal(
    get_dependencies("some/module"),
    list(dep = "foo/bar")
  )

  define(
    "some/other/module",
    list(
      module = "some/module",
      dep = "foo/bar"),
    NULL)

  expect_equal(
    get_dependencies("some/other/module"),
    list(module = "some/module", dep = "foo/bar")
  )

})

test_that("get_dependencies is able to find an undefined module", {
  reset()

  expect_error(get_dependencies("unexisting/module", load = F))

  expect_error(get_dependencies("unexisting/module", load = T))

  expect_error(get_dependencies("module_1", load = F))

  expect_equal(get_dependencies("module_1", load = T),
               list())

  expect_error(get_dependencies("module_2", load = F))

  expect_equal(get_dependencies("module_2", load = T),
               list(m_1 = "module_1"))

})

test_that("get_dependencies calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    get_dependencies("module_1", load = T)
  })
  expect_warning(make("module"))
})

test_that("reset purges the registry", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  reset()

  registry <- get("registry", pos = .modulr_env$injector)

  expect_equal(names(registry), MODULR_NAME)

  })

test_that("reset(all=T) purges the stashes", {
  reset(all = T)
  expect_equal(length(.modulr_env$injector$stash), 0)
  stash()
  reset(all = F)
  expect_false(length(.modulr_env$injector$stash) == 0)
  reset(all = T)
  expect_equal(length(.modulr_env$injector$stash), 0)
})

test_that("reset calls generate an error from within a module", {
  reset()
  define("module", NULL, function() {
    reset()
  })
  expect_error(make("module"))
})

test_that("undefine removes the module definition from the registry", {
  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  registry <- get("registry", pos = .modulr_env$injector)

  expect_true("some/module" %in% names(registry))

  status <- undefine("some/module")
  expect_null(status)

  registry <- get("registry", pos = .modulr_env$injector)

  expect_false("some/module" %in% names(registry))

  })

test_that("undefine removes only registryed modules", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_null(undefine("some/module"))
  expect_error(undefine("some/unregistryed/module"))

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

test_that("touch updates the registry", {
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

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[["some/module"]]

  expect_null(module$instance)
  expect_false(module$instanciated)
  expect_null(module$digest)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))

  })

test_that("touch updates only registryed modules", {
  reset()

  define(
    "some/module",
    list(dep = "foo/bar"),
    function(dep) {
      return(dep)
    })

  expect_null(touch("some/module"))
  expect_error(touch("some/unregistryed/module"))

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

test_that("define detects unused dependencies", {
  reset()
  expect_warning(define("module_1", NULL, function() NULL), regexp = NA)
  expect_warning(define("module_1", list(foo = "foo"), function() NULL))
  expect_warning(
    define("module_1", list(foo = "foo"), function() foo),
    regexp = NA)
})
