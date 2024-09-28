context("modulr")

test_that("new_injector returns a new injector", {
  injector <- new_injector()
  expect_true(is.environment(injector))
  expect_named(
    injector,
    c("registry", ".message_level", "config", "get", "provider",
      "reset", "touch", "shared_env"),
    ignore.order = TRUE)
  expect_named(injector$registry, c(MODULR_NAME), ignore.order = TRUE)
  expect_equal(
    injector$config[[".__root__"]][[1]],
    unique(eval(DEFAULT_ROOT_CONFIG)))
})

test_that("get_default_injector returns the default injector", {
  set_injector(get_default_injector())
  on.exit({
    set_injector(get_default_injector())
  })
  expect_identical(get_default_injector(), .default_injector)
  set_injector()
  expect_identical(get_default_injector(), .default_injector)
})

test_that("get_injector returns the current injector", {
  set_injector(get_default_injector())
  on.exit({
    rm(inj_1)
    set_injector(get_default_injector())
  })
  expect_identical(get_injector(), get_default_injector())
  inj_1 <- new.env()
  set_injector(inj_1)
  expect_identical(get_injector(), inj_1)
  set_injector(get_default_injector())
  expect_identical(get_injector(), get_default_injector())
})

test_that("set_injector stops when called from within a module", {
  reset()
  define("foo", NULL, function() {
    set_injector()
  })
  expect_error(make("foo"))
})

test_that("set_injector returns the previous injector", {
  set_injector(get_default_injector())
  on.exit({
    rm(default, inj_1, inj_2)
    set_injector(get_default_injector())
  })

  default <- set_injector()
  expect_identical(default, get_default_injector())
  inj_1 <- set_injector()
  inj_2 <- get_injector()
  set_injector(default)

  expect_identical(set_injector(inj_1), default)
  expect_identical(set_injector(inj_2), inj_1)
  expect_identical(set_injector(default), inj_2)

  expect_identical(set_injector(inj_2), default)
  expect_identical(set_injector(inj_1), inj_2)
  expect_identical(set_injector(default), inj_1)

  expect_false(identical(default, inj_1))
  expect_false(identical(inj_1, inj_2))
  expect_false(identical(inj_2, default))
})

test_that(".dir_exists flags existing dirs", {
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  expect_true(.dir_exists(tmp_dir))
  tmp_file <- file.path(tmp_dir, "test")
  cat("Hello World!", file = tmp_file)
  expect_false(.dir_exists(tmp_file))
  unlink(tmp_dir, recursive = TRUE)
  expect_false(.dir_exists(tmp_dir))
})

test_that(".deprecated outputs breadcrumbs from modules", {
  reset()
  define("foo", NULL, function() .deprecated())
  expect_warning(make("foo"), "breadcrumbs")
})

test_that(".deprecated doesn't output breadcrumbs from outside modules", {
  expect_warning(.deprecated(), "^((?!breadcrumbs)[\\S\\s])*$", perl = TRUE)
})
