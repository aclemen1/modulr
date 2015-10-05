context("load")

test_that(".is_defined return only T or F", {
  reset()
  define("bar", NULL, function() NULL)
  expect_false(.is_defined("foo"))
  expect_true(.is_defined("bar"))
  })

test_that("load_module finds and loads .R files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <- sprintf("define('%s', NULL, function() NULL)", name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  expect_equal(module_file, file)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_equal(module$name, name)

  })

test_that("load_module finds and loads .Rmd files", {
  reset()
  file <- tempfile("modulr_test", fileext = ".Rmd")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf(
      "```{r}\nlibrary(modulr)\ndefine('%s', NULL, function() NULL)\n```\n",
      name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  expect_equal(module_file, file)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_equal(module$name, name)

})

test_that("load_module finds and loads .Rnw files", {
  reset()
  file <- tempfile("modulr_test", fileext = ".Rnw")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf(
      "<<>>=\nlibrary(modulr)\ndefine('%s', NULL, function() NULL)\n@\n",
      name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  expect_equal(module_file, file)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_equal(module$name, name)

})

test_that("load_module re-loads modified .R files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <- sprintf("define('%s', NULL, function() NULL)", name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  timestamp <- Sys.time()

  module_text <- sprintf("define('%s', NULL, function() 'changed')", name)
  write(module_text, file)
  on.exit(unlink(file))
  module_file <- load_module(name)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

})

test_that("load_module re-loads modified .Rmd files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".Rmd")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf(
      "```{r}\nlibrary(modulr)\ndefine('%s', NULL, function() NULL)\n```\n",
      name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  timestamp <- Sys.time()

  module_text <-
    sprintf(
      paste0("```{r}\nlibrary(modulr)\ndefine('%s', ",
             "NULL, function() 'changed')\n```\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file))
  module_file <- load_module(name)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

})

test_that("load_module re-loads modified .Rnw files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".Rnw")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf(
      "<<>>=\nlibrary(modulr)\ndefine('%s', NULL, function() NULL)\n@\n",
      name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  timestamp <- Sys.time()

  module_text <-
    sprintf(
      paste0("<<>>=\nlibrary(modulr)\ndefine('%s', ",
             "NULL, function() 'changed')\n@\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file))
  module_file <- load_module(name)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

})

test_that("load_module calls are warned from within a module", {
  reset()
  define("module", NULL, function() {
    load_module("module_1")
  })
  expect_warning(make("module"))
})

test_that(".define_all_dependent_modules doesn't fall into cycles", {
  reset()

  file_1 <- tempfile("modulr_test", fileext = ".R")
  name_1 <- tools::file_path_sans_ext(basename(file_1))

  file_2 <- tempfile("modulr_test", fileext = ".R")
  name_2 <- tools::file_path_sans_ext(basename(file_2))

  module_1_text <- sprintf("define('%s', list(m2 = '%s'), function(m2) NULL)",
                           name_1, name_2)
  write(module_1_text, file_1)
  on.exit(unlink(file_1))

  module_2_text <- sprintf("define('%s', list(m1 = '%s'), function(m1) NULL)",
                           name_2, name_1)
  write(module_2_text, file_2)
  on.exit(unlink(file_2), add = T)

  root_config$set(tempdir())

  expect_equal(length(.define_all_dependent_modules(name_1)), 2)

})

test_that("load_all_modules finds and loads files in dir", {
  reset()

  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_null(load_all_modules(tmp_dir))

  tmp_file <- file.path(tmp_dir, "foo.R")
  cat('define("foo", NULL, function() print("Hello World!"))', file = tmp_file)
  tmp_file <- file.path(tmp_dir, "bar.R")
  cat('define("bar", NULL, function() print("hELLO wORLD?"))', file = tmp_file)
  load_all_modules(tmp_dir)

  register <- get("register", pos = modulr_env)

  expect_true(all(c("foo", "bar") %in% names(register)))

})

test_that("load_all_modules calls are warned from within a module", {
  reset()

  define("module", NULL, function() {
    tmp_dir <- tempfile("modulr_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE))
    load_all_modules(tmp_dir)
  })

  expect_warning(make("module"))
})
