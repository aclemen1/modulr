context("load")

test_that(".is_defined return only T or F", {
  reset()
  define("bar", NULL, function() NULL)
  expect_false(.is_defined("foo"))
  expect_true(.is_defined("bar"))
  })

test_that(".load_module loads modules along modules", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  other_name <- sprintf("%s/along", name)
  path <- dirname(file)

  module_text <- paste(
    sprintf("define('%s', NULL, function() NULL)", name),
    sprintf("define('%s', NULL, function() NULL)", other_name),
    sep = "\n")
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- .load_module(file, check = FALSE)

  expect_equal(unname(module_file), file)
  expect_true(.is_defined(other_name))

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]
  other_module <- registry[[other_name]]

  expect_equal(module$name, name)
  expect_equal(other_module$name, other_name)
  expect_equal(other_module$along, name)

})

test_that(".load_module guesses correct along-defined module name", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- "foo_name"
  other_name <- sprintf("%s/along", name)
  path <- dirname(file)

  module_text <- paste(
    sprintf("define('%s', NULL, function() NULL)", name),
    sprintf("define('%s', NULL, function() NULL)", other_name),
    sep = "\n")
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- .load_module(file, check = FALSE)

  expect_equal(unname(module_file), file)
  expect_true(.is_defined(other_name))

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]
  other_module <- registry[[other_name]]

  expect_equal(module$name, name)
  expect_equal(other_module$name, other_name)
  expect_equal(other_module$along, name)

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

  expect_equal(names(module_file), name)
  expect_equal(unname(module_file), file)

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]

  expect_equal(module$name, name)

  })

test_that("load_module rolls back on errors in .R files", {
  reset()
  registry <- get("registry", pos = .modulr_env$injector)

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf("define('%s', NULL, function() NULL); stop()", name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  expect_error(load_module(name))

  expect_identical(registry, get("registry", pos = .modulr_env$injector))
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

  expect_equal(names(module_file), name)
  expect_equal(unname(module_file), file)

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]

  expect_equal(module$name, name)

})

test_that("load_module rolls back on errors in .Rmd files", {
  reset()
  registry <- get("registry", pos = .modulr_env$injector)

  file <- tempfile("modulr_test", fileext = ".Rmd")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf(
      paste0(
      "```{r}\nlibrary(modulr)\ndefine('%s', NULL, function() NULL)\n",
      "stop()```\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  expect_error(load_module(name))
  expect_identical(registry, get("registry", pos = .modulr_env$injector))
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

  expect_equal(names(module_file), name)
  expect_equal(unname(module_file), file)

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]

  expect_equal(module$name, name)

})

test_that("load_module rolls back on errors in .Rnw files", {
  reset()
  registry <- get("registry", pos = .modulr_env$injector)

  file <- tempfile("modulr_test", fileext = ".Rnw")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <-
    sprintf(
      paste0(
        "<<>>=\nlibrary(modulr)\ndefine('%s', NULL, function() NULL)\n",
        "stop()\n@\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  expect_error(load_module(name))
  expect_identical(registry, get("registry", pos = .modulr_env$injector))
})

test_that("load_module finds and loads .Rnw from .Rmd files", {
  reset()
  path <- tempfile("modulr_")
  dir.create(path)
  on.exit(unlink(path))
  file_from <- tempfile("modulr_test_from", tmpdir = path, fileext = ".Rmd")
  name_from <- tools::file_path_sans_ext(basename(file_from))
  file <- tempfile("modulr_test", tmpdir = path, fileext = ".Rnw")
  name <- tools::file_path_sans_ext(basename(file))

  module_from_text <-
    sprintf(paste0(
      "```{r}\nlibrary(modulr)\n",
      "define('%s', list(foobar = '%s'), { foobar })\n```\n"), name_from, name)
  write(module_from_text, file_from)
  module_text <-
    sprintf(
        "<<>>=\nlibrary(modulr)\ndefine('%s', NULL, function() 'foobar')\n@\n",
      name)
  write(module_text, file)

  root_config$set(path)
  module_from <- load_module(name_from)

  expect_equal(names(module_from), name_from)
  expect_equal(unname(module_from), file_from)

  registry <- get("registry", pos = .modulr_env$injector)
  registry_module_from <- registry[[name_from]]

  expect_equal(registry_module_from$name, name_from)

  expect_equal(make(name_from), "foobar")

})

test_that("load_module finds and loads .Rmd from .Rnw files", {
  reset()
  path <- tempfile("modulr_")
  dir.create(path)
  on.exit(unlink(path))
  file_from <- tempfile("modulr_test_from", tmpdir = path, fileext = ".Rnw")
  name_from <- tools::file_path_sans_ext(basename(file_from))
  file <- tempfile("modulr_test", tmpdir = path, fileext = ".Rmd")
  name <- tools::file_path_sans_ext(basename(file))

  module_from_text <-
    sprintf(paste0(
      "<<>>=\nlibrary(modulr)\n",
      "define('%s', list(foobar = '%s'), { foobar })\n",
      "@\n"), name_from, name)
  write(module_from_text, file_from)
  module_text <-
    sprintf(paste0(
        "```{r}\nlibrary(modulr)\n",
        "define('%s', NULL, function() 'foobar')\n",
        "```\n"), name)
  write(module_text, file)

  root_config$set(path)
  module_from <- load_module(name_from)

  expect_equal(names(module_from), name_from)
  expect_equal(unname(module_from), file_from)

  registry <- get("registry", pos = .modulr_env$injector)
  registry_module_from <- registry[[name_from]]

  expect_equal(registry_module_from$name, name_from)

  expect_equal(make(name_from), "foobar")

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

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]

  expect_false(module$first_instance)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))

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

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]

  expect_false(module$first_instance)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))

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

  registry <- get("registry", pos = .modulr_env$injector)
  module <- registry[[name]]

  expect_false(module$first_instance)
  expect_lt(as.numeric(module$timestamp), as.numeric(Sys.time()))
  expect_gt(as.numeric(module$timestamp), as.numeric(timestamp))

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

  load_all_modules(path = tmp_dir)

  registry <- get("registry", pos = .modulr_env$injector)

  expect_true(all(c("foo", "bar") %in% names(registry)))

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

test_that("load_module doesn't recurse infinitely when sourced", {
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
        "if(env$deep > 3) stop()",
        "env$deep <- env$deep + 1",
        "load_module()", sep = "\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)

  root_config$set(path)

  source(file)

  expect_lte(test_env$deep, 3)

  expect_true(.is_defined(name))
})

test_that("load_module doesn't recurse infinitely when called", {
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
        "if(env$deep > 2) stop()",
        "env$deep <- env$deep + 1",
        "load_module()", sep = "\n"),
      name)
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)

  root_config$set(path)

  module_file <- load_module(name)

  expect_lte(test_env$deep, 2)

  expect_equal(names(module_file), name)
  expect_equal(unname(module_file), file)

  expect_true(.is_defined(name))
})
