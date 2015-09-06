context("import")

test_that(".is_defined return only T or F", {
  reset()
  define("bar", NULL, function() {})
  expect_false(.is_defined("foo"))
  expect_true(.is_defined("bar"))
  })

test_that("import finds and imports .R files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <- sprintf("define('%s', NULL, function() {})", name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  expect_equal(module_file, file)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_equal(module$name, name)

  })

test_that("import finds and imports .Rmd files", {
  reset()
  file <- tempfile("modulr_test", fileext = ".Rmd")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <- sprintf("```{r}\nlibrary(modulr)\ndefine('%s', NULL, function() {})\n```\n", name)
  write(module_text, file)
  #on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  expect_equal(module_file, file)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_equal(module$name, name)

})

test_that("import re-imports modified .R files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".R")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <- sprintf("define('%s', NULL, function() {})", name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  timestamp <- Sys.time()

  module_text <- sprintf("define('%s', NULL, function() {'changed'})", name)
  write(module_text, file)
  module_file <- load_module(name)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

})

test_that("import re-imports modified .Rmd files", {
  reset()

  file <- tempfile("modulr_test", fileext = ".Rmd")
  name <- tools::file_path_sans_ext(basename(file))
  path <- dirname(file)

  module_text <- sprintf("```{r}\nlibrary(modulr)\ndefine('%s', NULL, function() {})\n```\n", name)
  write(module_text, file)
  on.exit(unlink(file))

  root_config$set(path)
  module_file <- load_module(name)

  timestamp <- Sys.time()

  module_text <- sprintf("```{r}\nlibrary(modulr)\ndefine('%s', NULL, function() {'changed'})\n```\n", name)
  write(module_text, file)
  module_file <- load_module(name)

  register <- get("register", pos = modulr_env)
  module <- register[[name]]

  expect_false(module$first_instance)
  expect_less_than(module$timestamp, Sys.time())
  expect_more_than(module$timestamp, timestamp)

})
