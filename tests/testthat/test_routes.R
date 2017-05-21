context("routes")

test_that("find_path finds module paths", {
  # Most of the tests are made for .resolve_name(). Therefore, we just test the
  # output contract.
  reset()
  expect_null(find_path("unexisting"))
  define("test", NULL, NULL)
  expect_null(find_path("test"))
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("test", NULL, NULL)', file = tmp_file)
  result <- find_path("test")
  expect_named(result, "test")
  expect_equal(as.vector(result), tmp_file)
})

test_that("find_module finds modules", {
  # Most of the tests are made for .resolve_name(). Therefore, we just test the
  # output contract.
  reset()
  expect_null(find_module("unexisting"))
  define("test", NULL, NULL)
  expect_named(
    find_module("test"),
    c("name", "version", "storage", "filepath"),
    ignore.order = TRUE)
})

test_that(".is_sub_version flags sub-versions correctly", {
  expect_true(.is_sub_version(NA, numeric_version("1.0")))
  expect_true(.is_sub_version(numeric_version("1.0"), numeric_version("1.0")))
  expect_true(.is_sub_version(numeric_version("1.0"), numeric_version("1.0.1")))
  expect_false(
    .is_sub_version(numeric_version("1.0.0"), numeric_version("1.0")))
  expect_false(
    .is_sub_version(numeric_version("1.0"), numeric_version("1.1.0")))
})

test_that(".resolve_name resolves a module", {

  na_version <- numeric_version("", strict = FALSE)

  # Testing on-disk modules

  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("test", NULL, NULL)', file = tmp_file)
  result <- .resolve_name("test")
  resolved <- result[["resolved"]]
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = "test"
    )[names(resolved[[1]])]
  )
  unlink(tmp_dir, recursive = TRUE)

  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("test#1.0.0", NULL, NULL)', file = tmp_file)
  result <- .resolve_name("test")
  resolved <- result[["resolved"]]
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  unlink(tmp_dir, recursive = TRUE)

  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test#1.0.R")
  cat('define("test#1.0.1", NULL, NULL)', file = tmp_file)
  result <- .resolve_name("test")
  resolved <- result[["resolved"]]
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.1"),
      filepath = tmp_file,
      name = "test#1.0.1"
    )[names(resolved[[1]])]
  )
  unlink(tmp_dir, recursive = TRUE)

  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test#1.0.R")
  cat('define("test#1.1.0", NULL, NULL)', file = tmp_file)
  result <- .resolve_name("test")
  resolved <- result[["resolved"]]
  expect_equal(resolved, list())
  unlink(tmp_dir, recursive = TRUE)

  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("something_else", NULL, NULL)', file = tmp_file)
  result <- .resolve_name("test")
  resolved <- result[["resolved"]]
  expect_equal(resolved, list())
  unlink(tmp_dir, recursive = TRUE)

  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("test", NULL, NULL)', file = tmp_file)
  tmp_file_100 <- file.path(tmp_dir, "test#1.0.0.R")
  cat('define("test#1.0.0", NULL, NULL)', file = tmp_file_100)
  result <- .resolve_name("test")
  resolved <- result[["resolved"]]
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  expect_equal(
    resolved[[2]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = "test"
    )[names(resolved[[2]])]
  )

  # Testing that an in-memory module if resolved correctly.
  reset()
  define("test#1.1.1", NULL, NULL)
  expect_equal(
    .resolve_name("test", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#1.1.1", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#1.1.0", all = FALSE)[["resolved"]],
    list()
  )
  expect_equal(
    .resolve_name("test#~1.1.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#~1.1.2", all = FALSE)[["resolved"]],
    list()
  )
  expect_equal(
    .resolve_name("test#^1.1.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#^1.0.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#^1.2.0", all = FALSE)[["resolved"]],
    list()
  )
  expect_equal(
    .resolve_name("test#>=1.1.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=1.1", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=1", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=1.0.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=1.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=0.0.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=0.0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=0", all = FALSE)[["resolved"]][[1L]][["version"]],
    numeric_version("1.1.1")
  )
  expect_equal(
    .resolve_name("test#>=2.0.0", all = FALSE)[["resolved"]],
    list()
  )

  # Testing that if an in-memory module has a corresponding on-disk instance
  # with same version number, we keep the on-disk module only.
  reset()
  define("test#1.0.0", NULL, NULL)
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)
  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("test#1.0.0", NULL, NULL)', file = tmp_file)
  expect_equal(length(.resolve_name("test")[["resolved"]]), 2L)
  result <- .resolve_name("test", all = FALSE)
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  unlink(tmp_dir, recursive = TRUE)

  # Testing that we keep the highest version.
  reset()
  define("test#1.0.0", NULL, NULL)
  define("test#0.0.0.9999", NULL, NULL)
  define("test#1.0.1", NULL, NULL)
  result <- .resolve_name("test", all = FALSE)
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.1"),
      filepath = NA_character_,
      name = "test#1.0.1"
    )[names(resolved[[1]])]
  )

  # Testing that we keep the unversionned module if present.
  reset()
  define("test#1.0.0", NULL, NULL)
  define("test#0.0.0.9999", NULL, NULL)
  define("test#1.0.1", NULL, NULL)
  define("test", NULL, NULL)
  result <- .resolve_name("test", all = FALSE)
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[1]])]
  )

})

test_that(".resolve_candidates resolves candidates for a module", {

  na_version <- numeric_version("", strict = FALSE)

  # Testing in-memory candidates

  reset()
  resolved <- .resolve_candidates("unexisting")[["resolved"]]
  expect_equal(resolved, list())

  reset()
  define("test", NULL, NULL)
  result <- .resolve_candidates("test")
  expect_named(result, c("name", "scope_name", "resolved", "namespace"),
               ignore.order = TRUE)
  resolved <- result[["resolved"]]
  expect_named(resolved[[1]], c("storage", "version", "filepath", "name"),
               ignore.order = TRUE)
  expect_equal(
    resolved,
    list(list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[1]])])
  )

  reset()
  define("test#1.0.0", NULL, NULL)
  result <- .resolve_candidates("test")
  resolved <- result[["resolved"]]
  expect_equal(
    resolved,
    list(list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])])
  )

  reset()
  define("test/module_test", NULL, NULL)
  paths_config$set("other_test" = "test")
  resolved <- .resolve_candidates("other_test/module_test")[["resolved"]]
  expect_equal(
    resolved,
    list(list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test/module_test"
    )[names(resolved[[1]])])
  )

  reset()
  define("test", NULL, NULL)
  maps_config$set(other = c("other_test" = "test"))
  resolved <- .resolve_candidates("other_test", "other")[["resolved"]]
  expect_equal(
    resolved,
    list(list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[1]])])
  )

  reset()
  define("test/module_test", NULL, NULL)
  paths_config$set("other_test" = "test")
  maps_config$set(
    test = c("other_test/module_other" = "other_test/module_test"))
  resolved <-
    .resolve_candidates("other_test/module_other", "test")[["resolved"]]
  expect_equal(
    resolved,
    list(list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test/module_test"
    )[names(resolved[[1]])])
  )

  reset()
  define("test", NULL, NULL)
  define("test#1.0.0", NULL, NULL)
  define("test#1.0.1", NULL, NULL)
  define("test#1.1.0", NULL, NULL)
  define("test#2.0.0", NULL, NULL)

  result <- .resolve_candidates("test")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 5L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[5]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[5]])]
  )

  result <- .resolve_candidates("test#>=1.0.1")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 4L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.1"),
      filepath = NA_character_,
      name = "test#1.0.1"
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[4]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[4]])]
  )

  result <- .resolve_candidates("test#^1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 4L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[4]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[4]])]
  )

  result <- .resolve_candidates("test#~1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 3L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[3]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[3]])]
  )

  result <- .resolve_candidates("test#1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )

  # Testing on-disk candidates
  reset()
  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  root_config$set(tmp_dir)

  tmp_dir_300 <- file.path(tmp_dir, "test#3.0.0")
  dir.create(tmp_dir_300)
  result <- .resolve_candidates("test", include.dirs = TRUE)
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("3.0.0"),
      filepath = tmp_dir_300,
      name = NA_character_
    )[names(resolved[[1]])]
  )
  unlink(tmp_dir_300, recursive = TRUE)
  on.exit(unlink(tmp_dir_300, recursive = TRUE), add = TRUE)

  tmp_file <- file.path(tmp_dir, "test.R")
  cat('define("test", NULL, NULL)', file = tmp_file)
  tmp_file_100 <- file.path(tmp_dir, "test#1.0.0.R")
  cat('"test#1.0.0" %requires% list() %provides% {}', file = tmp_file_100)
  tmp_file_101 <- file.path(tmp_dir, "test#1.0.1.Rmd")
  cat('```{r}\nlibrary(modulr)\ndefine("test#1.0.1", NULL, NULL)\n```',
      file = tmp_file_101)
  tmp_file_110 <- file.path(tmp_dir, "test#1.1.0.Rnw")
  cat('<<>>=\nlibrary(modulr)\ndefine("test#1.1.0", NULL, NULL)\n@\n',
      file = tmp_file_110)
  tmp_file_200 <- file.path(tmp_dir, "test#2.0.0.R")
  cat('define("test#2.0.0", NULL, NULL)', file = tmp_file_200)

  result <- .resolve_candidates("test")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 5L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[5]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[5]])]
  )

  result <- .resolve_candidates("test#>=1.0.1")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 4L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.1"),
      filepath = tmp_file_101,
      name = NA_character_
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[4]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[4]])]
  )

  result <- .resolve_candidates("test#^1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 4L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[4]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[4]])]
  )

  result <- .resolve_candidates("test#~1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 3L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[3]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[3]])]
  )

  result <- .resolve_candidates("test#1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[1]])]
  )

  # Mixed (in-memory and on-disk)
  define("test", NULL, NULL)
  define("test#1.0.0", NULL, NULL)

  result <- .resolve_candidates("test")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 7L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  expect_equal(
    resolved[[2]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[2]])]
  )
  # ...
  expect_equal(
    resolved[[6]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[6]])]
  )
  expect_equal(
    resolved[[7]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[7]])]
  )

  result <- .resolve_candidates("test#>=1.0.1")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 5L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.1"),
      filepath = tmp_file_101,
      name = NA_character_
    )[names(resolved[[1]])]
  )
  # ...
  expect_equal(
    resolved[[4]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[4]])]
  )
  expect_equal(
    resolved[[5]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[4]])]
  )

  result <- .resolve_candidates("test#^1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 6L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  expect_equal(
    resolved[[2]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[2]])]
  )
  # ...
  expect_equal(
    resolved[[5]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[5]])]
  )
  expect_equal(
    resolved[[6]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[6]])]
  )

  result <- .resolve_candidates("test#~1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 5L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  expect_equal(
    resolved[[2]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[2]])]
  )
  # ...
  expect_equal(
    resolved[[4]],
    list(
      storage = "in-memory",
      version = na_version,
      filepath = NA_character_,
      name = "test"
    )[names(resolved[[4]])]
  )
  expect_equal(
    resolved[[5]],
    list(
      storage = "on-disk",
      version = na_version,
      filepath = tmp_file,
      name = NA_character_
    )[names(resolved[[5]])]
  )

  result <- .resolve_candidates("test#1.0.0")
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 2L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "in-memory",
      version = numeric_version("1.0.0"),
      filepath = NA_character_,
      name = "test#1.0.0"
    )[names(resolved[[1]])]
  )
  expect_equal(
    resolved[[2]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = tmp_file_100,
      name = NA_character_
    )[names(resolved[[2]])]
  )

  undefine("test#1.0.0")
  oldwd <- setwd(tmp_dir)
  on.exit(try(setwd(oldwd), silent = TRUE), add = TRUE)
  root_config$set(".")
  result <- .resolve_candidates("test#1.0.0", absolute = FALSE)
  resolved <- result[["resolved"]]
  expect_equal(length(resolved), 1L)
  expect_equal(
    resolved[[1]],
    list(
      storage = "on-disk",
      version = numeric_version("1.0.0"),
      filepath = file.path(".", "test#1.0.0.R"),
      name = NA_character_
    )[names(resolved[[1]])]
  )
  setwd(oldwd)
})

test_that(".extract_name extracts the module name of a module definition", {
  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- ""
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_null(.extract_name(file))

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- "define('modulr_test', NULL, function() NULL)"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- "define('modulr_test', NULL, function() NULL" # syntax error
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_error(.extract_name(file))

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- paste(c(
    rep("foo <- 'bar'", 10L),
    "define('modulr_test', NULL, function() NULL"
    ), collapse = "\n")
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_error(.extract_name(file))

  file <- tempfile("modulr_test", fileext = ".Rmd")
  module_text <-
    "```{r}\nlibrary(modulr)\ndefine('modulr_test', NULL, function() NULL)\n```"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".Rnw")
  module_text <-
    "<<>>=\nlibrary(modulr)\ndefine('modulr_test', NULL, function() NULL)\n@\n"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- "define('modulr_test#1.2.3', NULL, function() NULL)"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test#1.2.3")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- "'modulr_test' %requires% list() %provides% {}"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- "'modulr_test' %provides% {}"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- "# This is a comment\n\n'modulr_test' %provides% {}"
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <-
    paste(c(rep("foo <- 'bar'", 10L),
            "'modulr_test' %provides% {}"), collapse = "\n")
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test")

  file <- tempfile("modulr_test", fileext = ".R")
  module_text <- paste(
    "define('modulr_test_1', NULL, function() NULL)",
    "define('modulr_test_2', NULL, function() NULL)",
    sep = "\n")
  write(module_text, file)
  on.exit(unlink(file), add = TRUE)
  expect_equal(.extract_name(file), "modulr_test_1")
  expect_equal(.extract_name(file, namespace = "modulr_test_1"),
               "modulr_test_1")
  expect_equal(.extract_name(file, namespace = "modulr_test_2"),
               "modulr_test_2")
  expect_null(.extract_name(file, namespace = "modulr_test_foo"))

})

test_that(".flatten_versions and .unflatten_versions are inverses", {
  expect_equal(.flatten_versions(list()), list())
  expect_equal(.unflatten_versions(list()), list())
  na_version <- numeric_version("", strict = FALSE)
  na_in_memory <- na_version
  attr(na_in_memory, "storage") <- "in-memory"
  v101 <- numeric_version("1.0.1")
  v101_in_memory <- v101
  attr(v101_in_memory, "storage") <- "in-memory"
  v101_on_disk <- v101
  attr(v101_on_disk, "storage") <- "on-disk"
  expect_error(
    .unflatten_versions(stats::setNames(list(na_version), c("na_version"))))
  expect_equal(
    .unflatten_versions(stats::setNames(list(na_in_memory), c("na_in_memory"))),
    list(
      list(
        storage = "in-memory",
        version = na_version,
        filepath = NA_character_,
        name = "na_in_memory"
      )
    )
  )
  expect_equal(
    .flatten_versions(
      .unflatten_versions(stats::setNames(list(na_in_memory),
                                          c("na_in_memory")))),
    stats::setNames(list(na_in_memory), c("na_in_memory"))
  )
  expect_equal(
    .unflatten_versions(stats::setNames(list(v101_on_disk), c("v101_on_disk"))),
    list(
      list(
        storage = "on-disk",
        version = v101,
        filepath = "v101_on_disk",
        name = NA_character_
      )
    )
  )
  expect_equal(
    .flatten_versions(
      .unflatten_versions(stats::setNames(list(v101_on_disk),
                                          c("v101_on_disk")))),
    stats::setNames(list(v101_on_disk), c("v101_on_disk"))
  )
  expect_equal(
    .flatten_versions(list(
      list(
        storage = "in-memory",
        version = na_version,
        filepath = NA_character_,
        name = "na_in_memory"
      )
    )),
    stats::setNames(list(na_in_memory), c("na_in_memory"))
  )
  expect_equal(
    .unflatten_versions(
      .flatten_versions(list(
      list(
        storage = "in-memory",
        version = na_version,
        filepath = NA_character_,
        name = "na_in_memory"
      )
    ))),
    list(
      list(
        storage = "in-memory",
        version = na_version,
        filepath = NA_character_,
        name = "na_in_memory"
      )
    )
  )
  expect_equal(
    .flatten_versions(list(
      list(
        storage = "on-disk",
        version = v101,
        filepath = "v101_on_disk",
        name = NA_character_
      )
    )),
    stats::setNames(list(v101_on_disk), c("v101_on_disk"))
  )
  expect_equal(
    .unflatten_versions(
      .flatten_versions(list(
      list(
        storage = "on-disk",
        version = v101,
        filepath = "v101_on_disk",
        name = NA_character_
      )
    ))),
    list(
      list(
        storage = "on-disk",
        version = v101,
        filepath = "v101_on_disk",
        name = NA_character_
      )
    )
  )
  expect_equal(
    .flatten_versions(
      .unflatten_versions(
        stats::setNames(list(na_in_memory, v101_in_memory, v101_on_disk),
                        c("na_in_memory", "v101_in_memory", "v101_on_disk")))),
    stats::setNames(list(na_in_memory, v101_in_memory, v101_on_disk),
                    c("na_in_memory", "v101_in_memory", "v101_on_disk"))
  )
})

test_that(".filter_versions filters versions", {
  na_version <- numeric_version("", strict = FALSE)
  expect_equal(.filter_versions(list(), na_version, NA), list())
  expect_equal(
    .filter_versions(list(na_version), na_version, NA),
    list(na_version)
  )
  expect_equal(
    .filter_versions(list(na_version, na_version), na_version, NA),
    list(na_version)
  )
  na_in_memory <- na_version
  attr(na_in_memory, "storage") <- "in-memory"
  expect_equal(
    .filter_versions(
      list(
        na_in_memory,
        na_in_memory
      ), na_version, NA),
    list(na_in_memory)
  )
  na_on_disk <- na_version
  attr(na_on_disk, "storage") <- "on-disk"
  expect_equal(
    .filter_versions(
      list(
        na_in_memory,
        na_on_disk
      ), na_version, NA),
    list(na_in_memory, na_on_disk)
  )
  v100 <- numeric_version("1.0.0")
  expect_equal(
    .filter_versions(
      list(
        na_version,
        v100
      ), na_version, NA),
    list(v100, na_version)
  )
  expect_equal(
    .filter_versions(list(v100), v100, NA),
    list(v100)
  )
  v101 <- numeric_version("1.0.1")
  expect_equal(
    .filter_versions(list(v100, v101), v100, NA),
    list(v100)
  )
  expect_equal(
    .filter_versions(list(v101), v100, NA),
    list()
  )
  v110 <- numeric_version("1.1.0")
  v111 <- numeric_version("1.1.1")
  v200 <- numeric_version("2.0.0")
  v101_in_memory <- v101
  attr(v101_in_memory, "storage") <- "in-memory"
  v101_on_disk <- v101
  attr(v101_on_disk, "storage") <- "on-disk"
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), na_version, NA),
    list(v100, v101, v101_in_memory, v101_on_disk, v110, v111, v200, na_version)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v101, NA),
    list(v101, v101_in_memory, v101_on_disk)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v110, NA),
    list(v110)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v100, "~"),
    list(v100, v101, v101_in_memory, v101_on_disk)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v110, "^"),
    list(v110, v111)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v111, ">="),
    list(v111, v200)
  )
})

test_that(".filter_versions filters versions including NAs", {
  na_version <- numeric_version("", strict = FALSE)
  expect_equal(.filter_versions(list(), na_version, NA), list())
  expect_equal(
    .filter_versions(list(na_version), na_version, NA, include_NAs = TRUE),
    list(na_version)
  )
  expect_equal(
    .filter_versions(list(na_version, na_version), na_version, NA,
                     include_NAs = TRUE),
    list(na_version)
  )
  na_in_memory <- na_version
  attr(na_in_memory, "storage") <- "in-memory"
  expect_equal(
    .filter_versions(
      list(
        na_in_memory,
        na_in_memory
      ), na_version, NA, include_NAs = TRUE),
    list(na_in_memory)
  )
  na_on_disk <- na_version
  attr(na_on_disk, "storage") <- "on-disk"
  expect_equal(
    .filter_versions(
      list(
        na_in_memory,
        na_on_disk
      ), na_version, NA, include_NAs = TRUE),
    list(na_in_memory, na_on_disk)
  )
  v100 <- numeric_version("1.0.0")
  expect_equal(
    .filter_versions(
      list(
        na_version,
        v100
      ), na_version, NA, include_NAs = TRUE),
    list(v100, na_version)
  )
  expect_equal(
    .filter_versions(list(v100), v100, NA, include_NAs = TRUE),
    list(v100)
  )
  v101 <- numeric_version("1.0.1")
  expect_equal(
    .filter_versions(list(v100, v101), v100, NA, include_NAs = TRUE),
    list(v100)
  )
  expect_equal(
    .filter_versions(list(v101), v100, NA, include_NAs = TRUE),
    list()
  )
  v110 <- numeric_version("1.1.0")
  v111 <- numeric_version("1.1.1")
  v200 <- numeric_version("2.0.0")
  v101_in_memory <- v101
  attr(v101_in_memory, "storage") <- "in-memory"
  v101_on_disk <- v101
  attr(v101_on_disk, "storage") <- "on-disk"
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), na_version, NA, include_NAs = TRUE),
    list(v100, v101, v101_in_memory, v101_on_disk, v110, v111, v200, na_version)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v101, NA, include_NAs = TRUE),
    list(v101, v101_in_memory, v101_on_disk)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v110, NA, include_NAs = TRUE),
    list(v110)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v100, "~", include_NAs = TRUE),
    list(v100, v101, v101_in_memory, v101_on_disk, na_version)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v110, "^", include_NAs = TRUE),
    list(v110, v111, na_version)
  )
  expect_equal(
    .filter_versions(
      list(
        v110,
        v101,
        v200,
        na_version,
        v101_in_memory,
        v100,
        v101_on_disk,
        v101,
        v111
      ), v111, ">=", include_NAs = TRUE),
    list(v111, v200, na_version)
  )
})

test_that(".path_to_name and .name_to_path are inverses", {
  name <- "foo/bar/foobar#^1.0.0/test"
  expect_equal(
    .path_to_name(.name_to_path(name)),
    name
  )
  path <- "foo/bar/foobar#^1.0.0/test"
  expect_equal(
    .name_to_path(.path_to_name(path)),
    path
  )
  good_path <- "foo/bar/foobar/"
  expect_equal(.path_to_name(good_path), .remove_trailing_filesep(good_path))
  good_path <- "foo/bar///foobar/"
  expect_equal(.path_to_name(good_path),
               .remove_trailing_filesep(.remove_duplicate_filesep(good_path)))
  bad_path <- "foo/bar/foobar/."
  expect_error(.path_to_name(bad_path))
  bad_path <- "./foo/bar/foobar"
  expect_error(.path_to_name(bad_path))
})

test_that(".parse_name returns a list of informations", {
  n <- .parse_name("foo/bar/foobar#^1.0.0/test")
  expect_true(is.list(n))
  expect_named(n, c(
    "name",
    "namespace",
    "initials",
    "final",
    "symbol",
    "version",
    "suffix"), ignore.order = TRUE)
  expect_equal(n[["name"]], "foo/bar/foobar#^1.0.0/test")
  expect_equal(n[["namespace"]], "foo/bar/foobar#^1.0.0/test")
  expect_equal(n[["initials"]], "foo/bar/foobar#^1.0.0")
  expect_equal(n[["final"]], "test")
  expect_equal(n[["symbol"]], "^")
  expect_equal(n[["version"]], numeric_version("1.0.0"))
  expect_equal(n[["suffix"]], "test")
})

test_that(".parse_version returns a list of version and symbol", {
  v <- .parse_version("foobar#^1.0.0")
  expect_true(is.list(v))
  expect_named(v, c("version", "symbol"), ignore.order = TRUE)
  expect_equal(v[["version"]], numeric_version("1.0.0"))
  expect_equal(v[["symbol"]], "^")
  expect_equal(
    .parse_version("#1"),
    list(
      version = numeric_version("1"),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#1.0"),
    list(
      version = numeric_version("1.0"),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#~1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = "~")[
        names(v)]
  )
  expect_equal(
    .parse_version("#^1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = "^")[
        names(v)]
  )
  expect_equal(
    .parse_version("#>=1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = ">=")[
        names(v)]
  )
  expect_equal(
    .parse_version(""),
    list(
      version = numeric_version("", strict = FALSE),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#"),
    list(
      version = numeric_version("", strict = FALSE),
      symbol = NA_character_)[
        names(v)]
  )
})

test_that(".remove_trailing_filesep removes trailing fileseps", {
  expect_equal(
    .remove_trailing_filesep("foo"),
    "foo")
  expect_equal(
    .remove_trailing_filesep(paste0("foo", .Platform$file.sep)),
    "foo")
  expect_equal(
    .remove_trailing_filesep(
      paste(c("foo", rep(.Platform$file.sep, 2)), collapse = "")),
    "foo")
})

test_that(".remove_duplicate_filesep removes duplicate fileseps", {
  expect_equal(
    .remove_duplicate_filesep(paste0("foo", .Platform$file.sep)),
    paste0("foo", .Platform$file.sep))
  expect_equal(
    .remove_duplicate_filesep(
      paste(c("foo", rep(.Platform$file.sep, 2)), collapse = "")),
    paste0("foo", .Platform$file.sep))
  expect_equal(
    .remove_duplicate_filesep(
      paste(c("foo", rep(.Platform$file.sep, 3), "bar"), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar"), collapse = ""))
  expect_equal(
    .remove_duplicate_filesep(
      paste(c("foo", .Platform$file.sep, "bar"), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar"), collapse = ""))
  expect_equal(
    .remove_duplicate_filesep(
      paste(c("foo", rep(.Platform$file.sep, 2), "bar"), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar"), collapse = ""))
})

test_that(".make_path returns with a trailing filesep and only one", {
  expect_equal(.make_path(.Platform$file.sep), .Platform$file.sep)
  expect_equal(
    .make_path(paste0("foo", .Platform$file.sep)),
    paste0("foo", .Platform$file.sep))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar"), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
          collapse = ""))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
                     collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
          collapse = ""))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar",
                       rep(.Platform$file.sep, 2)), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
          collapse = ""))
})

test_that(".parse_filepath returns all the required stuff", {
  expect_error(.parse_filepath(NA))
  expect_error(.parse_filepath(NULL))
  expect_equal(.parse_filepath(""),
               list(
                 filename = "",
                 path = ".",
                 basename = "",
                 name = "",
                 extension = "",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath(" "),
               list(
                 filename = "",
                 path = ".",
                 basename = "",
                 name = "",
                 extension = "",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath("foo"),
               list(
                 filename = "foo",
                 path = ".",
                 basename = "foo",
                 name = "foo",
                 extension = "",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath("foo.R"),
               list(
                 filename = "foo.R",
                 path = ".",
                 basename = "foo.R",
                 name = "foo",
                 extension = "R",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath(paste0(.Platform$file.sep, "foo")),
               list(
                 filename = paste0(.Platform$file.sep, "foo"),
                 path = .Platform$file.sep,
                 basename = "foo",
                 name = "foo",
                 extension = "",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath(paste0(.Platform$file.sep, "foo.R")),
               list(
                 filename = paste0(.Platform$file.sep, "foo.R"),
                 path = .Platform$file.sep,
                 basename = "foo.R",
                 name = "foo",
                 extension = "R",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep)),
               list(
                 filename = paste0("foo", .Platform$file.sep),
                 path = "foo",
                 basename = "",
                 name = "",
                 extension = "",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar"),
                 path = "foo",
                 basename = "bar",
                 name = "bar",
                 extension = "",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar.R")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar.R"),
                 path = "foo",
                 basename = "bar.R",
                 name = "bar",
                 extension = "R",
                 version = numeric_version("", strict = FALSE)))
  expect_equal(
    .parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.0.0.R")),
    list(
      filename = paste0("foo", .Platform$file.sep, "bar#1.0.0.R"),
      path = "foo",
      basename = "bar#1.0.0.R",
      name = "bar#1.0.0",
      extension = "R",
      version = numeric_version("1.0.0", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.0.R")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar#1.0.R"),
                 path = "foo",
                 basename = "bar#1.0.R",
                 name = "bar#1.0",
                 extension = "R",
                 version = numeric_version("1.0", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.R")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar#1.R"),
                 path = "foo",
                 basename = "bar#1.R",
                 name = "bar#1",
                 extension = "R",
                 version = numeric_version("1", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.0.0")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar#1.0.0"),
                 path = "foo",
                 basename = "bar#1.0.0",
                 name = "bar#1.0.0",
                 extension = "",
                 version = numeric_version("1.0.0", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.0")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar#1.0"),
                 path = "foo",
                 basename = "bar#1.0",
                 name = "bar#1.0",
                 extension = "",
                 version = numeric_version("1.0", strict = FALSE)))
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar#1")),
               list(
                 filename = paste0("foo", .Platform$file.sep, "bar#1"),
                 path = "foo",
                 basename = "bar#1",
                 name = "bar#1",
                 extension = "",
                 version = numeric_version("1", strict = FALSE)))
})

test_that(".resolve_mapping resolves module names", {
  maps_config$set(
    "some/oldmodule" = list(
      "foo/bar" = "foo/bar_V1"
    ))
  expect_equal(.resolve_mapping("foo/bar", "some/oldmodule"),
               list(
                 name = "foo/bar",
                 scope_name = "some/oldmodule",
                 resolved = "foo/bar_V1")
               )
  expect_equal(.resolve_mapping("foo/bar"),
               list(
                 name = "foo/bar",
                 scope_name = NULL,
                 resolved = "foo/bar"
               ))

  maps_config$set(
    "some/oldmodule" = list(
      "foo/bar" = "foo/old/bar",
      "foo/bar" = "foo/new/bar"
    ))
  expect_warning(resol <- .resolve_mapping("foo/bar", "some/oldmodule"))
  expect_equal(resol, list(name = "foo/bar",
                           scope_name = "some/oldmodule",
                           resolved = "foo/old/bar"))
  maps_config$set(
    "some/oldmodule" = list(
      "foo/bar" = "foo/new/bar",
      "foo/bar" = "foo/old/bar"
    ))
  expect_warning(resol <- .resolve_mapping("foo/bar", "some/oldmodule"))
  expect_equal(resol, list(name = "foo/bar",
                           scope_name = "some/oldmodule",
                           resolved = "foo/new/bar"))

})

test_that(".resolve_namespace returns path relative to config and mapping", {
  paths_config$set(
    "bundle_1" = "lib/bundle_1",
    "bundle_2" = "lib/bundle_2")
  expect_equal(.resolve_namespace("foo/bar"),
               list(
                 name = "foo/bar",
                 scope_name = NULL,
                 resolved = "foo/bar",
                 mapping = .resolve_mapping("foo/bar")
               ))
  expect_equal(.resolve_namespace("bundle_1/foo/bar"),
               list(
                 name = "bundle_1/foo/bar",
                 scope_name = NULL,
                 resolved = "lib/bundle_1/foo/bar",
                 mapping = .resolve_mapping("bundle_1/foo/bar")
               ))
  expect_equal(.resolve_namespace("bundle_2/foo/baz"),
               list(
                 name = "bundle_2/foo/baz",
                 scope_name = NULL,
                 resolved = "lib/bundle_2/foo/baz",
                 mapping = .resolve_mapping("bundle_2/foo/baz")
               ))
  maps_config$set(
  "some/module" = list(
    "bundle_1/foo/bar" = "bundle_1/foo/bar_V1"
  ))
  expect_equal(.resolve_namespace("bundle_1/foo/bar", "some/module"),
               list(
                 name = "bundle_1/foo/bar",
                 scope_name = "some/module",
                 resolved = "lib/bundle_1/foo/bar_V1",
                 mapping = .resolve_mapping("bundle_1/foo/bar", "some/module")
               ))

})

test_that(".filter_versions works properly and keeps names", {
  expect_equal(
    .filter_versions(numeric_version(c(""), strict = FALSE),
                     numeric_version(c("1.0.0")), NA),
    list()
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0.0", ""), strict = FALSE),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0.0"), strict = FALSE))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0", ""), strict = FALSE),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0"), strict = FALSE))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0.0")),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0.0")))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0.0", "1.0.0")),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0.0")))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0.0", "1.0.1")),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0.0")))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0.0")),
                     numeric_version(c("1.0.0.9000")), NA),
    as.list(numeric_version(c("1.0.0")))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0", "1.0.0")),
                     numeric_version(c("1.0.0.9000")), NA),
    as.list(numeric_version(c("1.0", "1.0.0")))
  )
  expect_equal(
    .filter_versions(numeric_version(c(
      "2.0", "1.1.0", "1.0.1", "1.0.0", "0.0.0")),
      numeric_version(c("1.0.0")), ">="),
    as.list(numeric_version(c("1.0.0", "1.0.1", "1.1.0", "2.0")))
  )
  expect_equal(
    .filter_versions(numeric_version(c(
      "2.0", "2.0.0", "1.2", "1.1.0", "1.0.1", "1.0.0", "0.0.0")),
      numeric_version(c("1.0.0")), "^"),
    as.list(numeric_version(c("1.0.0", "1.0.1", "1.1.0", "1.2")))
  )
  expect_equal(
    .filter_versions(numeric_version(c(
      "2.0", "1.2.0", "1.2", "1.1.0", "1.0", "1.0.1", "1.0.0", "0.0.0")),
      numeric_version(c("1.0.0")), "~"),
    as.list(numeric_version(c("1.0", "1.0.0", "1.0.1")))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0", "", "1.0.1"), strict = FALSE),
                     numeric_version(c(""), strict = FALSE), NA),
    as.list(numeric_version(c("1.0", "1.0.1", ""), strict = FALSE))
  )
  expect_equal(
    .filter_versions(numeric_version(c(
      a = "2.0", b = "1.2.0", c = "1.2", d = "1.1.0",
      e = "1.0", f = "1.0.1", g = "1.0.0", h = "0.0.0")),
      numeric_version(c("1.0.0")), "~"),
    as.list(numeric_version(c(e = "1.0", g = "1.0.0", f = "1.0.1")))
  )
})
