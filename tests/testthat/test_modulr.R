context("modulr")

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
