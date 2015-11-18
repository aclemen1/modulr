context("modulr")

test_that(".deprecated outputs breadcrumbs from modules", {
  reset()
  define("foo", NULL, function() .deprecated())
  expect_warning(make("foo"), "breadcrumbs")
})

test_that(".deprecated doesn't output breadcrumbs from outside modules", {
  expect_warning(.deprecated(), "^((?!breadcrumbs)[\\S\\s])*$", perl = TRUE)
})
