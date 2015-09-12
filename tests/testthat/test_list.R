context("list")

test_that("list_modules can hide reserved modules", {
  reset()
  expect_equal(list_modules(reserved = T, wide = F), "modulr")
  expect_null(list_modules(reserved = F, wide = F))
})

test_that("list_modules sorts lexicographically", {
  reset()
  define("a", NULL, function() NULL)
  define("c", NULL, function() NULL)
  define("b", NULL, function() NULL)
  expect_equal(list_modules(reserved = F, wide = F), c("a", "b", "c"))
})

test_that("list_modules can filter by regexp", {
  reset()
  define("hello_world", NULL, function() NULL)
  define("hello", NULL, function() NULL)
  define("world", NULL, function() NULL)
  expect_equal(list_modules("^hello", reserved = F, wide = F),
               c("hello", "hello_world"))
  expect_equal(list_modules("world$", reserved = F, wide = F),
               c("hello_world", "world"))
  expect_equal(list_modules("hello|world", reserved = F, wide = F),
               c("hello", "hello_world", "world"))
})

test_that("list_modules can return a data frame", {
  reset()
  expect_true(is.data.frame(list_modules(reserved = T, wide = T)))
})

test_that("list_modules can return selected infos", {
  reset()
  expect_equal(names(list_modules(reserved = T, wide = T, cols = c("created"))),
               c("name", "created"))
  })
