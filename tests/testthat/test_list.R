context("list")

test_that("list_modules can hide special modules", {
  reset()
  expect_equal(list_modules(all = T, wide = F), "modulr")
  expect_null(list_modules(all = F, wide = F))
})

test_that("list_modules sorts lexicographically", {
  reset()
  define("a", NULL, function() {})
  define("c", NULL, function() {})
  define("b", NULL, function() {})
  expect_equal(list_modules(all = F, wide = F), c("a", "b", "c"))
})

test_that("list_modules can filter by regexp", {
  reset()
  define("hello_world", NULL, function() {})
  define("hello", NULL, function() {})
  define("world", NULL, function() {})
  expect_equal(list_modules("^hello", all = F, wide = F),
               c("hello", "hello_world"))
  expect_equal(list_modules("world$", all = F, wide = F),
               c("hello_world", "world"))
  expect_equal(list_modules("hello|world", all = F, wide = F),
               c("hello", "hello_world", "world"))
})

test_that("list_modules can return a data frame", {
  reset()
  expect_true(is.data.frame(list_modules(all = T, wide = T)))
})

test_that("list_modules can return selected infos", {
  reset()
  expect_equal(names(list_modules(all = T, wide = T, cols = c("created"))),
               c("name", "created"))
  })
