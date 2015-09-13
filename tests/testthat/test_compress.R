context("compress")

test_that(".decompress() is a retract of .compress()", {
  object <- function() NULL
  expect_equal(.decompress(.compress(object)), object)
})
