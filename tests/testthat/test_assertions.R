context("assertions")

test_that(".is_defined detects defined modules", {
  reset()
  expect_true(.is_defined("modulr"))
  expect_false(.is_defined("undefined/module"))
  })

test_that(".is_undefined detects undefined modules", {
  reset()
  expect_false(.is_undefined("modulr"))
  expect_true(.is_undefined("undefined/module"))
})

test_that(".is_conform detects conform module names", {
  reset()
  expect_true(.is_conform("modulr"))
  expect_true(.is_conform("foo"))
  expect_false(.is_conform(".foo"))
})

test_that(".is_reserved detects reserved module names", {
  reset()
  expect_true(.is_reserved("modulr"))
  expect_false(.is_reserved("foo"))
})

test_that(".is_regular detects regular module names", {
  reset()
  expect_false(.is_regular("modulr"))
  expect_true(.is_regular("foo"))
})

test_that(".is_regular_core detects regular core module names", {
  reset()
  expect_false(.is_regular_core("modulr"))
  expect_false(.is_regular_core("foo/test"))
  expect_false(.is_regular_core("foo/tests"))
  expect_false(.is_regular_core("foo/mock"))
  expect_false(.is_regular_core("foo/mocks"))
  expect_false(.is_regular_core("foo/TEST"))
  expect_false(.is_regular_core("foo/TESTS"))
  expect_false(.is_regular_core("foo/MOCK"))
  expect_false(.is_regular_core("foo/MOCKS"))
  expect_false(.is_regular_core("foo/test/a/dependency"))
  expect_false(.is_regular_core("foo/tests/a/dependency"))
  expect_false(.is_regular_core("foo/mock/a/dependency"))
  expect_false(.is_regular_core("foo/mocks/a/dependency"))
  expect_false(.is_regular_core("foo/TEST/a/dependency"))
  expect_false(.is_regular_core("foo/TESTS/a/dependency"))
  expect_false(.is_regular_core("foo/MOCK/a/dependency"))
  expect_false(.is_regular_core("foo/MOCKS/a/dependency"))
  expect_true(.is_regular_core("foo"))
})

test_that(".is_defined_regular detects regular defined modules", {
  reset()
  define("foo", NULL, function() NULL)
  expect_true(.is_defined_regular("foo"))
  expect_false(.is_defined_regular("undefined/module"))
  expect_false(.is_defined_regular("modulr"))
})
