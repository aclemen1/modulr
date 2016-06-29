context("assertions")

test_that(".is_defined detects defined modules", {
  reset()
  expect_true(.is_defined(MODULR_NAME))
  expect_false(.is_defined("undefined/module"))
  })

test_that(".is_undefined detects undefined modules", {
  reset()
  expect_false(.is_undefined(MODULR_NAME))
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
  expect_true(.is_reserved(RESERVED_NAMES[1]))
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
  expect_false(.is_regular_core("foo/example"))
  expect_false(.is_regular_core("foo/examples"))
  expect_false(.is_regular_core("foo/TEST"))
  expect_false(.is_regular_core("foo/TESTS"))
  expect_false(.is_regular_core("foo/MOCK"))
  expect_false(.is_regular_core("foo/MOCKS"))
  expect_false(.is_regular_core("foo/EXAMPLE"))
  expect_false(.is_regular_core("foo/EXAMPLES"))
  expect_false(.is_regular_core("foo/test/a/dependency"))
  expect_false(.is_regular_core("foo/tests/a/dependency"))
  expect_false(.is_regular_core("foo/mock/a/dependency"))
  expect_false(.is_regular_core("foo/mocks/a/dependency"))
  expect_false(.is_regular_core("foo/example/a/dependency"))
  expect_false(.is_regular_core("foo/examples/a/dependency"))
  expect_false(.is_regular_core("foo/TEST/a/dependency"))
  expect_false(.is_regular_core("foo/TESTS/a/dependency"))
  expect_false(.is_regular_core("foo/MOCK/a/dependency"))
  expect_false(.is_regular_core("foo/MOCKS/a/dependency"))
  expect_false(.is_regular_core("foo/EXAMPLE/a/dependency"))
  expect_false(.is_regular_core("foo/EXAMPLES/a/dependency"))
  expect_true(.is_regular_core("foo"))
})

test_that(".is_defined_regular detects regular defined modules", {
  reset()
  define("foo", NULL, function() NULL)
  expect_true(.is_defined_regular("foo"))
  expect_false(.is_defined_regular("undefined/module"))
  expect_false(.is_defined_regular("modulr"))
})

test_that(".is_braced_expression detects braced expressions", {
  expect_true(.is_braced_expression(quote({
    })))
  expect_false(.is_braced_expression({
    }))
})

test_that(".is_constant detects constants", {
  expect_true(.is_constant("foo"))
  expect_true(.is_constant(1L))
  expect_true(.is_constant(1.0))
  expect_true(.is_constant(TRUE))
  expect_true(.is_constant(NA))
  expect_true(.is_constant(NULL))
  expect_false(.is_constant(c("foo", "bar")))
  expect_false(.is_constant(list("foo")))
  expect_false(.is_constant(list(foo = "foo")))
  expect_false(.is_constant(list("foo", "bar")))
  expect_false(.is_constant(function() NULL))
  expect_false(.is_constant(expression(x + 1)))
  expect_false(.is_constant(quote(exp)))
})
