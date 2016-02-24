library(testthat)
library(modulr)

if (packageVersion("testthat") < package_version("0.11.0.9000")) {
  expect_lt <- testthat::expect_less_than
  expect_gt <- testthat::expect_more_than
}

test_check("modulr")
