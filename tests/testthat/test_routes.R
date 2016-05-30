context("routes")

test_that(".make_path returns with a trailing slash and only one", {
  expect_equal(.make_path("/"), "/")
  expect_equal(.make_path("foo/"), "foo/")
  expect_equal(.make_path("foo/bar"), "foo/bar/")
  expect_equal(.make_path("foo/bar/"), "foo/bar/")
  expect_equal(.make_path("foo/bar//"), "foo/bar/")
})

test_that(".parse_filename returns all the required stuff", {
  expect_error(.parse_filename(NA))
  expect_error(.parse_filename(NULL))
  expect_equal(.parse_filename(""),
               list(
                 filename = "",
                 path = ".",
                 basename = "",
                 name = "",
                 extension = "",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename(" "),
               list(
                 filename = "",
                 path = ".",
                 basename = "",
                 name = "",
                 extension = "",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("foo"),
               list(
                 filename = "foo",
                 path = ".",
                 basename = "foo",
                 name = "foo",
                 extension = "",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("foo.R"),
               list(
                 filename = "foo.R",
                 path = ".",
                 basename = "foo.R",
                 name = "foo",
                 extension = "R",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("/foo"), # Exclude Linting
               list(
                 filename = "/foo", # Exclude Linting
                 path = "/",
                 basename = "foo",
                 name = "foo",
                 extension = "",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("/foo.R"), # Exclude Linting
               list(
                 filename = "/foo.R", # Exclude Linting
                 path = "/",
                 basename = "foo.R",
                 name = "foo",
                 extension = "R",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("foo/"),
               list(
                 filename = "foo/",
                 path = "foo",
                 basename = "",
                 name = "",
                 extension = "",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("foo/bar"),
               list(
                 filename = "foo/bar",
                 path = "foo",
                 basename = "bar",
                 name = "bar",
                 extension = "",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
  expect_equal(.parse_filename("foo/bar.R"),
               list(
                 filename = "foo/bar.R",
                 path = "foo",
                 basename = "bar.R",
                 name = "bar",
                 extension = "R",
                 version = package_version("", strict = FALSE),
                 symbol = NA_character_))
})

test_that(".resolve_mapping resolves module names", {
  maps_config$set(
    "some/oldmodule" = list(
      "foo/bar" = "foo/bar_V1"
    ))
  expect_equal(.resolve_mapping("foo/bar", "some/oldmodule"),
               "foo/bar_V1")
  expect_equal(.resolve_mapping("foo/bar"),
               "foo/bar")

  maps_config$set(
    "some/oldmodule" = list(
      "foo/bar" = "foo/old/bar",
      "foo/bar" = "foo/new/bar"
    ))
  expect_warning(resol <- .resolve_mapping("foo/bar", "some/oldmodule"))
  expect_equal(resol, "foo/old/bar")
  maps_config$set(
    "some/oldmodule" = list(
      "foo/bar" = "foo/new/bar",
      "foo/bar" = "foo/old/bar"
    ))
  expect_warning(resol <- .resolve_mapping("foo/bar", "some/oldmodule"))
  expect_equal(resol, "foo/new/bar")

})

test_that("find_path returns path relative to config", {
  paths_config$set(
    "bundle_1" = "lib/bundle_1",
    "bundle_2" = "lib/bundle_2")
  expect_equal(find_path("bundle_1/foo/bar", absolute = F),
               "./lib/bundle_1/foo/bar.R")
  expect_equal(find_path("bundle_2/foo/baz", absolute = F),
               "./lib/bundle_2/foo/baz.R")
})

test_that("find_path returns path relative to mappings", {
  paths_config$set(
    "3rd_party_modules" = "lib/vendor",
    "my_modules" = "lib/modules")
  maps_config$set(
    "my_modules/old/module" = list(
      "3rd_party_modules/foo/bar" = "3rd_party_modules/foo/bar_V2"
      )
    )
  expect_equal(find_path("3rd_party_modules/foo/bar",
                            "my_modules/old/module", absolute = F),
               "./lib/vendor/foo/bar_V2.R")
  expect_equal(find_path("3rd_party_modules/foo/bar", absolute = F),
               "./lib/vendor/foo/bar.R")
})

test_that("find_path returns absolute path relative to config", {
  root_config$set(".")
  paths_config$set(
    "bundle_1" = "lib/bundle_1",
    "bundle_2" = "lib/bundle_2")
  expect_equal(find_path("bundle_1/foo/bar", absolute = T),
               normalizePath("./lib/bundle_1/foo/bar.R"))
  expect_equal(find_path("bundle_2/foo/baz", absolute = T),
               normalizePath("./lib/bundle_2/foo/baz.R"))
})
