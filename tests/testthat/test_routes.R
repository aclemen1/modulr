context("routes")

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
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep), collapse = ""))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
                     collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep), collapse = ""))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar",
                       rep(.Platform$file.sep, 2)), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep), collapse = ""))
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
  expect_equal(.parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.0.0.R")),
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

# test_that(".parse_root returns path and basename of the root part", {
#   expect_equal(.parse_root(""),
#                list(
#                  path = "",
#                  basename = character(0)
#                ))
#   expect_equal(.parse_root("foo"),
#                list(
#                  path = "",
#                  basename = "foo"
#                ))
#   expect_equal(.parse_root("foo/bar"),
#                list(
#                  path = "foo",
#                  basename = "bar"
#                ))
#   expect_equal(.parse_root("foo/bar/foobar"),
#                list(
#                  path = "foo/bar",
#                  basename = "foobar"
#                ))
# })

test_that(".filter_versions works properly and keeps names", {
  expect_equal(
    .filter_versions(numeric_version(c(""), strict = FALSE),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c(""), strict = FALSE))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0.0", ""), strict = FALSE),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0.0"), strict = FALSE))
  )
  expect_equal(
    .filter_versions(numeric_version(c("1.0", ""), strict = FALSE),
                     numeric_version(c("1.0.0")), NA),
    as.list(numeric_version(c("1.0", ""), strict = FALSE))
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

# test_that("find_path returns path relative to config", {
#   paths_config$set(
#     "bundle_1" = "lib/bundle_1",
#     "bundle_2" = "lib/bundle_2")
#   expect_equal(find_path("bundle_1/foo/bar", absolute = F),
#                "./lib/bundle_1/foo/bar.R")
#   expect_equal(find_path("bundle_2/foo/baz", absolute = F),
#                "./lib/bundle_2/foo/baz.R")
# })
#
# test_that("find_path returns path relative to mappings", {
#   paths_config$set(
#     "3rd_party_modules" = "lib/vendor",
#     "my_modules" = "lib/modules")
#   maps_config$set(
#     "my_modules/old/module" = list(
#       "3rd_party_modules/foo/bar" = "3rd_party_modules/foo/bar_V2"
#       )
#     )
#   expect_equal(find_path("3rd_party_modules/foo/bar",
#                             "my_modules/old/module", absolute = F),
#                "./lib/vendor/foo/bar_V2.R")
#   expect_equal(find_path("3rd_party_modules/foo/bar", absolute = F),
#                "./lib/vendor/foo/bar.R")
# })
#
# test_that("find_path returns absolute path relative to config", {
#   root_config$set(".")
#   paths_config$set(
#     "bundle_1" = "lib/bundle_1",
#     "bundle_2" = "lib/bundle_2")
#   expect_equal(find_path("bundle_1/foo/bar", absolute = T),
#                normalizePath("./lib/bundle_1/foo/bar.R"))
#   expect_equal(find_path("bundle_2/foo/baz", absolute = T),
#                normalizePath("./lib/bundle_2/foo/baz.R"))
# })


# context("routes")
#
# test_that(".make_path returns with a trailing slash and only one", {
#   expect_equal(.make_path("/"), "/")
#   expect_equal(.make_path("foo/"), "foo/")
#   expect_equal(.make_path("foo/bar"), "foo/bar/")
#   expect_equal(.make_path("foo/bar/"), "foo/bar/")
#   expect_equal(.make_path("foo/bar//"), "foo/bar/")
# })
#
# test_that(".parse_filename returns all the required stuff", {
#   expect_error(.parse_filename(NA))
#   expect_error(.parse_filename(NULL))
#   expect_equal(.parse_filename(""),
#                list(
#                  filename = "",
#                  path = ".",
#                  basename = "",
#                  name = "",
#                  extension = "",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename(" "),
#                list(
#                  filename = "",
#                  path = ".",
#                  basename = "",
#                  name = "",
#                  extension = "",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("foo"),
#                list(
#                  filename = "foo",
#                  path = ".",
#                  basename = "foo",
#                  name = "foo",
#                  extension = "",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("foo.R"),
#                list(
#                  filename = "foo.R",
#                  path = ".",
#                  basename = "foo.R",
#                  name = "foo",
#                  extension = "R",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("/foo"), # Exclude Linting
#                list(
#                  filename = "/foo", # Exclude Linting
#                  path = "/",
#                  basename = "foo",
#                  name = "foo",
#                  extension = "",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("/foo.R"), # Exclude Linting
#                list(
#                  filename = "/foo.R", # Exclude Linting
#                  path = "/",
#                  basename = "foo.R",
#                  name = "foo",
#                  extension = "R",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("foo/"),
#                list(
#                  filename = "foo/",
#                  path = "foo",
#                  basename = "",
#                  name = "",
#                  extension = "",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("foo/bar"),
#                list(
#                  filename = "foo/bar",
#                  path = "foo",
#                  basename = "bar",
#                  name = "bar",
#                  extension = "",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
#   expect_equal(.parse_filename("foo/bar.R"),
#                list(
#                  filename = "foo/bar.R",
#                  path = "foo",
#                  basename = "bar.R",
#                  name = "bar",
#                  extension = "R",
#                  version = package_version("", strict = FALSE),
#                  symbol = NA_character_))
# })
#
# test_that(".resolve_mapping resolves module names", {
#   maps_config$set(
#     "some/oldmodule" = list(
#       "foo/bar" = "foo/bar_V1"
#     ))
#   expect_equal(.resolve_mapping("foo/bar", "some/oldmodule"),
#                "foo/bar_V1")
#   expect_equal(.resolve_mapping("foo/bar"),
#                "foo/bar")
#
#   maps_config$set(
#     "some/oldmodule" = list(
#       "foo/bar" = "foo/old/bar",
#       "foo/bar" = "foo/new/bar"
#     ))
#   expect_warning(resol <- .resolve_mapping("foo/bar", "some/oldmodule"))
#   expect_equal(resol, "foo/old/bar")
#   maps_config$set(
#     "some/oldmodule" = list(
#       "foo/bar" = "foo/new/bar",
#       "foo/bar" = "foo/old/bar"
#     ))
#   expect_warning(resol <- .resolve_mapping("foo/bar", "some/oldmodule"))
#   expect_equal(resol, "foo/new/bar")
#
# })
#
# test_that("find_path returns path relative to config", {
#   paths_config$set(
#     "bundle_1" = "lib/bundle_1",
#     "bundle_2" = "lib/bundle_2")
#   expect_equal(find_path("bundle_1/foo/bar", absolute = F),
#                "./lib/bundle_1/foo/bar.R")
#   expect_equal(find_path("bundle_2/foo/baz", absolute = F),
#                "./lib/bundle_2/foo/baz.R")
# })
#
# test_that("find_path returns path relative to mappings", {
#   paths_config$set(
#     "3rd_party_modules" = "lib/vendor",
#     "my_modules" = "lib/modules")
#   maps_config$set(
#     "my_modules/old/module" = list(
#       "3rd_party_modules/foo/bar" = "3rd_party_modules/foo/bar_V2"
#       )
#     )
#   expect_equal(find_path("3rd_party_modules/foo/bar",
#                             "my_modules/old/module", absolute = F),
#                "./lib/vendor/foo/bar_V2.R")
#   expect_equal(find_path("3rd_party_modules/foo/bar", absolute = F),
#                "./lib/vendor/foo/bar.R")
# })
#
# test_that("find_path returns absolute path relative to config", {
#   root_config$set(".")
#   paths_config$set(
#     "bundle_1" = "lib/bundle_1",
#     "bundle_2" = "lib/bundle_2")
#   expect_equal(find_path("bundle_1/foo/bar", absolute = T),
#                normalizePath("./lib/bundle_1/foo/bar.R"))
#   expect_equal(find_path("bundle_2/foo/baz", absolute = T),
#                normalizePath("./lib/bundle_2/foo/baz.R"))
# })
