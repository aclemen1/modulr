context("routes")

test_that(".path_to_name and .name_to_path are inverses", {
  name <- "foo/bar/foobar#^1.0.0/test"
  expect_equal(
    .path_to_name(.name_to_path(name)),
    name
  )
  path <- "foo/bar/foobar#^1.0.0/test"
  expect_equal(
    .name_to_path(.path_to_name(path)),
    path
  )
  good_path <- "foo/bar/foobar/"
  expect_equal(.path_to_name(good_path), .remove_trailing_filesep(good_path))
  good_path <- "foo/bar///foobar/"
  expect_equal(.path_to_name(good_path),
               .remove_trailing_filesep(.remove_duplicate_filesep(good_path)))
  bad_path <- "foo/bar/foobar/."
  expect_error(.path_to_name(bad_path))
  bad_path <- "./foo/bar/foobar"
  expect_error(.path_to_name(bad_path))
})

test_that(".parse_name returns a list of informations", {
  n <- .parse_name("foo/bar/foobar#^1.0.0/test")
  expect_true(is.list(n))
  expect_named(n, c(
    "name",
    "namespace",
    "initials",
    "final",
    "symbol",
    "version",
    "suffix"), ignore.order = TRUE)
  expect_equal(n[["name"]], "foo/bar/foobar#^1.0.0/test")
  expect_equal(n[["namespace"]], "foo/bar/foobar")
  expect_equal(n[["initials"]], "foo/bar")
  expect_equal(n[["final"]], "foobar")
  expect_equal(n[["symbol"]], "^")
  expect_equal(n[["version"]], numeric_version("1.0.0"))
  expect_equal(n[["suffix"]], "test")
})

test_that(".parse_version returns a list of version and symbol", {
  v <- .parse_version("foobar#^1.0.0")
  expect_true(is.list(v))
  expect_named(v, c("version", "symbol"), ignore.order = TRUE)
  expect_equal(v[["version"]], numeric_version("1.0.0"))
  expect_equal(v[["symbol"]], "^")
  expect_equal(
    .parse_version("#1"),
    list(
      version = numeric_version("1"),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#1.0"),
    list(
      version = numeric_version("1.0"),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#~1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = "~")[
        names(v)]
  )
  expect_equal(
    .parse_version("#^1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = "^")[
        names(v)]
  )
  expect_equal(
    .parse_version("#>=1.0.0"),
    list(
      version = numeric_version("1.0.0"),
      symbol = ">=")[
        names(v)]
  )
  expect_equal(
    .parse_version(""),
    list(
      version = numeric_version("", strict = FALSE),
      symbol = NA_character_)[
        names(v)]
  )
  expect_equal(
    .parse_version("#"),
    list(
      version = numeric_version("", strict = FALSE),
      symbol = NA_character_)[
        names(v)]
  )
})

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
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
          collapse = ""))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
                     collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
          collapse = ""))
  expect_equal(
    .make_path(paste(c("foo", .Platform$file.sep, "bar",
                       rep(.Platform$file.sep, 2)), collapse = "")),
    paste(c("foo", .Platform$file.sep, "bar", .Platform$file.sep),
          collapse = ""))
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
  expect_equal(
    .parse_filepath(paste0("foo", .Platform$file.sep, "bar#1.0.0.R")),
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
