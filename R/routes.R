.parse_version <- function(string) {
  matches <- regmatches(string, regexec(.version_regex, string))[[1]]
  list(
    symbol = as.character(ifelse(matches[2] == "", NA, matches[2])),
    version = numeric_version(
      matches[3],
      strict = FALSE)
  )
}

# TODO test that!
# Parse a module name.
.parse_name <- function(name) {
  assert_that(assertthat::is.string(name), .is_conform(name))
  matches <- regmatches(name, regexec(.conform_regex, name))[[1]]
  matches <-
    setNames(as.list(matches),
             c("name", "namespace", "symbol", "version", "suffix"))
  matches[["suffix"]] <- sub("^/", "", matches[["suffix"]])
  matches[c("symbol", "version")] <-
    .parse_version(paste0("#", matches[["symbol"]], matches[["version"]]))[
      c("symbol", "version")]
  components <- strsplit(matches[["namespace"]], "/", fixed = TRUE)[[1]]
  matches[["initials"]] <- paste(head(components, -1L), collapse = "/")
  matches[["final"]] <- tail(components, 1L)
  matches[c(
    "name", "namespace", "initials", "final", "symbol", "version", "suffix")]
}

# Mapping allows to resolve a new module name depending on a scope.
# This is very useful when two different versions of a module are required
# from different modules. For instance, "some/old_module" and "some/module"
# both require "foo/bar", but we would like "foo/bar" to be resolved as
# "foo/bar_V1" for "some/old_module" only. See tests for more examples.
.resolve_mapping <- function(name, scope_name = NULL) {

  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name))

  mappings <- maps_config$get(scope_name)

  if (is.null(mappings)) return(
    list(
      name = name,
      scope_name = scope_name,
      resolved = name))

  candidates <- Map(
    function(map) {

      reg <- regexpr(sprintf("^(?:%s)$", map), name)

      list(
        map = map,
        start = as.integer(reg),
        end = as.integer(reg) + attr(reg, "match.length") - 1)

    },
    names(mappings))

  candidates <- Filter(function(candidate) {
    candidate[["start"]] == 1
  },
  candidates)

  if (length(candidates) == 0) return(name)

  maximum_length <- max(unlist(Map(
    function(candidate) {
      candidate[["end"]]
    },
    candidates)))

  candidates <- Filter(
    function(candidate) {
      candidate[["end"]] == maximum_length
    },
    candidates)

  if (length(candidates) > 1) warning(
    "More than one matching mapping. ",
    "Considering only the first occurence.",
    call. = FALSE, immediate. = TRUE)

  matching_map <- candidates[[1]][["map"]]

  list(
    name = name,
    scope_name = scope_name,
    resolved = sub(matching_map, mappings[[matching_map]], name)
  )

}

.remove_duplicate_filesep <- function(path) {
  gsub(paste(c(rep(.Platform$file.sep, 2), "+"), collapse = ""),
       .Platform$file.sep, path)
}

# For a filepath, we need to know the following attributes:
# path, basename, name (as a module), extension and version number and symbol.
.parse_filepath <- function(filename,
                            extensions = c(".R", ".r",
                                           ".Rmd", ".rmd",
                                           ".Rnw", ".rnw")) {

  assert_that(assertthat::is.string(filename))

  filename <-
    .remove_duplicate_filesep(gsub("^\\s*|\\s*$", "", file.path(filename)))

  is_path <-
    substr(filename, nchar(filename), nchar(filename)) == .Platform$file.sep
  filename_shifted <-
    ifelse(filename == "", ".",
           ifelse(is_path, paste0(filename, "."), filename))

  path <- dirname(filename_shifted) # "foo/" has to be seen as a path

  base_name <- basename(
    ifelse(is_path, substr(filename, nchar(filename), nchar(filename) - 1),
           filename))

  extension <- tools::file_ext(filename)
  if (extension %in% gsub("^\\.", "", extensions)) {
    name <- basename(tools::file_path_sans_ext(base_name))
  } else {
    name <- base_name
    extension <- ""
  }

  list(
    filename = filename,
    path = path,
    basename = base_name,
    name = name,
    extension = extension,
    version = .parse_version(name)[["version"]]
  )

}

# Paths should end with exactly one trailing slash
.make_path <- function(path) {
  assert_that(assertthat::is.string(path))
  .remove_duplicate_filesep(paste0(path, .Platform$file.sep))
}

# TODO test that!
.remove_trailing_filesep <- function(path) {
  assert_that(assertthat::is.string(path))
  sub(paste0(.Platform$file.sep, "+$"), "", path)
}

# TODO test that!
.name_to_path <- function(name) {
  assert_that(.is_conform(name))
  paste(strsplit(name, "/", fixed = TRUE)[[1]], collapse = .Platform$file.sep)
}

.path_to_name <- function(path) {
  name <-
    paste(strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]], collapse = "/")
  assert_that(.is_conform(name))
  name
}

# For instance, paths_config$set("vendor" = "third_parties/vendor") will map the
# vendor/great_module namespace to the third_parties/vendor/great_module
# namespace.
.resolve_namespace <- function(name, scope_name = NULL) {

  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name))

  mapping <- .resolve_mapping(name, scope_name)

  namespace <- .parse_name(mapping[["resolved"]])[["namespace"]]

  candidates <- Map(
    function(namespace_) {

      reg <- regexpr(paste0(namespace_, "/"), namespace, fixed = TRUE)

      list(
        namespace = namespace_,
        start = as.integer(reg),
        end = as.integer(reg) + attr(reg, "match.length") - 1)

    },
    names(paths_config$get_all()))

  candidates <- Filter(
    function(candidate) {
      candidate[["start"]] == 1
    },
    candidates)

  if (length(candidates) == 0) {

    candidate <- mapping[["resolved"]]

  } else {

    maximum_length <- max(unlist(Map(
      function(candidate) {
        candidate[["end"]]
      },
      candidates)))

    candidates <- Filter(
      function(candidate) {
        candidate[["end"]] == maximum_length
      },
      candidates)

    if (length(candidates) > 1) warning(
      "More than one matching namespace. ",
      "Considering only the first occurence.")

    matching_namespace <- candidates[[1]][["namespace"]]

    candidate <- sub(
      matching_namespace,
      paths_config$get_all()[[matching_namespace]],
      mapping[["resolved"]])

  }

  list(
    name = name,
    scope_name = scope_name,
    resolved = candidate,
    mapping = mapping
  )

}

.filter_versions <- function(versions, version, symbol) {
  filter_ <- function(versions, version, symbol) {
    candidates <- Filter(function(v) {
      !isTRUE(v < version)
    },
    versions)
    keep <- FALSE
    if (length(candidates) > 0) {
      keep <- if (is.na(symbol)) {
        Vectorize(identical)(candidates, version) | is.na(candidates)
      } else {
        if (symbol == ">=") {
          TRUE
        } else if (symbol == "^") {
          !Vectorize(isTRUE)(do.call(c, lapply(candidates, `[`, 1, 1)) >
                               version[1, 1])
        } else if (symbol == "~") {
          !Vectorize(isTRUE)(do.call(c, lapply(candidates, `[`, 1, c(1, 2))) >
                               version[1, c(1, 2)])
        }
      }
    }
    candidates[keep]
  }

  if (!is.na(version)) {
    l_max <- length(do.call(c, version))

    filtered <- c()
    for (l in seq_len(l_max)) {
      filtered <- c(
        filter_(versions, version[1, c(1:l)], ifelse(l == l_max, symbol, NA)),
        filtered) # has to be in second position in the vector!
    }
    filtered <- Filter(length, filtered)

    if(is.na(symbol) && isTRUE(length(filtered) > 0L)) {
      # looks strange, the following is true:
      # numeric_version("1.0") == numeric_version("1.0.0")
      # therefore we use 'identical'
      strict <- Vectorize(identical)(filtered, version)
      if(any(strict))
        filtered <- filtered[strict]
    }

  } else {
    filtered <- do.call(c, as.list(versions))
  }

  filtered_str <- as.character(filtered)
  ordering <- order(filtered_str, na.last = TRUE)
  versions <-
    filtered[ordering][!duplicated(filtered_str)[ordering]]

  # looks strange, see equality above:
  # unique(numeric_version(c("1.0", "1.0.0")))
  as.list(versions)
}

# TODO test that!
.resolve_candidates <- function(name, scope_name = NULL, absolute = TRUE,
                                extensions = c(".R", ".r",
                                               ".Rmd", ".rmd",
                                               ".Rnw", ".rnw")) {
  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name),
    is.character(extensions))

  resolved_namespace <- .resolve_namespace(name, scope_name)
  resolved_name <- resolved_namespace[["resolved"]]
  parsed_name <- .parse_name(resolved_name)

  pattern <- sprintf("(?:%s)", paste(paste0(
    "^", parsed_name[["final"]],
    paste0(.version_regex, "?"),
    glob2rx(sprintf("*%s", extensions), trim.head = TRUE)),
    collapse = "|"))

  roots <- unique(c(root_config$get_all()[[1L]], "."))

  files <- c()
  for (root in roots) {
    path <- .remove_trailing_filesep(
      file.path(root, .name_to_path(parsed_name[["initials"]])))
    files_ <-
      ifelse(absolute, normalizePath,
             Vectorize(.remove_duplicate_filesep, "path"))(
               list.files(path = path, pattern = pattern, full.names = TRUE))
    files <- c(files, files_)
  }

  versions <- do.call(c, Map(function(file) {
    .parse_version(basename(file))[["version"]]
  },
  files))

  versions <- .filter_versions(
    versions = versions,
    version = parsed_name[["version"]], symbol = parsed_name[["symbol"]])

  resolution <- lapply(names(versions), function(filepath) {
    list(
      filepath = filepath,
      version = versions[[filepath]]
    )
  })

#   candidates <- list()
#   candidates[["versions"]] <- .filter_versions(
#     versions = versions,
#     version = parsed_name[["version"]], symbol = parsed_name[["symbol"]])
#
#   candidates[["resolved_name"]] <- parsed_name

  list(
    name = name,
    scope_name = scope_name,
    resolved = resolution,
    namespace = resolved_namespace
  )

}

# TODO test that!
.extract_name <- function(filepath) {
  if(file.exists(filepath)) {
    con <- file(filepath, "r")
    on.exit(close(con))
    while (TRUE) {
      line <- readLines(con = con, n = 1L, warn = FALSE)
      if (length(line) == 0) break
      matches <- regmatches(
        line,
        regexec(
          sprintf(
            "(?:^\\s*|(?:define\\())[\"']([^\"']*?(?:%s)[^\"']*?)[\"']",
            .version_regex,
            perl = TRUE),
          line)
      )[[1]]
      if (length(matches) > 0) {
        return(matches[2])
      }
    }
  }
}

# TODO test that!
.resolve_name <- function(name, scope_name = NULL, absolute = TRUE, all = TRUE,
                          extensions = c(".R", ".r",
                                         ".Rmd", ".rmd",
                                         ".Rnw", ".rnw")) {

  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name),
    is.character(extensions))

  candidates <-
    .resolve_candidates(
      name = name, scope_name = scope_name,
      absolute = absolute, extensions = extensions)

  parsed_version <-
    .parse_version(candidates[[c("namespace", "resolved")]])
  parsed_name <-
    .parse_name(candidates[[c("namespace", "resolved")]])

  versions <- c()
  for (filepath in unlist(
    lapply(candidates[["resolved"]], `[[`, "filepath"))) {
    extracted_name <- .extract_name(filepath)
    if (!is.null(extracted_name)) {
      parsed_name <- .parse_name(extracted_name)
    }
    versions <- c(setNames(parsed_name[["version"]], filepath), versions)
  }

  versions <- as.list(versions)
  versions <-
    .filter_versions(
      versions, parsed_version[["version"]], parsed_version[["symbol"]])

  resolution <- list()
  if (length(versions) > 0L) {
    if (!all) versions <- tail(versions, 1L)
    resolved_versions <- do.call(c, versions)
    resolved_filepaths <- names(versions)
    versions_str <- as.character(resolved_versions)
    resolved_names <-
      ifelse(is.na(versions_str), parsed_name[["namespace"]],
             paste(parsed_name[["namespace"]], versions_str,
                   sep = "#"))
    resolution <- list(
      name = resolved_names,
      filepath = resolved_filepaths,
      version = resolved_versions
    )
  }

  list(
    name = name,
    scope_name = scope_name,
    resolved = resolution,
    candidates = candidates
  )

# else {
#       list(
#         name = name,
#         scope_name = scope_name,
#         resolved = list(
#           name = resolved_names
#           filepath = resolved_filepath,
#           version = resolved_versions,
#         ),
#         candidates = candidates
#         versions = versions,
#         resolved_names = setNames(parsed_name[["namespace"]], name)
#       )
#     }

}

#
#
# .parse_root <- function(name) {
#   parts <- strsplit(name, "/", fixed = TRUE)[[1]]
#   list(
#     path = paste(head(parts, -1L), collapse = .Platform$file.sep),
#     basename = tail(parts, 1L)
#   )
# }
#
# # TODO test that!
# .find_candidates <- function(name, scope_name = NULL, absolute = TRUE,
#                              extensions = c(".R", ".r",
#                                             ".Rmd", ".rmd",
#                                             ".Rnw", ".rnw"),
#                              all = FALSE) {
#
#   assert_that(.is_conform(name))
#
#   resolved <-
#     .resolve_namespace(name = name, scope_name = scope_name, extensions = extensions)
#
#   parsed_name <- .parse_name(resolved$name)
#   parsed_root_name <- ..parse_root(parsed_name[["namespace"]])
#   parsed_root_path <- ..parse_root(.parse_name(resolved$path)[["namespace"]])
#
#   pattern <- sprintf("(?:%s)", paste(paste0(
#     "^", parsed_root_name$basename,
#     paste0(.version_regex, "?"),
#     glob2rx(sprintf("*%s", extensions), trim.head = TRUE)),
#     collapse = "|"))
#
#   files <- c()
#   roots <- unique(c(root_config$get_all()[[1L]], "."))
#   for (root in roots) {
#     path <- file.path(root, parsed_root_path$path)
#     files_ <-
#       ifelse(absolute, normalizePath,
#              Vectorize(.remove_duplicate_filesep, "path"))(
#                list.files(path = path, pattern = pattern, full.names = TRUE))
#     files <- c(files, files_)
#   }
#
#   candidates <- Filter(
#     function(file) {
#       .acceptable_version(parsed_name$version, parsed_name$symbol,
#                           file$version, file$symbol)
#     },
#     Map(function(file) {
#       parsed <- .parse_filepath(file)
#       parsed[["namespace"]] <- parsed_name[["namespace"]]
#       if (!is.na(parsed$version)) {
#         resolved_name <- paste(parsed_name[["namespace"]], parsed$version, sep = "#")
#       } else if (isTRUE(parsed$symbol == "*")) {
#         resolved_name <- paste(parsed_name[["namespace"]], "latest", sep = "#")
#       } else {
#         resolved_name <- parsed_name[["namespace"]]
#       }
#       parsed$resolved_name <- resolved_name
#       parsed
#     },
#     files)
#   )
#
#   versions <- do.call(c, lapply(
#     candidates, function(candidate) candidate$version))
#   symbols <- do.call(c, lapply(
#     candidates, function(candidate) candidate$symbol))
#   latest <- which(symbols == "*")
#   if (length(latest) > 0L) {
#     candidates <- candidates[latest]
#   } else if (length(versions) > 0L) {
#     version_max <- max(versions, na.rm = TRUE)
#     if (length(version_max) > 0L) {
#       candidates <- Filter(function(candidate) {
#         candidate$version == version_max
#       },
#       candidates)
#     }
#   }
#
#   if (!all && length(candidates) > 0L) candidates <- tail(candidates, 1L)
#
#   unname(candidates)
#
# }

# # TODO test that!
# resolve_name <- function(name, scope_name = NULL) {
#   candidates <- .find_candidates(name = name, scope_name = scope_name)
#   if (length(candidates) > 0L) {
#     candidates[[1]]$resolved_name
#   } else {
#     .resolve_mapping(name = name, scope_name)$resolved
#   }
# }

# # TODO test that!
# .acceptable_version <- function(base_version, base_symbol, version, symbol) {
#   return(TRUE)
#   if (is.na(base_version)) {
#     # no base version provided
#     if (base_symbol == "*") {
#       # latest version accepted, no version accepted
#       !is.na(version) || isTRUE(symbol == "*") ||
#         (is.na(version) && !isTRUE(symbol == "*"))
#     } else {
#       # only no version accepted
#       is.na(version) && !isTRUE(symbol == "*")
#     }
#   } else {
#     # base version provided
#     if (base_symbol == "^") {
#       # e.g. 1.x.x
#       version$major == base_version$major &&
#         (version$minor > base_version$minor ||
#            (version$minor == base_version$minor &
#               version$patchlevel >= base_version$patchlevel))
#     } else if (base_symbol == "~") {
#       # e.g. 1.2.x
#       version$major == base_version$major &&
#         version$minor == base_version$minor &&
#         version$patchlevel >= base_version$patchlevel
#     } else {
#       # exact match
#       base_version == version
#     }
#   }
# }

#' Find the Path of a Module.
#'
#' Find the path of a module, in the context of a module scope, if any. The
#' returned path can be absolute or relative to a root directory.
#'
#' @inheritParams define
#' @param scope_name A module name to use as scope (see \code{\link{define}},
#'   \code{\link{maps_config}}, and examples).
#' @param absolute A flag. Should the returned path be absolute? (see
#'   \code{\link{define}}, \code{\link{root_config}}, and examples)
#' @param extensions A character vector. File extensions to consider.
#'
#' @return A string containing the path of the module.
#'
#' @seealso \code{\link{define}}, \code{\link{maps_config}},
#'   \code{\link{reset}}, and \code{\link{root_config}},
#'
#' @examples
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() "Hello World!")', file = tmp_file)
#' root_config$set(tmp_dir)
#' set_verbosity(1L)
#' find_path("foo")
#' unlink(tmp_dir, recursive = TRUE)
#'
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(file.path(tmp_dir, 'foo'), recursive = TRUE)
#' dir.create(file.path(tmp_dir, 'vendor'), recursive = TRUE)
#' cat(paste0('define("bar", list(great_module = "vendor/great_module"), ',
#'            'function() great_module)'),
#'     file = file.path(tmp_dir, "foo", "bar.R"))
#' cat('define("great_module", NULL, function() "Great Module")',
#'     file = file.path(tmp_dir, "vendor", "great_module.R"))
#' cat('define("great_module", NULL, function() "Old Great Module")',
#'     file = file.path(tmp_dir, "vendor", "old_great_module.R"))
#' root_config$set(tmp_dir)
#' set_verbosity(1L)
#' find_path("vendor/great_module")
#' maps_config$set("foo/bar" = list("vendor/great_module" =
#'                                  "vendor/old_great_module"))
#' find_path("vendor/great_module", "foo/bar")
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @export
find_path <- function(name, scope_name = NULL, absolute = TRUE,
                      extensions = c(".R", ".r",
                                     ".Rmd", ".rmd",
                                     ".Rnw", ".rnw")) {
  resolved_name <- .resolve_name(
    name = name, scope_name = scope_name,
    absolute = absolute, all = FALSE,
    extensions = extensions)[["resolved"]]

  if (!is.null(resolved_name)) {
    return(setNames(resolved_name[["filepath"]], resolved_name[["name"]]))
  }

}
