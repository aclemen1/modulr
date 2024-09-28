# Parse version from a string.
.parse_version <- function(string) {
  assert_that(assertthat::is.string(string))
  matches <-
    regmatches(string, regexec(.version_hash_string_regex, string))[[1L]]
  assert_that(.is_version_string(matches[3L]))
  list(
    symbol = as.character(ifelse(matches[2L] == "", NA, matches[2L])),
    version = numeric_version(
      matches[3L],
      strict = FALSE)
  )
}

# Parse a module name.
.parse_name <- function(name) {
  assert_that(.is_conform(name))
  matches <- regmatches(name, regexec(.conform_regex, name))[[1L]]
  matches <-
    stats::setNames(as.list(matches),
                    c("name", "namespace", "symbol", "version", "suffix"))
  matches[["suffix"]] <- sub("^/", "", matches[["suffix"]])
  matches[c("symbol", "version")] <-
    .parse_version(paste0("#", matches[["symbol"]], matches[["version"]]))[
      c("symbol", "version")]
  if (nchar(matches[["suffix"]]) > 0L) {
    matches[["namespace"]] <- matches[["name"]]
  }
  components <- strsplit(matches[["namespace"]], "/", fixed = TRUE)[[1L]]
  matches[["initials"]] <- paste(utils::head(components, -1L), collapse = "/")
  matches[["final"]] <- utils::tail(components, 1L)
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
        end = as.integer(reg) + attr(reg, "match.length") - 1L)

    },
    names(mappings))

  candidates <- Filter(function(candidate) {
    candidate[["start"]] == 1L
  },
  candidates)

  if (length(candidates) == 0L) return(list(
    name = name,
    scope_name = scope_name,
    resolved = name))

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

  if (length(candidates) > 1L) warning(
    "More than one matching mapping. ",
    "Considering only the first occurence.",
    call. = FALSE, immediate. = TRUE)

  matching_map <- candidates[[1L]][["map"]]

  list(
    name = name,
    scope_name = scope_name,
    resolved = sub(matching_map, mappings[[matching_map]], name)
  )

}

.remove_duplicate_filesep <- function(path) {
  gsub(paste(c(rep(.Platform$file.sep, 2L), "+"), collapse = ""),
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
    ifelse(is_path, substr(filename, nchar(filename), nchar(filename) - 1L),
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

# Remove trailing file separators in a path.
.remove_trailing_filesep <- function(path) {
  assert_that(assertthat::is.string(path))
  sub(paste0(.Platform$file.sep, "+$"), "", path)
}

# Transform a module name to a path (with file separators replacing '/''s).
.name_to_path <- function(name) {
  assert_that(.is_conform(name))
  paste(strsplit(name, "/", fixed = TRUE)[[1L]], collapse = .Platform$file.sep)
}

# Transform a module name to a path (with file separators replacing '/''s).
.name_to_first_path <- function(name) {
  assert_that(.is_conform(name))
  strsplit(name, "/", fixed = TRUE)[[1L]][1L]
}

# Transform a path into a module name (with '/'s replacing file separators).
.path_to_name <- function(path) {
  name <-
    paste(strsplit(.remove_trailing_filesep(.remove_duplicate_filesep(path)),
                   .Platform$file.sep, fixed = TRUE)[[1L]], collapse = "/")
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
        end = as.integer(reg) + attr(reg, "match.length") - 1L)

    },
    names(paths_config$get_all()))

  candidates <- Filter(
    function(candidate) {
      candidate[["start"]] == 1L
    },
    candidates)

  if (length(candidates) == 0L) {

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

    if (length(candidates) > 1L) warning(
      "More than one matching namespace. ",
      "Considering only the first occurence.")

    matching_namespace <- candidates[[1L]][["namespace"]]

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

# Filter versions relative to a base version and a version symbol.
.filter_versions <- function(versions, version, symbol, include_NAs = FALSE) {

  assert_that(
    is.list(versions),
    .is_version(version),
    .is_version_symbol(symbol))

  Map(assert_that, Map(.is_version, versions))

  filter_ <- function(versions, version, symbol) {
    candidates <- Filter(function(v) {
      if (include_NAs) {
        !isTRUE(unname(v) < version)
      } else {
        isTRUE(unname(v) >= version)
      }
    },
    versions)
    keep <- FALSE
    if (length(candidates) > 0L) {
      keep <- if (is.na(symbol)) {
        unlist(Vectorize(identical)(
          lapply(candidates, as.character),
          as.character(version))) | unlist(lapply(candidates, is.na))
      } else {
        if (symbol == ">=") {
          TRUE
        } else if (symbol == "^") {
          !Vectorize(isTRUE)(do.call(c, lapply(candidates, `[`, 1L, 1L)) >
                               version[1L, 1L])
        } else if (symbol == "~") {
          !Vectorize(isTRUE)(
            do.call(c, lapply(candidates, `[`, 1L, c(1L, 2L))) >
              version[1L, c(1L, 2L)])
        }
      }
    }
    candidates[keep]
  }

  if (!is.na(version)) {
    l_max <- length(do.call(c, version))

    filtered <- list()
    for (l in seq_len(l_max)) {
      filtered <- c(
        filter_(versions, version[1L, c(1L:l)], ifelse(l == l_max, symbol, NA)),
        filtered) # has to be in second position in the vector!
    }
    filtered <- Filter(length, filtered)

    if (is.na(symbol) && isTRUE(length(filtered) > 0L)) {
      # looks strange, the following is true:
      # numeric_version("1.0") == numeric_version("1.0.0") # Exclude Linting
      # therefore we use 'identical' (on the strings to avoid comparing
      # attributes which can legitimately be different)
      strict <- Vectorize(identical)(lapply(filtered, as.character),
                                     as.character(version))
      if (any(strict))
        filtered <- filtered[strict]
    }

  } else {
    filtered <- versions
  }

  # looks strange, see equality above:
  # unique(numeric_version(c("1.0", "1.0.0"))) # Exclude Linting
  filtered_str <- do.call(c, lapply(filtered, as.character))
  if (!is.null(filtered_str)) {
    filtered_attr <- lapply(filtered, attr, "storage")
    filtered_criterion <- paste(filtered_str, filtered_attr, sep = "$")
    ordering <- order(filtered_str, na.last = TRUE)
    filtered <-
      filtered[ordering][!duplicated(filtered_criterion)[ordering]]
  }

  as.list(filtered)
}

# Transform a named list of versions with 'storage' attributes into a structured
# list.
.unflatten_versions <- function(versions) {
  assert_that(is.list(versions))
  if (length(versions) > 0L) {
    assert_that(assertthat::has_attr(versions, "names"))
    Map(assert_that, Map(assertthat::has_attr, versions, "storage"),
        msg = "'storage' attribute missing.")
    versions <- lapply(seq_along(versions), function(idx) {
      version <- c(versions[[idx]])
      storage <- attr(versions[[idx]], "storage")
      node <- list(
        storage = storage,
        version = version
      )
      if (storage == "on-disk") {
        node[["storage"]] <- "on-disk"
        node[["filepath"]] <- names(versions)[idx]
        node[["name"]] <- NA_character_
      } else {
        node[["storage"]] <- "in-memory"
        node[["filepath"]] <- NA_character_
        node[["name"]] <- names(versions)[idx]
      }
      node
    })
  }
  versions
}

# Transform a structured list of versions into a named list of versions with
# 'storage' attributes.
.flatten_versions <- function(versions) {
  assert_that(is.list(versions))
  if (length(versions) > 0L) {
    Map(assert_that, Map(is.list, versions), msg = "unstructured versions.")
    Map(assert_that, Map(assertthat::has_attr, versions, "names"),
        msg = "unnamed versions.")
    assert_that(
      all(do.call(c, Map(all, lapply(
        versions, assertthat::has_name,
        c("storage", "version", "filepath", "name"))))),
      msg = "badly structured versions.")
    assert_that(
      all(is.character(do.call(c, lapply(versions, `[[`, "storage")))),
      msg = "badly structured versions.")
    assert_that(
      all(do.call(c, lapply(versions, `[[`, "storage")) %in%
            c("in-memory", "on-disk")),
      msg = "badly structured versions.")
    assert_that(
      all(.is_version(do.call(c, lapply(versions, `[[`, "version")))),
      msg = "badly structured versions.")
    assert_that(
      all(is.character(do.call(c, lapply(versions, `[[`, "filepath")))),
      msg = "badly structured versions.")
    assert_that(
      all(is.character(do.call(c, lapply(versions, `[[`, "name")))),
      msg = "badly structured versions.")
    versions <-
      stats::setNames(
        versions,
        Map(function(v) {
          ifelse(v[["storage"]] == "on-disk", v[["filepath"]], v[["name"]])
        },
        versions))
    versions <- Map(function(version) {
      node <- version[["version"]]
      attr(node, "storage") <- version[["storage"]]
      node
    },
    versions)
  }
  versions
}

# TODO test that
.cumpaste <- function(x, sep = " ")
  Reduce(function(x1, x2) paste(x1, x2, sep = sep), x, accumulate = TRUE)

.parse_modulrignore_file <- memoise::memoise(function(path) {
  file <- file.path(path, ".modulrignore")
  if (file.exists(file)) {
    con <- file(file, open = "r")
    on.exit(close(con))
    grep("^[[:space:]]*#", readLines(con), value = TRUE, invert = TRUE)
  }
}, ~memoise::timeout(3L))

# Find all in-memory and on-disk candidates for a given module name.
.resolve_candidates <- function(name, scope_name = NULL, absolute = TRUE,
                                extensions = c(".R", ".r",
                                               ".Rmd", ".rmd",
                                               ".Rnw", ".rnw"),
                                include.dirs = FALSE) {
  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name),
    is.character(extensions),
    assertthat::is.flag(include.dirs))

  resolved_namespace <- .resolve_namespace(name, scope_name)
  resolved_name <- resolved_namespace[["resolved"]]
  parsed_name <- .parse_name(resolved_name)

  # looking for on-disk candidates

  pattern <- sprintf("(?:%s)", paste(paste0(
    "^", sprintf("(?:%s)", parsed_name[["final"]]),
    paste0(.version_hash_string_regex, "?"),
    sprintf(ifelse(include.dirs, "(?:%s)?$", "%s"),
            utils::glob2rx(sprintf("*%s", extensions), trim.head = TRUE))),
    collapse = "|"))

  roots <-
    unique(c(root_config$get_all()[[1L]], "."))

  files <- c()
  for (root in roots) {
    if (.dir_exists(root)) {

      exclude_globs <-
        .parse_modulrignore_file(
          file.path(root, .name_to_first_path(parsed_name[["initials"]])))

      path <- .remove_trailing_filesep(
        file.path(root, .name_to_path(parsed_name[["initials"]])))
      if (.dir_exists(path)) {
        walks <- c("", .cumpaste(
          strsplit(path, split = .Platform$file.sep, fixed = TRUE)[[1L]],
          sep = .Platform$file.sep))
        if (!any(file.exists(file.path(walks, "__IGNORE__")))) {
          files_ <-
            ifelse(absolute, normalizePath,
                   Vectorize(.remove_duplicate_filesep, "path"))(
                     list.files(path = path, pattern = pattern,
                                full.names = TRUE, include.dirs = include.dirs))
          if (length(files_) > 0L) {
            if (length(exclude_globs) > 0L)
              files_ <-
                files_[!apply(
                  matrix(Vectorize(grepl, vectorize.args = "pattern")(
                    utils::glob2rx(file.path(
                      normalizePath(root), exclude_globs)),
                    lapply(files_, normalizePath)),
                    ncol = length(exclude_globs)), 1L, any)]
            files <- c(files, files_)
          }
        }
      }
    }
  }

  on_disk_versions <- do.call(c, Map(function(file) {
    .parse_version(basename(file))[["version"]]
  },
  files))

  on_disk_versions <-
    lapply(as.list(on_disk_versions), `attr<-`, "storage", "on-disk")

  # looking for in-memory candidates
  pattern <- paste0(
    "^", sprintf("(?:%s)", parsed_name[["namespace"]]),
    .version_hash_string_regex, "?$")
  mods <- grep(pattern,
               names(.modulr_env$injector$registry),
               value = TRUE)
  in_memory_versions <-
    do.call(c, lapply(Map(.parse_version, mods), `[[`, "version"))
  in_memory_versions <-
    lapply(as.list(in_memory_versions), `attr<-`, "storage", "in-memory")

  # resolving on-disk and in-memory versions

  versions <- c(
    in_memory_versions,
    on_disk_versions
  )

  versions <- .filter_versions(
    versions = versions,
    version = parsed_name[["version"]], symbol = parsed_name[["symbol"]],
    include_NAs = TRUE)

  resolution <- .unflatten_versions(versions)

  list(
    name = name,
    scope_name = scope_name,
    resolved = resolution,
    namespace = resolved_namespace
  )

}

# Flag sub-version of a version, e.g. 1.0.1 is sub-1.0.
.is_sub_version <- function(version, sub_version) {

  assert_that(
    is.na(version) || .is_version(version),
    is.na(sub_version) || .is_version(sub_version))

  if (is.na(version)) return(TRUE)

  version <- unclass(version)[[1L]]
  sub_version <- unclass(sub_version)[[1L]]
  level <- length(version)
  if (level > length(sub_version)) return(FALSE)

  all(vapply(seq_len(level), FUN = function(i) {
    version[i] == sub_version[i]
  },
  FUN.VALUE = TRUE))

}

# Extract the name of a module definition in a file.
.extract_name <- function(filepath = NULL, text = NULL, namespace = NULL,
                          version = NA, strict = FALSE) {

  assert_that(
    (!is.null(text) && assertthat::is.string(text)) ||
      (!is.null(filepath) && file.exists(filepath)))
  assert_that(is.null(namespace) || .is_namespace(namespace))
  assert_that(is.na(version) || .is_version(version))
  assert_that(assertthat::is.flag(strict))

  extract_ <- function(x, all = x, idx = c(), strict = TRUE) {

    loop_ <- function(e) {
      if (is.expression(e)) {
        unlist(lapply(e, loop_))
      } else if (is.language(e) && is.recursive(e)) {
        if (
          identical(e[[1L]], quote(`%provides%`)) ||
          identical(e[[1L]], quote(`%provides_options%`)) ||
          identical(e[[1L]], quote(`%requires%`)) ||
          identical(e[[1L]], quote(`%clones%`)) ||
          identical(e[[1L]], quote(define)) ||
          identical(e[[1L]], quote(clone))) {
          if (is.character(e[[2L]])) e[[2L]] else loop_(e[[2L]])
        }
      }
    }

    items <- loop_(x)

    if (length(items) > 0L) {
      parsed_names <- Map(.parse_name, items)
      finals <- Map(`[[`, parsed_names, "final")
      namespaces <- Map(`[[`, parsed_names, "namespace")
      versions <- Map(`[[`, parsed_names, "version")
      is_sub_version <-
        Vectorize(.is_sub_version, vectorize.args = "sub_version")(
          version, do.call(c, versions))
      if (any(is_sub_version)) {
        if (!is.null(namespace)) {
          # must be strict
          idx <- which(
            is_sub_version &
              namespaces == namespace &
              (!strict |
                 ifelse(
                   is.null(filepath), TRUE,
                   finals ==
                     .parse_name(
                       .parse_filepath(filepath)[["name"]])[["final"]])))
          if (length(idx) >= 1L) names(idx)
        } else {
          names(which.max(
            is_sub_version &
              (!strict | is.null(filepath) | finals ==
                 .parse_name(
                   .parse_filepath(filepath)[["name"]])[["final"]])))
        }
      }
    }

  }

  if (is.null(text)) {

    if (tolower(tools::file_ext(filepath)) %in% c("rmd", "rnw")) {

      opat <- knitr::knit_patterns$get()
      oopts_knit <- knitr::opts_knit$get()
      oopts_template <- knitr::opts_template$get()
      oopts_hooks <- knitr::opts_hooks$get()
      oopts_chunk <- knitr::opts_chunk$get()
      oopts_current <- knitr::opts_current$get()

      knitr::knit_patterns$restore()
      on.exit(knitr::knit_patterns$set(opat), add = TRUE)
      knitr::opts_knit$restore()
      on.exit(knitr::opts_knit$set(oopts_knit), add = TRUE)
      knitr::opts_template$restore()
      on.exit(knitr::opts_template$set(oopts_template), add = TRUE)
      knitr::opts_hooks$restore()
      on.exit(knitr::opts_hooks$set(oopts_hooks), add = TRUE)
      knitr::opts_chunk$restore()
      on.exit(knitr::opts_chunk$set(oopts_chunk), add = TRUE)
      knitr::opts_current$restore()
      on.exit(knitr::opts_current$set(oopts_current), add = TRUE)

      knitr::opts_knit$set(
        "unnamed.chunk.label" =
          paste("modulr", filepath, sep = "-"),
        "tidy" = FALSE)

      script <-
        knitr::knit(text = readChar(filepath, file.info(filepath)[["size"]]),
                    tangle = TRUE, quiet = TRUE)

      args <- list(text = script)

    } else {

      args <- list(file = filepath)

    }

    # Usually, the module is defined in the first expressions, so we parse the
    # beginning of the file only.
    parsed <- tryCatch(
      do.call(parse, args = c(args, list(n = 2L, keep.source = FALSE))),
      error = function(e) {
        e[["call"]] <- NULL
        stop(e)
      },
      silent = TRUE)
    name <- extract_(parsed, strict = FALSE)
    if (
      !is.null(name) &&
      name == .parse_name(.parse_filepath(filepath)[["name"]])[["final"]])
      return(name)


    # If no name has been found in the first expression, we then parse the whole
    # file.
    parsed <- tryCatch(
      do.call(parse, args = c(args, list(keep.source = FALSE))),
      error = function(e) {
        e[["call"]] <- NULL
        stop(e)
      },
      silent = TRUE)

    return(extract_(parsed, strict = strict))

  } else {

    args <- list(text = text)
    parsed <- tryCatch(
      do.call(parse, args = c(args, list(keep.source = FALSE))),
      error = function(e) {
        e[["call"]] <- NULL
        stop(e)
      },
      silent = TRUE)

    return(extract_(parsed, strict = strict))

  }

}

# Parse all candidates for a module name to find the best fitting version.
.resolve_name <- function(name, scope_name = NULL, absolute = TRUE, all = TRUE,
                          extensions = c(".R", ".r",
                                         ".Rmd", ".rmd",
                                         ".Rnw", ".rnw"),
                          include.dirs = FALSE) {

  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name),
    is.character(extensions))

  candidates <-
    .resolve_candidates(
      name = name, scope_name = scope_name,
      absolute = absolute, extensions = extensions,
      include.dirs = include.dirs)

  parsed_version <-
    .parse_version(candidates[[c("namespace", "resolved")]])
  parsed_name <-
    .parse_name(candidates[[c("namespace", "resolved")]])

  # differentiate treatment of on-disk and in-memory modules

  on_disk_candidates <-
    Filter(function(candidate) {
      candidate[["storage"]] == "on-disk"
    },
    candidates[["resolved"]])

  on_disk_versions <- list()
  for (idx in seq_along(on_disk_candidates)) {
    filepath <- on_disk_candidates[[idx]][["filepath"]]
    version <- on_disk_candidates[[idx]][["version"]]
    extracted_names <- .extract_name(
      filepath, namespace = parsed_name[["namespace"]], version = version)
    if (!is.null(extracted_names)) {
      for (extracted_name in extracted_names) {
        parsed_extracted_name <- .parse_name(extracted_name)
        node <- on_disk_candidates[[idx]]
        node[["version"]] <- parsed_extracted_name[["version"]]
        on_disk_versions <- c(list(node), on_disk_versions)
      }
    }
  }

  in_memory_versions <-
    Filter(function(candidate) {
      candidate[["storage"]] == "in-memory"
    },
    candidates[["resolved"]])

  versions <- .flatten_versions(c(on_disk_versions, in_memory_versions))

  versions <-
    .filter_versions(
      versions, parsed_version[["version"]], parsed_version[["symbol"]])

  versions <- .unflatten_versions(versions)

  resolution <- lapply(versions, function(version) {
    if (version[["storage"]] == "on-disk") {
      version[["name"]] <-
        ifelse(is.na(version[["version"]]), parsed_name[["namespace"]],
               paste(parsed_name[["namespace"]],
                     as.character(version[["version"]]),
                     sep = "#"))
    }
    version
  })

  if (length(resolution) > 0L && !all) {
    version <- as.character(utils::tail(resolution, 1L)[[1L]][["version"]])
    resolution <- Filter(function(node) {
      !isTRUE(as.character(node[["version"]]) != version)
    },
    resolution)

    if (length(resolution) > 1L) {
      # if an in-memory module has a corresponding on-disk instance with same
      # version number, we keep the on-disk module only
      storages <- unlist(lapply(resolution, `[[`, "storage"))
      if (any(storages == "on-disk")) {
        resolution <- resolution[storages == "on-disk"]
      }
      resolution <- utils::tail(resolution, 1L)
    }
  }

  list(
    name = name,
    scope_name = scope_name,
    resolved = resolution,
    candidates = candidates
  )

}

#' Find a Module.
#'
#' Find a module, in the context of a module scope, if any.
#'
#' @inheritParams define
#' @param scope_name A module name to use as scope (see \code{\link{define}},
#'   \code{\link{maps_config}}, and examples).
#' @param absolute A flag. Should the returned path be absolute? (see
#'   \code{\link{define}}, \code{\link{root_config}}, and examples)
#' @param extensions A character vector. File extensions to consider.
#'
#' @return A list containing informations relative to the module: name, version,
#'   in-memory or on-disk storage, and file path (absolute or relative).
#'
#' @seealso \code{\link{define}}, \code{\link{find_path}},
#'   \code{\link{maps_config}}, \code{\link{reset}}, and
#'   \code{\link{root_config}},
#'
#' @examples
#' reset()
#' define("foo", NULL, function() "Hello World!")
#' find_module("foo")
#'
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() "Hello World!")', file = tmp_file)
#' root_config$set(tmp_dir)
#' set_verbosity(1L)
#' find_module("foo")
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
#' find_module("vendor/great_module")
#' maps_config$set("foo/bar" = list("vendor/great_module" =
#'                                  "vendor/old_great_module"))
#' find_module("vendor/great_module", "foo/bar")
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @export
find_module <- function(name, scope_name = NULL, absolute = TRUE,
                        extensions = c(".R", ".r",
                                       ".Rmd", ".rmd",
                                       ".Rnw", ".rnw")) {
  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name),
    assertthat::is.flag(absolute),
    is.character(extensions))

  resolved <- .resolve_name(
    name = name, scope_name = scope_name,
    absolute = absolute, all = FALSE,
    extensions = extensions)[["resolved"]]

  if (length(resolved) > 0L) {
    resolved[[1L]][c("name", "version", "storage", "filepath")]
  }

}

#' Find the Path of a Module.
#'
#' Find the path of a module, in the context of a module scope, if any. The
#' returned path can be absolute or relative to a root directory.
#'
#' @inheritParams find_module
#' @param ... Further arguments to be passed to \code{\link{find_module}}.
#'
#' @return A string containing the path (relative or absolute) of the module.
#'
#' @seealso \code{\link{define}}, \code{\link{find_module}},
#'   \code{\link{maps_config}}, \code{\link{reset}}, and
#'   \code{\link{root_config}},
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
find_path <- function(name, scope_name = NULL, ...) {

  assert_that(
    .is_conform(name),
    is.null(scope_name) || .is_exact(scope_name))

  module <- find_module(name = name, scope_name = scope_name, ...)

  if (!is.null(module) && module[["storage"]] == "on-disk") {
    return(stats::setNames(module[["filepath"]],
                           module[["name"]]))
  }

}

# Deprecated and kept for backward compatibility.
# nocov start
.deprecated_resolve_path <- function(...) {
  resolved_name <- .resolve_name(..., include.dirs = TRUE)
  resolved <- (if (length(resolved_name[["resolved"]]) > 0L)
    resolved_name else resolved_name[["candidates"]])[["resolved"]]
  if (length(resolved) > 0L) {
    resolved <- resolved[[1L]]
    if (resolved[["storage"]] == "on-disk") {
      return(resolved[["filepath"]])
    } else {
      return(resolved[["name"]])
    }
  }
}
# nocov end

# Deprecated and kept for backward compatibility.
# nocov start
.deprecated_resolve_mapping <- function(...) {
  mapping <- .resolve_mapping(...)
  if (!is.null(mapping)) mapping[["resolved"]]
}
# nocov end
