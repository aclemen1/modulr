# TODO test that!
.parse_root_name <- function(core_name) {
  parts <- strsplit(core_name, "/", fixed = TRUE)[[1]]
  list(
    path = paste(head(parts, -1L), collapse = .Platform$file.sep),
    basename = tail(parts, 1L)
  )
}

# TODO test that!
.acceptable_version <- function(base_version, base_symbol, version, symbol) {
  if (is.na(base_version)) {
    # no base version provided
    if (base_symbol == "*") {
      # latest version accepted, no version accepted
      !is.na(version) || isTRUE(symbol == "*") ||
        (is.na(version) && !isTRUE(symbol == "*"))
    } else {
      # only no version accepted
      is.na(version) && !isTRUE(symbol == "*")
    }
  } else {
    # base version provided
    if (base_symbol == "^") {
      # e.g. 1.x.x
      version$major == base_version$major &&
        (version$minor > base_version$minor ||
           (version$minor == base_version$minor &
              version$patchlevel >= base_version$patchlevel))
    } else if (base_symbol == "~") {
      # e.g. 1.2.x
      version$major == base_version$major &&
        version$minor == base_version$minor &&
        version$patchlevel >= base_version$patchlevel
    } else {
      # exact match
      base_version == version
    }
  }
}

# TODO test that!
.remove_duplicate_filesep <- function(path) {
  gsub(paste(c(rep(.Platform$file.sep, 2), "+"), collapse = ""),
       .Platform$file.sep, path)
}

# Paths should end with exactly one trailing slash
.make_path <- function(path) {

  assert_that(assertthat::is.string(path))

  .remove_duplicate_filesep(paste0(path, .Platform$file.sep))

}

# For a filename, we need to know the following attributes:
# path, basename, name (as a module) and extension
.parse_filename <- function(filename) {

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

  name <- basename(tools::file_path_sans_ext(base_name))

  v <- .parse_version(name)

  list(
    filename = filename,
    path = path,
    basename = base_name,
    name = name,
    extension = tools::file_ext(filename),
    version = v[["version"]],
    symbol = v[["symbol"]]
  )

}

# Mapping allows to resolve a new module name depending on a scope.
# This is very useful when two different versions of a module are required
# from different modules. For instance, "some/old_module" and "some/module"
# both require "foo/bar", but we would like "foo/bar" to be resolved as
# "foo/bar_V1" for "some/old_module" only. See tests for more examples.
.resolve_mapping <- function(name, scope_name = NULL) {

  assert_that(
    assertthat::is.string(name),
    is.null(scope_name) || assertthat::is.string(scope_name))

  mappings <- maps_config$get(scope_name)

  if (is.null(mappings)) return(name)

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

  sub(matching_map, mappings[[matching_map]], name)

}

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
  candidates <- .find_candidates(
    name = name, scope_name = scope_name,
    absolute = absolute, extensions = extensions,
    all = FALSE)

  if (length(candidates) > 0L)
    candidates[[1]]$filename
}

# TODO test that!
.find_candidates <- function(name, scope_name = NULL, absolute = TRUE,
                             extensions = c(".R", ".r",
                                            ".Rmd", ".rmd",
                                            ".Rnw", ".rnw"),
                             all = FALSE) {

  assert_that(.is_conform(name))

  resolved <-
    .resolve_path(name = name, scope_name = scope_name, extensions = extensions)

  parsed_name <- .parse_name(resolved$name)
  parsed_root_name <- .parse_root_name(parsed_name$root)
  parsed_root_path <- .parse_root_name(.parse_name(resolved$path)$root)

  pattern <- sprintf("(?:%s)", paste(paste0(
    "^", parsed_root_name$basename,
    paste0(.version_regex, "?"),
    glob2rx(sprintf("*%s", extensions), trim.head = TRUE)),
    collapse = "|"))

  files <- c()
  roots <- unique(c(root_config$get_all()[[1L]], "."))
  for (root in roots) {
    path <- file.path(root, parsed_root_path$path)
    files_ <-
      ifelse(absolute, normalizePath,
             Vectorize(.remove_duplicate_filesep, "path"))(
               list.files(path = path, pattern = pattern, full.names = TRUE))
    files <- c(files, files_)
  }

  candidates <- Filter(
    function(file) {
      .acceptable_version(parsed_name$version, parsed_name$symbol,
                          file$version, file$symbol)
    },
    Map(function(file) {
      parsed <- .parse_filename(file)
      parsed$root <- parsed_name$root
      if (!is.na(parsed$version)) {
        resolved_name <- paste(parsed_name$root, parsed$version, sep = "#")
      } else if (isTRUE(parsed$symbol == "*")) {
        resolved_name <- paste(parsed_name$root, "latest", sep = "#")
      } else {
        resolved_name <- parsed_name$root
      }
      parsed$resolved_name <- resolved_name
      parsed
    },
    files)
  )

  versions <- do.call(c, lapply(
    candidates, function(candidate) candidate$version))
  symbols <- do.call(c, lapply(
    candidates, function(candidate) candidate$symbol))
  latest <- which(symbols == "*")
  if (length(latest) > 0L) {
    candidates <- candidates[latest]
  } else if (length(versions) > 0L) {
    version_max <- max(versions, na.rm = TRUE)
    if (length(version_max) > 0L) {
      candidates <- Filter(function(candidate) {
        candidate$version == version_max
      },
      candidates)
    }
  }

  if (!all && length(candidates) > 0L) candidates <- tail(candidates, 1L)

  unname(candidates)

}

.resolve_path <- function(name, scope_name = NULL,
                          extensions = c(".R", ".r",
                                         ".Rmd", ".rmd",
                                         ".Rnw", ".rnw")) {

  assert_that(
    assertthat::is.string(name),
    is.null(scope_name) || assertthat::is.string(scope_name),
    is.character(extensions))

  if (is.null(scope_name)) injected_name <- name else
    injected_name <- .resolve_mapping(name, scope_name)

  parsed_injected_name <- .parse_filename(injected_name)

  injected_namespace <- parsed_injected_name$path

  candidates <- Map(
    function(namespace) {

      path <- .parse_filename(.make_path(namespace))$path

      reg <- regexpr(path, injected_namespace, fixed = TRUE)

      list(
        namespace = namespace,
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

    candidate <- injected_name

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

    matching_namespace <- candidates[[1]]$namespace

    candidate <- sub(
      matching_namespace,
      paths_config$get_all()[[matching_namespace]],
      injected_name)

  }

  list(
    name = injected_name,
    path = candidate
  )

}

# TODO test that!
resolve_name <- function(name, scope_name = NULL) {
  candidates <- .find_candidates(name = name, scope_name = scope_name)
  if (length(candidates) > 0L) {
    candidates[[1]]$resolved_name
  } else {
    .resolve_mapping(name = name, scope_name)
  }
}
