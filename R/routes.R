# Paths should end with exactly one trailing slash
.make_path <- function(path) {

  assert_that(assertthat::is.string(path))

  paste0(gsub("\\/+$", "", file.path(path)), "/")

}

# For a filename, we need to know the following attributes:
# path, basename, name (as a module) and extension
.parse_filename <- function(filename) {

  assert_that(assertthat::is.string(filename))

  filename <- gsub("^\\s*|\\s*$", "", file.path(filename))

  filename_shifted <- file.path(paste0(filename, .Platform$path.sep))

  path <- dirname(filename_shifted) # add "." for "foo/" to be a path

  basename <- sub(sprintf("%s$", .Platform$path.sep), "",
                  basename(filename_shifted))

  name <- basename(tools::file_path_sans_ext(basename))

  extension <- tools::file_ext(filename)

  list(
    filename = filename,
    path = path,
    basename = basename,
    name = name,
    extension = extension)

}

# From a module path, we need to infer its absolute path in the filesystem.
# If several root paths are present, the first occurrence of an absolute path
# matching an existing file (with extension) or directory is returned.
.find_absolute_path <- function(path,
                                extensions = c(".R", ".r", ".Rmd", ".rmd")) {

  assert_that(
    assertthat::is.string(path),
    is.character(extensions))

  root <- unique(c(root_config$get_all()[[1]], "."))

  for (root_candidate in root) {

    candidate_path <- file.path(root_candidate, path)

    for (ext in extensions) {

      path_candidate <- paste0(candidate_path, ext)

      if(file.exists(path_candidate)) return(path_candidate)

    }

    if(.dir_exists(candidate_path)) return(candidate_path)

  }

  invisible()

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

  if(is.null(mappings)) return(name)

  candidates <- Map(
    function(map) {

      reg <- regexpr(map, name)

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

  if(length(candidates) == 0) return(name)

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

  if(length(candidates) > 1) warning(
    "More than one matching mapping. ",
    "Considering only the first occurence.",
    call. = FALSE, immediate. = TRUE)

  matching_map <- candidates[[1]][["map"]]

  sub(matching_map, mappings[[matching_map]], name)

}

# From a module name, we want to infer its (absolute) path.
.resolve_path <- function(name, scope_name = NULL, absolute = TRUE, ...) {

  assert_that(
    assertthat::is.string(name),
    is.null(scope_name) || assertthat::is.string(scope_name),
    assertthat::is.flag(absolute))

  if(is.null(scope_name)) injected_name <- name else
    injected_name <- .resolve_mapping(name, scope_name)

  parsed_injected_name <- .parse_filename(injected_name)

  injected_namespace <- parsed_injected_name$path

  candidates <- Map(
    function(namespace) {

      path <- .parse_filename(.make_path(namespace))$path

      reg <- regexpr(path, injected_namespace)

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

  if(length(candidates) == 0) {

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

    if(length(candidates) > 1) warning(
      "More than one matching namespace. ",
      "Considering only the first occurence.")

    matching_namespace <- candidates[[1]]$namespace

    candidate <- sub(
      matching_namespace,
      paths_config$get_all()[[matching_namespace]],
      injected_name)

  }

  if(absolute)
    return(.find_absolute_path(candidate, ...))

  candidate

}
