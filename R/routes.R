# Paths should end with exactly one trailing slash
.make_path <- function(path) {
  return(paste0(stringr::str_replace(file.path(path), "/+$", ""), "/"))
}

# For a filename, we need to know the following attributes:
# path, basename, name (as a module) and extension
.parse_filename <- function(filename) {
  if(is.null(filename)) return(NULL)
  if(is.na(filename)) return(NULL)
  filename <- stringr::str_trim(file.path(filename))
  filename_shifted <- file.path(paste0(filename, .Platform$path.sep))
  path <- dirname(filename_shifted) # add "." for "foo/" to be a path
  basename <- stringr::str_replace(basename(filename_shifted),
                                   sprintf("%s$", .Platform$path.sep), "")
  name <- basename(tools::file_path_sans_ext(basename))
  extension <- tools::file_ext(filename)
  return(list(
    filename = filename,
    path = path,
    basename = basename,
    name = name,
    extension = extension))
}

# From a module path, we need to infer its absolute path in the filesystem.
# If several root paths are present, the first occurrence of an absolute path
# matching an existing file (with extension) or directory is returned.
.find_absolute_path <- function(path,
                                extensions = c(".R", ".r", ".Rmd", ".rmd")) {
  root <- unique(c(root_config$get_all()[[1]], "."))
  for(root_candidate in root) {
    candidate_path <- file.path(root_candidate, path)
    for(ext in extensions) {
      path_candidate <- paste0(candidate_path, ext)
      if(file.exists(path_candidate)) return(path_candidate)
    }
    if(dir.exists(candidate_path)) return(candidate_path)
  }
  invisible(NULL)
}

# Mapping allows to resolve a new module name depending on a scope.
# This is very useful when two different versions of a module are required
# from different modules. For instance, "some/old_module" and "some/module"
# both require "foo/bar", but we would like "foo/bar" to be resolved as
# "foo/bar_V1" for "some/old_module" only. See tests for more examples.
.resolve_mapping <- function(name, scope_name = NULL) {

  if(!(is.character(name) & isTRUE(length(name) == 1)))
    stop("Type mismatch, string expected for name.", call. = F)

  if(!((is.character(scope_name) & isTRUE(length(scope_name) == 1)) |
       is.null(scope_name)))
    stop("Type mismatch, string expected for scope.", call. = F)

  mappings <- maps_config$get(scope_name)
  if(is.null(mappings)) return(name)

  candidates <- Map(function(map) {
    start_end_pos <- stringr::str_locate(name, map)
    list(
      map = map,
      start = start_end_pos[1],
      end = start_end_pos[2])
  }, names(mappings))

  candidates <- Filter(function(candidate) {
    candidate$start == 1
  }, candidates)

  if(length(candidates) == 0) return(name)

  maximum_length <- max(unlist(Map(function(candidate) {
    candidate$end
  }, candidates)))

  candidates <- Filter(function(candidate) {
    candidate$end == maximum_length
  }, candidates)

  if(length(candidates) > 1) warning(
    "More than one matching mapping. ",
    "Considering only the first occurence.",
    call. = F, immediate. = T)

  matching_map <- candidates[[1]]$map

  stringr::str_replace(name, matching_map, mappings[[matching_map]])

}

# From a module name, we want to infer its (absolute) path.
.resolve_path <- function(name, scope_name = NULL, absolute = T, ...) {
  if(!(is.character(name) & isTRUE(length(name) == 1)))
    stop("Type mismatch, string expected for name.", call. = F)

  if(!((is.character(scope_name) & isTRUE(length(scope_name) == 1)) |
         is.null(scope_name)))
    stop("Type mismatch, string expected for scope.", call. = F)

  configuration <- get("configuration", pos = modulr_env)

  if(is.null(scope_name)) injected_name <- name else
    injected_name <- .resolve_mapping(name, scope_name)

  parsed_injected_name <- .parse_filename(injected_name)
  injected_namespace <- parsed_injected_name$path
  injected_basename <- parsed_injected_name$basename

  candidates <- Map(function(namespace) {
    path <- .parse_filename(.make_path(namespace))$path
    start_end_pos <- stringr::str_locate(injected_namespace, path)
    list(
      namespace = namespace,
      start = start_end_pos[1],
      end = start_end_pos[2])},
    names(paths_config$get_all()))

  candidates <- Filter(function(candidate) {
    candidate$start == 1
  }, candidates)

  if(length(candidates) == 0) {
    candidate <- injected_name
  } else {
    maximum_length <- max(unlist(Map(function(candidate) {
      candidate$end
    }, candidates)))

    candidates <- Filter(function(candidate) {
      candidate$end == maximum_length
    }, candidates)

    if(length(candidates) > 1) warning(
      "More than one matching namespace. ",
      "Considering only the first occurence.")

    matching_namespace <- candidates[[1]]$namespace

    candidate <- stringr::str_replace(
      injected_name,
      matching_namespace,
      paths_config$get_all()[[matching_namespace]])
  }

  if(absolute) {
    .find_absolute_path(candidate, ...)
  } else {
    return(candidate)
  }
}

