.resolve_mapping <- function(name, scope_name) {
  mappings <- get("configuration", pos = modulr_env)$mappings[[scope_name]]
  if(is.null(mappings)) return(name)

  candidates <- Map(function(map) {
    start_end_pos <- str_locate(name, map)
    list(
      map = map
      , start = start_end_pos[1]
      , end = start_end_pos[2]
    )
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
    "Considering only the first occurence.")

  matching_map <- candidates[[1]]$map

  str_replace(name, matching_map, mappings[[matching_map]])

}

.make_path <- function(path) {
  if(str_sub(path, -1) != "/") path <- paste0(path, "/")
  path
}

.split_filename <- function(filename) {
  components <- str_split(filename, "/")[[1]]
  basename <- tail(components, 1)
  basename_components <- str_split(basename, "\\.")[[1]]
  if(length(basename_components) > 1) {
    extension <- tail(basename_components, 1)
    name <- paste(head(basename_components, length(basename_components) - 1), collapse = ".")
  } else {
    extension <- ""
    name <- basename_components
  }
  path <- paste(
    head(components, length(components) - 1),
    collapse = "/")
  list(
    filename = filename
    , path = path
    , basename = basename
    , name = name
    , extension = extension
  )
}

resolve_path <- function(name, scope_name) {
  configuration <- get("configuration", pos = modulr_env)

  if(missing(scope_name)) injected_name <- name else
    injected_name <- .resolve_mapping(name, scope_name)

  splitted_injected_name <- .split_filename(injected_name)
  injected_namespace <- splitted_injected_name$path
  injected_basename <- splitted_injected_name$basename

  candidates <- Map(function(namespace) {
    path <- .split_filename(.make_path(namespace))$path
    start_end_pos <- str_locate(injected_namespace, path)
    list(
      namespace = namespace
      , start = start_end_pos[1]
      , end = start_end_pos[2]
    )
  },
  names(configuration$paths))

  candidates <- Filter(function(candidate) {
    candidate$start == 1
  }, candidates)

  if(length(candidates) == 0) return(injected_name)

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

  str_replace(injected_name, matching_namespace,
              normalizePath(configuration$paths[[matching_namespace]]))
}
