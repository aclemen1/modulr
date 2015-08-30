#' Import module.
#'
#' @export
# TODO: write documentation
import <- function(name) {
  if(!(name %in% RESERVED_NAMES)) {
    path <- .resolve_path(name)

    if(!is.null(path)) {
      if(tolower(tools::file_ext(path)) == "r") {
        source(path)
      } else if(tolower(tools::file_ext(path)) == "rmd") {
        unnamed_chunk_label_opts = knitr::opts_knit$get("unnamed.chunk.label")
        knitr::opts_knit$set("unnamed.chunk.label" =
                               paste("modulr", name, sep="/"))
        source(knitr::knit(path,
                           output = tempfile(fileext = ".R"),
                           tangle = T, quiet = T))
        knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)
      }
    }

    if (!.is_defined(name)) {
      stop(sprintf("%s not found", name), call. = F)
    }

    return(path)
  }
}

# We need to know if a module is already defined.
.is_defined <- function(name) {
  !is.null(get("register", pos = modulr_env)[[name]])
}

# We need to make sure all dependent modules of a given module are defined.
# TODO: test that
.define_all_dependent_modules <- function(name) {
  visited_dependencies <- list()
  iteration <- function(name, scope_name = NULL) {
    name <- .resolve_mapping(name, scope_name)
    if(!(name %in% visited_dependencies)) {
      import(name)
      visited_dependencies <<- c(visited_dependencies, name)
      Map(function(dependency) iteration(dependency, name),
          get("register", pos = modulr_env)[[name]]$dependencies)
    }
  }
  iteration(name)
  unlist(visited_dependencies)
}
