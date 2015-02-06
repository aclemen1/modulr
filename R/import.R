#' Import module.
#'
#' @export

import <- function(name, scope_name, force_reimport = F) {
  if(!(name %in% RESERVED_NAMES) & (!.is_defined(name) | force_reimport)) {
    if(missing(scope_name)) path <- resolve_path(name) else
      path <- resolve_path(name, scope_name)
    if(file.exists(paste0(path, ".R"))) {
      #       message_open(sprintf("Module '%s'", name))
      #       message_close("Found in .R file ")
      source(paste0(path, ".R"))
      return(paste0(path, ".R"))
    } else if(file.exists(paste0(path, ".Rmd"))) {
      #       message_open(sprintf("Module '%s'", name))
      #       message_close("Found in .Rmd file")
      unnamed_chunk_label_opts = knitr::opts_knit$get("unnamed.chunk.label")
      knitr::opts_knit$set("unnamed.chunk.label" = paste("modulr", name, sep="/"))
      source(knitr::knit(paste0(path, ".Rmd"), output = tempfile(fileext = ".R"), tangle = T, quiet = T))
      knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)
      return(paste0(path, ".Rmd"))
    } else if (!.is_defined(name)) {
      message_open(sprintf("Module '%s'", name))
      message_close(sprintf("Not found", path))
    }
    NULL
  }
}

#' Reimport module.
#'
#' @export

reimport <- function(name, scope_name)
  import(name, scope_name, force_reimport = T)


#' Redefine module.
#'
#' @export

redefine <- reimport

.is_defined <- function(name) {
  !is.null(get("register", pos = modulr_env)[[name]])
}


# make sure all dependent modules are defined
.define_all_dependent_modules <- function(name, force_reimport_all = F) {
  visited_dependencies <- list()
  iteration <- function(name, scope_name) {
    if(!(name %in% visited_dependencies)) {
      import(name, scope_name, force_reimport_all)
      visited_dependencies <<- c(visited_dependencies, name)
      Map(function(dependency) iteration(dependency, name),
          get("register", pos = modulr_env)[[name]]$dependencies)
    }
  }
  iteration(name)
  unlist(visited_dependencies)
}
