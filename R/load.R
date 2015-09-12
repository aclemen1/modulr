#' load_module module.
#'
#' @export
# TODO: write documentation
load_module <- function(name) {

  .message_meta(sprintf("Entering load_module() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("`%imports%` is called from within a module.",
            call. = F, immediate. = T)
  }

  if(.is_regular(name)) {

    path <- .resolve_path(name)

    if(!is.null(path)) {

      if(tolower(tools::file_ext(path)) == "r") {

        source(path)

      } else if(tolower(tools::file_ext(path)) == "rmd") {

        unnamed_chunk_label_opts <- knitr::opts_knit$get("unnamed.chunk.label")

        knitr::opts_knit$set("unnamed.chunk.label" =
                               paste("modulr", name, sep="/"))

        tmp_file <- tempfile(fileext = ".R")
        source(knitr::knit(path,
                           output = tmp_file,
                           tangle = T, quiet = T))

        try(unlink(tmp_file), silent = T)

        knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)

      }

    }

    assertthat::assert_that(.is_defined(name))

    return(path)

  }

}

# We need to make sure all dependent modules of a given module are defined.
.define_all_dependent_modules <- function(group) {

  assertthat::assert_that(is.character(group))

  visited_dependencies <- list()

  iteration <- function(name, scope_name = NULL) {

    name <- .resolve_mapping(name, scope_name)

    if(!(name %in% visited_dependencies)) {

      load_module(name)

      visited_dependencies <<- c(visited_dependencies, name)

      Map(function(dependency) iteration(dependency, name),
          get("register", pos = modulr_env)[[name]]$dependencies)

    }

  }

  for (name in group)
    iteration(name)

  unlist(visited_dependencies)

}
