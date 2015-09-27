#' Load a Module.
#'
#' Load or reload a module which is defined in an R, R Markdown or R Sweave
#' file stored on disk.
#'
#' @inheritParams define
#'
#' @return The absolute file name of the loaded module.
#'
#' @details
#'
#' As described in \code{\link{define}} under \emph{Implicit Definition}, the
#' module name is resolved into a file location by applying the rules accessed
#' by \code{\link{root_config}}, \code{\link{paths_config}}, and
#' \code{\link{maps_config}}. R Markdown and R Sweave files are accordingly
#' tangled into R code, which is in turn evaluated.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{.Last.name}}, \code{\link{make}},
#'   \code{\link{maps_config}}, \code{\link{paths_config}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() "Hello World!")', file = tmp_file)
#' root_config$set(tmp_dir)
#' load_module("foo")
#' make()
#' unlink(tmp_dir, recursive = TRUE)
#' load_module("foo")
#'
#' @export
load_module <- function(name = .Last.name) {

  .message_meta(sprintf("Entering load_module() for '%s' ...", name),
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("`%imports%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (.is_regular(name)) {

    path <- .resolve_path(name)

    if (!is.null(path)) {

      if (tolower(tools::file_ext(path)) == "r") {

        source(path)

      } else if (tolower(tools::file_ext(path)) %in% c("rmd", "rnw")) {

        unnamed_chunk_label_opts <- knitr::opts_knit$get("unnamed.chunk.label")

        knitr::opts_knit$set("unnamed.chunk.label" =
                               paste("modulr", name, sep="/"))

        tmp_file <- tempfile(fileext = ".R")

        source(knitr::knit(path,
                           output = tmp_file,
                           tangle = TRUE, quiet = TRUE))

        try(unlink(tmp_file), silent = TRUE)

        knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)

      }

    }

    assert_that(.is_defined(name))

    return(path)

  }

}

# We need to make sure all dependent modules of a given module are defined.
.define_all_dependent_modules <- function(group) {

  assert_that(is.character(group))

  visited_dependencies <- list()

  iteration <- function(name, scope_name = NULL) {

    name <- .resolve_mapping(name, scope_name)

    if (!(name %in% visited_dependencies)) {

      load_module(name)

      visited_dependencies <<- c(visited_dependencies, name)

      Map(function(dependency) iteration(dependency, name),
          modulr_env$register[[c(name, "dependencies")]])

    }

  }

  for (name in group)
    iteration(name)

  unlist(visited_dependencies)

}
