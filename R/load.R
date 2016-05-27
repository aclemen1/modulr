# Load a module at a given location.
.load_module <- function(path, name = path, check = TRUE) {

  if (!is.null(path)) {

    register <- modulr_env$register
    .Last.name <- modulr_env$.Last.name
    config <- modulr_env$config
    verbosity <- modulr_env$verbosity
    stash <- modulr_env$stash

    rollback <- function() {
      modulr_env$register <- register
      modulr_env$.Last.name <- .Last.name
      modulr_env$config <- config
      modulr_env$verbosity <- verbosity
      modulr_env$stash <- stash
    }

    if (tolower(tools::file_ext(path)) == "r") {

      if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable()) {

        # nocov start
        tryCatch({
          # Seems that tryCatch is unable to catch errors happening in an
          # RStudio's debugSource call (version 0.98.1103).
          try(stop("fake error", call. = FALSE), silent = TRUE)
          last_error <- geterrmessage()
          local(do.call("debugSource", args = list(path, echo = FALSE))) # nocov
          if (last_error != geterrmessage()) {
            stop(sub("\n$", "", geterrmessage()), call. = FALSE)
          } else {
            try(stop(last_error, call. = FALSE), silent = TRUE)
          }
        },
        error = function(e) {
          rollback()
          e$message <- sprintf("%s. Rolling back.", e$message)
          on.exit(try(stop(e), silent = TRUE))
          stop("Rolling back.", call. = FALSE)
        })
        # nocov end

      } else {
        tryCatch({
          source(path, local = TRUE, echo = FALSE, keep.source = TRUE)
        },
        error = function(e) {
          rollback()
          e$message <- sprintf("%s. Rolling back.", e$message)
          stop(e)
        })
      }

    } else if (tolower(tools::file_ext(path)) %in% c("rmd", "rnw")) {

      unnamed_chunk_label_opts <- knitr::opts_knit$get("unnamed.chunk.label")

      knitr::opts_knit$set("unnamed.chunk.label" =
                             paste("modulr", name, sep = "/"))

      script <- knitr::knit(text = readChar(path, file.info(path)$size),
                            tangle = TRUE, quiet = TRUE)

      tryCatch({
        local(eval(parse(text = script, keep.source = TRUE)))
      },
      error = function(e) {
        rollback()
        e$message <- sprintf("%s. Rolling back.", e$message)
        stop(e)
      })

      knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)

    }

  }

  if (check)
    assert_that(.is_defined(name))

  return(path)

}

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
    warning("load_module is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (.is_nested_load()) {
    warning("loading calls are nested. Stopping recursion.",
            call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  if (.is_regular(name)) {

    path <- find_path(name)

    .load_module(path = path, name = name, check = TRUE)

  }

}

#' Load All Modules.
#'
#' Load or reload all modules which are defined in the named directory.
#'
#' @inheritParams base::list.files
#' @param ... Further arguments to be passed to \code{base::\link[base]{list.files}}.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{load_module}}, \code{\link{make_all}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() print("Hello World!"))', file = tmp_file)
#' tmp_file <- file.path(tmp_dir, "bar.R")
#' cat('define("bar", NULL, function() print("hELLO wORLD?"))', file = tmp_file)
#' load_all_modules(tmp_dir)
#' make_all()
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @export
load_all_modules <- function(
  path = c("lib", "libs", "module", "modules", "."),
  pattern = "[^_]\\.[rR][(?:md)|(?:nw)]?$",
  full.names = TRUE,
  recursive = TRUE, ...) {

  .message_meta("Entering load_all_modules() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("load_all_modules is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  files <- list.files(
    path = path, pattern = pattern,
    full.names = full.names, recursive = recursive, ...)

  if (length(files) > 0)
    Map(.load_module, files, check = FALSE)

  invisible(NULL)

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
