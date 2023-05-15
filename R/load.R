# Load a module at a given location.
.load_module <- function(path, name = NULL, check = TRUE) {

  if (!is.null(path)) {

    .modulr_env$injector$.__name__ <- if (is.null(name)) "__noname__" else name
    on.exit(rm(list = ".__name__", pos = .modulr_env$injector))

    loaded <- FALSE

    registry <- .modulr_env$injector$registry
    .Last.name <- .modulr_env$injector$.Last.name
    config <- .modulr_env$injector$config
    verbosity <- .modulr_env$injector$verbosity
    stash <- .modulr_env$injector$stash

    rollback <- function() {
      .modulr_env$injector$registry <- registry
      .modulr_env$injector$.Last.name <- .Last.name
      .modulr_env$injector$config <- config
      .modulr_env$injector$verbosity <- verbosity
      .modulr_env$injector$stash <- stash
    }

    wrapper_env <- new.env()
    wrapper_env$.__name__ <- if (is.null(name)) "__noname__" else name

    if (tolower(tools::file_ext(path)) == "r") {

      if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable() && exists("debugSource")) {

        # nocov start
        # Seems that tryCatch is unable to catch errors happening in an
        # RStudio's debugSource call (version 0.98.1103).
        try(stop("fake error", call. = FALSE), silent = TRUE)
        last_error <- geterrmessage()
        tryCatch({
          local(do.call("debugSource", args = list(path, echo = FALSE)),
                envir = wrapper_env) # nocov
          if (last_error != geterrmessage()) {
            stop(sub("\n$", "", geterrmessage()), call. = FALSE)
          } else {
            try(stop(last_error, call. = FALSE), silent = TRUE)
          }
          loaded <- TRUE
        },
        error = function(e) {
          rollback()
          e$message <- sprintf("%s Rolling back.", e$message)
          on.exit(try(stop(e), silent = TRUE))
          stop("Rolling back.", call. = FALSE)
        })
        # nocov end

      } else {
        tryCatch({
          local(source(path, local = TRUE, echo = FALSE, keep.source = TRUE),
                envir = wrapper_env)
          loaded <- TRUE
        },
        error = function(e) {
          rollback()
          e$message <- sprintf("%s Rolling back.", e$message)
          stop(e)
        })
      }

    } else if (tolower(tools::file_ext(path)) %in% c("rmd", "rnw")) {

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

      knitr::opts_knit$set("unnamed.chunk.label" =
                           paste("modulr", path, sep = "-"))

      script <-
        knitr::knit(text = readChar(path, file.info(path)[["size"]]),
                    tangle = TRUE, quiet = TRUE)

      tryCatch({
        local(eval(parse(text = script, keep.source = TRUE)),
              envir = wrapper_env)
        loaded <- TRUE
      },
      error = function(e) {
        rollback()
        e$message <- sprintf("%s Rolling back.", e$message)
        stop(e)
      })

    }

    loaded_names <-
      setdiff(names(.modulr_env$injector$registry), names(registry))
    if (loaded)
      loaded_names <- c(name, loaded_names)
    loaded_names <- unique(loaded_names)

    along <- NA_character_
    if (length(loaded_names) > 1L) {
      along <- if (is.null(name)) {
        parsed_names <- lapply(loaded_names, .parse_name)
        finals <- do.call(c, lapply(parsed_names, `[[`, "final"))
        file_final <-
          .parse_name(basename(tools::file_path_sans_ext(path)))[["final"]]
        along_ <- loaded_names[finals == file_final]
        if (length(along_) > 0L) along_[1L] else loaded_names[1L]
      } else name
    }

    Map(function(name_) {
      .modulr_env$injector$registry[[c(name_, "storage")]] <- "on-disk"
      .modulr_env$injector$registry[[c(name_, "filepath")]] <- path
      .modulr_env$injector$registry[[c(name_, "along")]] <-
        ifelse(isTRUE(name_ != along), along, NA_character_)
    },
    loaded_names)

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

  assert_that(.is_conform(name))

  if (.is_called_from_within_module()) {
    warning("load_module is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (.is_nested_load()) {
    warning("loading calls are nested. Stopping recursion.",
            call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  module <- find_module(name, absolute = FALSE)
  loaded_module <- NA
  if (!is.null(module)) {
    name <- module[["name"]]
    if (module[["storage"]] == "on-disk" && file.exists(module[["filepath"]])) {
      loaded_module <-
        .load_module(path = module[["filepath"]], name = name,
                     check = TRUE)
    }
  }
  assert_that(.is_defined(name))
  stats::setNames(loaded_module, name)

}

#' Load All Modules.
#'
#' Load or reload all modules which are defined in the named directory.
#'
#' @inheritParams base::list.files
#' @param strict A flag. Should stop at errors?
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
  path = root_config$get_all()[[1L]],
  pattern = "[^_]\\.[Rr](?:md|nw)?$",
  full.names = TRUE,
  recursive = TRUE,
  strict = TRUE,
  ...) {

  .message_meta("Entering load_all_modules() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("load_all_modules is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  files <- list.files(
    path = path, pattern = pattern,
    full.names = full.names, recursive = recursive, ...)

  loader <- if (strict) {
    .load_module
  } else {
    function(...) try(.load_module(...))
  }

  if (length(files) > 0L)
    Map(loader, files, check = FALSE)

  invisible(NULL)

}

# We need to make sure all dependent modules of a given module are defined.
.define_all_dependent_modules <- function(group) {

  assert_that(is.character(group))

  visited_dependencies <- c()

  iteration <- function(name, scope_name = NULL) {

    if (!(name %in% visited_dependencies)) {

      loaded_module <-
        stats::setNames(names(load_module(name)), name)

      visited_dependencies <<- c(visited_dependencies, loaded_module)

      Map(function(dependency) iteration(dependency, name),
          .modulr_env$injector$registry[[c(loaded_module, "dependencies")]])

    }

  }

  for (name in group)
    iteration(name)

  visited_dependencies

}
