#' Modulr: A Dependency Injection Framework for R
#'
#' @description
#' This package contains a Dependency Injection framework for R. It allows to
#' organize programs into discrete, modular, and loosely coupled units. By
#' nature, such modules are easy to develop, debug, test, reuse, share and
#' maintain in a wide range of common situations. This package may also allow
#' for higher compliance with best practices in software engineering.
#'
#' @details
#' To learn more about modulr, start with the vignettes and use
#' \code{browseVignettes(package = "modulr")}.
#'
#' For a complete list of functions,
#' use \code{library(help = "modulr")}.
#'
#' @docType package
#' @name modulr
#' @author Alain Clément <\email{alain.clement@@unil.ch}>
NULL

.modulr_env <- new.env(parent = emptyenv())

.initialize_injector <- function() {
  injector <- new.env(parent = emptyenv())
  injector$shared_env <- new.env(parent = emptyenv())
  injector
}

.default_injector <- .initialize_injector()

.set_injector <- function(injector) {
  .modulr_env$injector <- injector
}

.set_injector(injector = .default_injector)


#' With Injector.
#'
#' Temporarily use a given injector.
#'
#' @param injector An injector (R environment).
#' @param code Any object. Code to execute with the temporary injector.
#'
#' @return The result of the evaluation of the \code{code} argument.
#'
#' @seealso \code{\link[withr]{withr}} for examples of 'with_' methods.
#'
#' @export
with_injector <- function(injector, code) {
  injector_ <- set_injector(injector)
  on.exit(set_injector(injector_))
  force(code)
}

#' @title Create, Set, and Get Injectors (Modulr Internal States).
#'
#' @description Create an new injector, set and get the current injector, and
#'   get the default injector.
#'
#' @return Every function returns an injector (R environment).
#'
#' @details
#'
#' An injector essentially carries an internal modulr state. Technically, it is
#' is an R environment containing every piece of information needed by modulr to
#' reflect the module definitions, the dependencies between them, the
#' configurations, and all the associated metadata. As a PORO (Plain Old R
#' Object), an injector can be stored to disk with the session data, or shared
#' between Alice and Bob, for instance.
#'
#' When the modulr package is loaded, a default injector is created. This
#' injector is returned by the \code{get_default_injector} function.
#'
#' @section Warning: Setting an injector from within a module is not allowed and
#'   results in an error.
#'
#' @seealso \code{\link{.SharedEnv}}, \code{\link{define}},
#'   \code{\link{list_modules}}, \code{\link{make}}, and \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() NULL)
#' injector <- new_injector()
#' previous_injector <- set_injector(injector)
#' define("bar", NULL, function() NULL)
#' lsmod()
#' set_injector(previous_injector)
#' lsmod()
#'
#' \dontrun{
#' .Last <- function() {
#'  # Bind the current injector (internal modulr state) to the environment.
#'  injector <- get_injector()
#' }
#' quit(save = "yes")}
#'
#' reset()
#' define("foo", NULL, function() "Hi Bob!")
#' ## Alice saves its injector and sends it to ...
#' saveRDS(get_injector(), file = "injector.R")
#' ## ... Bob who restores it.
#' injector <- readRDS("injector.R")
#' set_injector(injector)
#' make("foo")
#'
#' @name injector
#' @aliases new_injector set_injector get_injector get_default_injector
NULL

#' @rdname injector
#' @export
new_injector <- function() {

  injector <- .initialize_injector()

  injector_ <- get_injector()

  .set_injector(injector = injector)

  if (!.is_defined("modulr")) define_modulr()

  root_config$set(unique(eval(DEFAULT_ROOT_CONFIG)))

  .set_injector(injector = injector_)

  injector$get <- function(...) with_injector(injector, make(...))
  injector$provider <- function(...) with_injector(injector, define(...))
  injector$touch <- function(...) with_injector(injector, touch(...))
  injector$reset <- function(...) with_injector(injector, reset(...))

  injector
}

#' @rdname injector
#' @param injector An injector returned by \code{new_injector}.
#' @export
set_injector <- function(injector = new_injector()) {

  if (.is_called_from_within_module()) {
    stop("set_injector is called from within a module.", call. = FALSE)
  }

  assert_that(is.environment(injector))

  on.exit(gc())

  injector_ <- get_injector()
  .set_injector(injector = injector)

  injector_
}

.default_injector$get <- function(...) {
  with_injector(get_default_injector(), make(...))
}

.default_injector$get <- function(...) {
  with_injector(get_default_injector(), define(...))
}

#' @rdname injector
#' @export
set_default_injector <- function() {
  set_injector(.default_injector)
}

#' @rdname injector
#' @export
get_injector <- function() {
  .modulr_env$injector
}

#' @rdname injector
#' @export
get_default_injector <- function() {
  .default_injector
}

# the base::get0 function exsists only since R 3.2
.get_0 <- function(var, ..., ifnotfound = NULL) {
  .get_0_ <- function() if (exists(var, ...)) get(var, ...) else ifnotfound
  do.call(.get_0_, args = list(), envir = parent.frame(2L))
}

.dir_exists <- function(file) {
  isTRUE(file.info(file)[1L, "isdir"])
}

.deprecated <- function(
  new, package = NULL, msg,
  old = as.character(sys.call(sys.parent()))[1L],
  immediate. = F) {
  if (isTRUE(getOption("modulr.ignore_deprecated"))) return()
  msg <- if (missing(msg)) {
    msg <- c()
    bc <- get_breadcrumbs(verbose = FALSE)
    if (length(bc) > 1L)
      msg <- gettextf("modulr breadcrumbs: %s\n",
                      paste(gettextf("'%s'", bc), collapse = " > "))
    msg <- c(msg, gettextf("'%s' is deprecated.\n", old))
    if (!missing(new))
      msg <- c(msg, gettextf("Use '%s' instead.\n", new))
    c(
      msg,
      if (!is.null(package))
        gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").",
                 package)
      else
        gettext("See help(\"Deprecated\")"))
  }
  else as.character(msg)
  warning(
    paste(msg, collapse = ""),
    call. = FALSE, domain = NA,
    immediate. = immediate.)
}

#' @name .SharedEnv
#' @rdname shared_env
#' @aliases .SharedEnv sharedenv
#' @title Shared Environment.
#' @description A shared environment between modules.
#' @usage
#' .SharedEnv
#' @details
#' The shared environment is bound to the injector being used.
#' @section Warning:
#' Do not assign to \code{.SharedEnv} in the workspace, because this will always
#' mask the object of the same name in \code{package:modulr}.
#' @return The shared environment.
#' @seealso \code{\link{injector}}.
#' @examples
#' .SharedEnv$foo <- "foo"
#' "foobar" %provides% { function() print(.SharedEnv$foo) }
#' make()()
#' .SharedEnv$foo <- "bar"
#' make()()
NULL

#' @name .Last.name
#' @aliases .Last.name
#' @rdname last_name
#' @title Name of Last Used Module.
#' @description The name of the last used module.
#' @usage .Last.name
#' @details
#' A module is considered used last if it has just been:
#' \itemize{
#'  \item defined or imported with a name without special meaning and not
#'    reserved (see \code{\link{define}}),
#'  \item made or touched with a non-reserved name.
#' }
#' Other actions on modules do not alter the value of \code{.Last.name}.
#' @section Warning:
#' Do not assign to \code{.Last.name} in the workspace, because this will always
#' mask the object of the same name in \code{package:modulr}.
#' @return The name of the last used module.
#' @seealso \code{\link{define}}.
#' @examples
#' define("foo", NULL, function() NULL)
#' .Last.name
#' define("bar/test", NULL, function() NULL)
#' .Last.name
#' make("bar/test")
#' .Last.name
NULL

#' @name module_metadata
#' @aliases .__name__ .__file__ .__version__ .__namespace__ .__initials__
#'   .__final__
#' @rdname module_metadata
#' @title Module Metadata.
#' @description Access module metadata.
#' @usage .__name__
#' @details When modulr loads a module file, it assigns the module's name to
#'   \code{.__name__}. A module file can discover whether or not it is running
#'   in the main scope by checking if \code{.__name__} has value
#'   \code{"__main__"}. This allows a common idiom for conditionally executing
#'   code in a module file when it is run as a script (see example). It is
#'   mainly useful when one wants to write a module file which can be executed
#'   directly as a script and alternatively declared as a dependency and used by
#'   other modules.
#' @section Warning: Do not assign to any metadata in the workspace, because
#'   this will always mask the object of the same name in \code{package:modulr}.
#' @return The name of the current module scope.
#' @seealso \code{\link{define}} and \code{\link{make}}.
#' @examples
#' # script.R
#' "script" %provides% { cat("Hello World\n"); print(.__name__) }
#' if (.__name__ == "main") make()
#' # EOF
#' \dontrun{source("script.R")}
#' make("script")
NULL

#' @name .__version__
#' @rdname module_metadata
#' @usage .__version__
NULL

#' @name .__namespace__
#' @rdname module_metadata
#' @usage .__namespace__
NULL

#' @name .__initials__
#' @rdname module_metadata
#' @usage .__initials__
NULL

#' @name .__final__
#' @rdname module_metadata
#' @usage .__final__
NULL

#' @name .__file__
#' @rdname module_metadata
#' @usage .__file__
NULL

#' @name .__path__
#' @rdname module_metadata
#' @usage .__path__
NULL

#' @rdname shared_env
#' @export
sharedenv <- function() {
  .modulr_env$injector$shared_env
}

globalVariables(c(
  ".Last.name",
  ".Last.packages_manifest",
  ".SharedEnv",
  ".__name__"))

if (utils::packageVersion("assertthat") >= package_version("0.1.0.99")) {
  assert_that <- assertthat::assert_that
} else {
  assert_that <- function(..., msg) {
    do.call(assertthat::assert_that, list(...), envir = parent.frame())
  }
}

#' Set and Get Verbosity Level.
#'
#' Set and get the global verbosity level.
#'
#' @param level A scalar (integer vector of length one),
#'  possibly \code{-Inf} or \code{+Inf}.
#'
#' @details
#'
#' Messages are generated, accordingly to the following levels:
#'
#' \describe{
#' \item{Level 0.}{None.}
#' \item{Level 1.}{Operations modifying the internal state.}
#' \item{Level 2 (default).}{All operations.}
#' \item{Level \code{+Inf}.}{Debugging informations.}
#' }
#'
#' @seealso \code{\link{define}}, \code{\link{make}}, \code{\link{reset}}, and
#'   \code{\link{touch}}.
#'
#' @examples
#' reset()
#' set_verbosity(+Inf)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' reset()
#' set_verbosity(2)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' reset()
#' set_verbosity(1L)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' reset()
#' set_verbosity(0L)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' @export
set_verbosity <- function(level = 2L) {

  assertthat::assert_that(assertthat::is.scalar(level))
  olevel <- get_verbosity()
  .modulr_env$injector$verbosity <- level
  invisible(olevel)

}

#' @rdname set_verbosity
#' @export
get_verbosity <- function() {

  .get_0("verbosity", envir = .modulr_env$injector, ifnotfound = 2L)

}

#' With verbosity.
#'
#' Temporarily change the verbosity.
#'
#' @inheritParams set_verbosity
#' @param code Any object. Code to execute in the temporary environment.
#'
#' @return The result of the evaluation of the \code{code} argument.
#'
#' @details
#'
#' Verbosity is temporarily changed.
#'
#' @seealso \code{\link[withr]{withr}} for examples of 'with_' methods,
#'   \code{\link{get_verbosity}}, and \code{\link{set_verbosity}}.
#'
#' @examples
#' define("foo", NULL, { "bar" })
#' with_verbosity(-Inf, {
#'   make("foo")
#' })
#'
#' @export
with_verbosity <- function(level, code) {

  # nocov start
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("package 'devtools' is needed for this function to work. ",
         "Please install it.",
         call. = FALSE)
  }
  # nocov end

  old <- set_verbosity(level = level)
  on.exit(set_verbosity(old))
  force(code)

}

DEFAULT_DEPARSE_MAX_LINES_IN_PIPES <- 2L

DEFAULT_ROOT_CONFIG <- quote(c(
  ".",
  "modules",
  "module",
  "libs",
  "lib",
  if (requireNamespace("rstudioapi", quietly = TRUE))
    tryCatch({
      file.path(rstudioapi::getActiveProject(), c(
        ".",
        "modules",
        "module",
        "libs",
        "lib"
      ))
    }, error = function(...) NULL),
  if (nzchar(Sys.getenv("HOME")))
    file.path(Sys.getenv("HOME"), ".modulr"),
  if (nzchar(Sys.getenv("R_HOME")))
    file.path(Sys.getenv("R_HOME"), "etc", "modulr.d")
  ))

PRAISE <- c(
  "Outstanding",
  "I'm so proud of you",
  "You're great",
  "Wonderful",
  "Great job",
  "Terrific",
  "You're super",
  "Great smile",
  "You're the best",
  "Perfect",
  "Way to go",
  "Good for you",
  "Fabulous",
  "You're delightful",
  "You did it",
  "You make me happy",
  "You're an inspiration",
  "Great",
  "Excellent",
  "Thanks for sharing",
  "Super work",
  "Marvelous",
  "I trust you",
  "You're getting there",
  "Fantastic",
  "You're special",
  "You deserve a star",
  "Very good",
  "I'm impressed",
  "Exceptional",
  "Thanks for caring",
  "You're very responsible",
  "You're a joy to be around",
  "You're tops",
  "Nice work",
  "You're a gem",
  "Dynamite",
  "Hurray for you",
  "You're so creative",
  "You're a champ",
  "Beautiful",
  "Great imagination",
  "You'll get it",
  "Keep up the good work",
  "You're very brave",
  "Good sport",
  "Sounds great",
  "You've got what it takes",
  "You're #1",
  "How clever",
  "How thoughtful",
  "You're on the mark",
  "You're the greatest",
  "I've got faith in you",
  "Well done",
  "How artistic",
  "What careful work",
  "Exceptional",
  "That's neat",
  "Wonderful imagination",
  "You're right",
  "You brighten my day",
  "Delightful idea",
  "Super job")
