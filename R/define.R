# Compute a SHA-1 digest of an R object
.hash <- function(object) {

  digest::digest(object, "sha1")

}

#' In order to know if a module definition has changed,
#' we compute a digest of it with a cryptographic hash.
#' @export
# TODO: write documentation
get_digest <- function(name, load = FALSE) {

  .message_meta(sprintf("Entering get_digest() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(assertthat::is.flag(load))

  if(.is_undefined(name) & load) {

    if(.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)
  }

  assert_that(.is_defined(name))

  .hash(c(
    deparse(modulr_env$register[[c(name, "dependencies")]]),
    deparse(modulr_env$register[[c(name, "factory")]])))

}

#' Define a Module.
#'
#' Define or redefine a module by name, dependencies, and factory.
#'
#' @param name A string (character vector of lenght one).
#'
#'   A module name can contain letters, figures and some special characters,
#'   namely \code{_}, \code{-}, and \code{/}. The latter is a namespace
#'   separator.
#'
#'   The name "modulr" corresponds to a special module and is therefore
#'   reserved.
#'
#' @param dependencies A (preferably named) list of strings or NULL.
#'
#'   Elements of the list of dependencies designate modules by their name.
#'
#' @param factory A function.
#'
#'   The formals of the factory must coincide with the list of dependencies.
#'
#' @return A wrapper function around a make call for the defined module.
#'
#' @details
#'
#' The definition of a module can be done on-the-fly (in the console or
#' evaluating a script), from a file which is persistent on a disk or at a given
#' remote URL via the http(s) protocol. These three ways of defining modules
#' have their specificities.
#'
#' \describe{
#' \item{On-The-Fly Method}{
#' This is the most direct method to define or redefine a module. This is also
#' the most volatile since the lifespan of the module is limited to the R
#' session only. When a new module is defined, the internal state of the package
#' is modified to record its name, dependencies and factory. Some other useful
#' metadata are also recorded, like timestamps, various flags and counters, and
#' a digest. When an existing module is redefined, the internal state is updated
#' accordingly, unless no change is detected by digests comparison. No other
#' side-effect occurs during the definition process and notably, the actual
#' evaluation of the factory takes place uniquely during a \code{\link{make}}
#' call.
#' }
#'
#' \item{Persistent Method}{ This is the natural
#' method to choose when a module is intended to be reused. In such a case, the
#' definition takes place in a dedicated file, which name is closely related to
#' the module's name.
#'
#' As a file \code{/home/user/readme.txt} is composed of a path
#' \code{/home/user} and a file name \code{readme.txt}, a module name
#' \code{vendor/tool/swissknife} is similarily composed of a namespace
#' \code{vendor/tool} and a local name \code{swissknife}. For modulr to find
#' this module, it is sufficient to store its definition in an R or R Markdown
#' file named \code{swissknife.R[md]} (R files have precedence over Rmd's),
#' laid out on disk in the \code{vendor/tool} path, relative to the modulr
#' root directory (see \code{\link{root_config}}).
#'
#' \itemize{
#' \item \code{vendor/}
#' \itemize{
#' \item \code{tool/}
#' \itemize{
#' \item \code{swissknife.R},
#'
#' contains the "vendor/tool/swissknife" definition.}}}
#'
#' Each time the definition of a module is needed, modulr resolves its name into
#' a file location by applying the following configurable rules.
#'
#' \enumerate{
#' \item
#' The \code{\link{root_config}} accessor acts at the \emph{filesystem level} by
#' specifying the root directory, relative to which all paths are expressed. For
#' instance, \code{root_config$set("./lib")} tells modulr that all modules are
#' to be found in \code{lib}, in the R working directory. The directory path can
#' be relative (e.g. \code{./lib}) or absolute (e.g. \code{/home/user/lib}).
#' By default, modulr looks in turn into the following directories
#' \code{"./module"}, \code{"./modules"}, \code{"./lib"}, \code{"./libs"},
#' and \code{"."}.
#'
#' \item The \code{\link{paths_config}} accessor acts at a \emph{namespace
#' level} by mapping a specific namespace to a dedicated path, relative to the
#' root directory. For instance, \code{paths_config$set("vendor" =
#' "third_parties/vendor")} will map the \code{vendor/great_module} to
#' the \code{third_parties/vendor/great_module.R[md]} path, relative to the
#' root directory.
#' \itemize{
#' \item \code{third_parties}
#'
#' is a dedicated container for third-parties modules.
#' \itemize{
#' \item \code{vendor}
#' \itemize{
#' \item \code{great_module.R}
#'
#' contains the "vendor/great_module" definition.
#' }
#' }
#' }
#' \item The \code{\link{maps_config}} accessor acts at a
#' \emph{module level} by substituting specific dependencies within the scope of
#' a given module only. This is especially useful in a situation where a
#' dependency has been replaced by a newer version, but a module still needs to
#' rely on the older one. For instance, \code{maps_config$set("foo/bar" =
#' list("vendor/great_module" = "vendor/old_great_module"))} tells
#' modulr that for the module \code{foo/bar} only, the dependency
#' \code{vendor/great_module} must be replaced by
#' \code{vendor/old_great_module}. }
#'
#' \itemize{
#' \item \code{foo}
#' \itemize{
#' \item \code{bar.R}
#'
#' depends on \code{vendor/great_module} by definition, but will be replaced by
#' \code{vendor/old_great_module} when needed.
#' }
#' \item \code{vendor}
#' \itemize{
#' \item \code{great_module.R}
#'
#' serves all modules that depend on it, except \code{foo/bar}.
#' \item \code{old_great_module}
#'
#' serves \code{foo/bar} only.
#' }
#' }
#'
#' These rules are applied in reverse order, substituting dependencies first,
#' then mapping namespaces and finally expressing the absolute path, relative to
#' the modulr root directory.
#'
#' } \item{Remote Method}{Third item} }
#'
#' @export
# TODO: write the documentation
define <- function(name, dependencies, factory) {

  .message_meta(sprintf("Entering define() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("define is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.string(name),
    is.function(factory))

  assert_that(
    .is_regular(name) | .is_reserved(name),
    msg = "module name is not regular nor reserved.")

  assert_that(
    is.null(dependencies) || is.list(dependencies),
    is.function(factory)
    )

  assert_that(
    is.null(dependencies) || (
      setequal(names(dependencies), names(formals(factory))) || (
        assertthat::are_equal(length(dependencies),
                              length(formals(factory))) &&
          is.null(names(dependencies)))),
    msg = "dependencies and formals are not matching.")

  if(is.null(dependencies)) dependencies <- list()

  timestamp <- Sys.time()

  if(.is_undefined(name)) {

    .message_meta(
      sprintf("Defining '%s' ...", name), {
        modulr_env$register[[name]] <- list()
        modulr_env$register[[c(name, "name")]] <- name
        modulr_env$register[[c(name, "dependencies")]] <- dependencies
        modulr_env$register[[c(name, "factory")]] <- factory
        modulr_env$register[[c(name, "digest")]] <- .hash(c(
          deparse(dependencies),
          deparse(factory)))
        modulr_env$register[[c(name, "instance")]] <- NULL
        modulr_env$register[[c(name, "instanciated")]] <- F
        modulr_env$register[[c(name, "calls")]] <- 0
        modulr_env$register[[c(name, "duration")]] <- NA_integer_
        modulr_env$register[[c(name, "first_instance")]] <- T
        modulr_env$register[[c(name, "timestamp")]] <- timestamp
        modulr_env$register[[c(name, "created")]] <- timestamp

    },
    verbosity = ifelse(.is_regular(name), 2, 3))



  } else if(.is_regular(name)) {

    previous_digest <- modulr_env$register[[c(name, "digest")]]
    digest <- .hash(c(
      deparse(dependencies),
      deparse(factory)))
    if(digest != previous_digest) {
      .message_meta(sprintf("Re-defining '%s' ...", name), {
        modulr_env$register[[c(name, "dependencies")]] <- dependencies
        modulr_env$register[[c(name, "factory")]] <- factory
        modulr_env$register[[c(name, "digest")]] <- digest
        modulr_env$register[[c(name, "instance")]] <- NULL
        modulr_env$register[[c(name, "instanciated")]] <- F
        modulr_env$register[[c(name, "calls")]] <- 0
        modulr_env$register[[c(name, "duration")]] <- NA_integer_
        modulr_env$register[[c(name, "first_instance")]] <- F
        modulr_env$register[[c(name, "timestamp")]] <- timestamp
      },
      verbosity = 1)

    }
  } else {
    assert_that(.is_regular(name))
  }

  if(.is_regular_core(name))
    modulr_env$.Last.name <- name # Exclude Linting

  invisible(function(...) make(name, ...))

}

#' Get module factory.
#'
#' @export
# TODO: write documentation
get_factory <- function(name, load = FALSE) {

  .message_meta(sprintf("Entering get_factory() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(assertthat::is.flag(load))

  if(.is_undefined(name) & load) {

    if(.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)
  }

  assert_that(.is_defined(name))

  modulr_env$register[[c(name, "factory")]]

}

#' Remove all module definitions.
#'
#' @export
# TODO: write documentation
reset <- function(all = FALSE, verbose = TRUE) {

  .message_meta("Entering reset() ...",
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("reset is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.flag(all),
    assertthat::is.flag(verbose))

  if(verbose)
    .message_meta("Resetting modulr state ... ", verbosity = 2)

  modulr_env$register <- list()
  modulr_env$config <- list(modules = list())
  modulr_env$verbosity <- 2
  modulr_env$.Last.name <- NULL # Exclude Linting
  if(all)
    modulr_env$stash <- list()

  .define_modulr()

  root_config$set(c("module", "modules", "lib", "libs", "."))

  invisible()

}

#' Undefine module.
#'
#' @export
# TODO: write documentation

undefine <- function(name) {

  .message_meta(sprintf("Entering undefine() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("undefine is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(.is_defined_regular(name))

  .message_meta(sprintf("Undefining '%s' ... ", name), verbosity = 2)

  modulr_env$register[[name]] <- NULL

  invisible()

}

#' Touch module.
#'
#' @export
# TODO: write documentation
touch <- function(name) {

  .message_meta(sprintf("Entering touch() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("touch is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(.is_defined_regular(name))

  .message_meta(sprintf("Touching '%s' ...", name), verbosity = 2)

  modulr_env$register[[c(name, "instance")]] <- NULL
  modulr_env$register[[c(name, "instanciated")]] <- F
  modulr_env$register[[c(name, "calls")]] <- 0
  modulr_env$register[[c(name, "duration")]] <- NA_integer_
  modulr_env$register[[c(name, "timestamp")]] <- Sys.time()

  if(.is_regular(name))
    modulr_env$.Last.name <- name # Exclude Linting

  module_option(name)$unset()

  invisible()

}

#' Syntactic sugar to require dependencies, to be used in conjunction with \%provides\%.
#'
#' @export
`%requires%` <- function(lhs, rhs) {

  assert_that(.is_regular(lhs))

  assert_that(
    is.list(rhs) || is.null(rhs),
    msg = "right-hand side of `%requires%` is not a list of dependencies."
  )

  list(name = lhs, dependencies = rhs)

}

#' Syntactic sugar to provide a factory, can be used in conjunction with \%requires\%.
#'
#' @export
`%provides%` <- function(lhs, rhs) {

  if(.is_called_from_within_module()) {
    warning("`%provides%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    is.function(rhs),
    msg = "right-hand side of `%provides%` is not a factory."
    )

  assert_that(
    assertthat::is.string(lhs) || (
      is.list(lhs) &&
        setequal(names(lhs), c("name", "dependencies"))),
    msg = paste0("left-hand side of `%provides%` ",
                 "is not a module name or a list of dependencies.")
  )

  if(is.list(lhs)) {
    name <- lhs[["name"]]
    dependencies <- lhs[["dependencies"]]
  } else {
    name <- as.character(lhs)
    dependencies <- list()
  }
  factory <- rhs

  do.call(
    define,
    args =
      list(name = name,
           dependencies = dependencies,
           factory = factory),
    envir = parent.frame())

}
