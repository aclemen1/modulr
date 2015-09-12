# Compute a SHA-1 digest of an R object
.hash <- function(object) {

  digest::digest(object, "sha1")

}

#' In order to know if a module definition has changed,
#' we compute a digest of it with a cryptographic hash.
#' @export
# TODO: write documentation
get_digest <- function(name, load = F) {

  .message_meta(sprintf("Entering get_digest() for '%s' ...", name),
                verbosity = +Inf)

  assertthat::assert_that(assertthat::is.flag(load))

  if(.is_undefined(name) & load) {

    if(.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = F, immediate. = T)
    }

    load_module(name)
  }

  assertthat::assert_that(.is_defined(name))

  register <- .internals()$register

  module <- register[[name]]

  .hash(c(
    deparse(module$dependencies),
    deparse(module$factory)))

}

#' Define a module.
#'
#' @param name  the module name, given as a character string.
#' @param dependencies  the list of module dependencies, given as module names.
#' @param factory the factory function.
#' @return a wrapper function around the module instanciation.
#' @examples
#' # define "module_1"
#' define("module_1", list(), function() {
#'  message("Module 1"); "value 1"})
#'
#' # define "module_2"
#' m2 <- define("module_2", list("module_1"), function(m1) {
#'  message("Module 2 with one dependency"); paste(m1, "value 2")})
#'
#' # instanciate "module_2"
#' m2()
#' @export
# TODO: write the documentation
define <- function(name, dependencies, factory) {

  .message_meta(sprintf("Entering define() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("define is called from within a module.",
            call. = F, immediate. = T)
  }

  assertthat::assert_that(
    assertthat::is.string(name),
    is.function(factory))

  assertthat::assert_that(
    is.null(dependencies) || is.list(dependencies),
    is.function(factory)
    )

  assertthat::assert_that(
    is.null(dependencies) || (
      setequal(names(dependencies), names(formals(factory))) || (
        assertthat::are_equal(length(dependencies),
                              length(formals(factory))) &&
          is.null(names(dependencies)))),
    msg = "dependencies and formals are not matching.")

  if(is.null(dependencies)) dependencies <- list()

  timestamp <- Sys.time()

  register <- .internals()$register

  if(.is_undefined(name)) {

    if(.is_regular(name))
      .message_meta(sprintf("Defining '%s' ...", name), verbosity = 2)

    register[[name]]$name <- name
    register[[name]]$dependencies <- dependencies
    register[[name]]$factory <- factory
    register[[name]]$digest <- .hash(c(
      deparse(dependencies),
      deparse(factory)))
    register[[name]]$instance <- NULL
    register[[name]]$instanciated <- F
    register[[name]]$calls <- 0
    register[[name]]$duration <- NA_integer_
    register[[name]]$first_instance <- T
    register[[name]]$timestamp <- timestamp
    register[[name]]$created <- timestamp

  } else if(.is_regular(name)) {

    previous_digest <- register[[name]]$digest
    digest <- .hash(c(
      deparse(dependencies),
      deparse(factory)))
    if(digest != previous_digest) {

      .message_meta(sprintf("Re-defining '%s' ...", name), verbosity = 1)

      register[[name]]$dependencies <- dependencies
      register[[name]]$factory <- factory
      register[[name]]$digest <- digest
      register[[name]]$instance <- NULL
      register[[name]]$instanciated <- F
      register[[name]]$calls <- 0
      register[[name]]$duration <- NA_integer_
      register[[name]]$first_instance <- F
      register[[name]]$timestamp <- timestamp

    }
  } else {
    assertthat::assert_that(.is_regular(name))
  }

  assign("register", register, pos = modulr_env)
  if(.is_regular_core(name))
    assign(".Last.name", name, pos = modulr_env) # Exclude Linting

  invisible(function(...) make(name, ...))

}

#' Get module factory.
#'
#' @export
# TODO: write documentation
get_factory <- function(name, load = F) {

  .message_meta(sprintf("Entering get_factory() for '%s' ...", name),
                verbosity = +Inf)

  assertthat::assert_that(assertthat::is.flag(load))

  if(.is_undefined(name) & load) {

    if(.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = F, immediate. = T)
    }

    load_module(name)
  }

  assertthat::assert_that(.is_defined(name))

  .internals()$register[[name]]$factory

}

#' Remove all module definitions.
#'
#' @export
# TODO: write documentation
reset <- function(all = F, verbose = T) {

  .message_meta("Entering reset() ...",
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("reset is called from within a module.",
            call. = F, immediate. = T)
  }

  assertthat::assert_that(
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
            call. = F, immediate. = T)
  }

  assertthat::assert_that(.is_defined_regular(name))

  register <- .internals()$register

  .message_meta(sprintf("Undefining '%s' ... ", name), verbosity = 2)

  register[[name]] <- NULL

  assign("register", register, pos = modulr_env)

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
            call. = F, immediate. = T)
  }

  assertthat::assert_that(.is_defined_regular(name))

  register <- .internals()$register

  .message_meta(sprintf("Touching '%s' ...", name), verbosity = 2)

  register[[name]]$instance <- NULL
  register[[name]]$instanciated <- F
  register[[name]]$calls <- 0
  register[[name]]$duration <- NA_integer_
  register[[name]]$timestamp <- Sys.time()

  assign("register", register, pos = modulr_env)
  if(.is_regular(name))
    assign(".Last.name", name, pos = modulr_env) # Exclude Linting

  module_option(name)$unset()

  invisible()

}

#' Syntactic sugar to require dependencies, to be used in conjunction with \%provides\%.
#'
#' @export
`%requires%` <- function(lhs, rhs) {

  assertthat::assert_that(.is_regular(lhs))

  assertthat::assert_that(
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
            call. = F, immediate. = T)
  }

  assertthat::assert_that(
    is.function(rhs),
    msg = "right-hand side of `%provides%` is not a factory."
    )

  assertthat::assert_that(
    assertthat::is.string(lhs) || (
      is.list(lhs) &&
        setequal(names(lhs), c("name", "dependencies"))),
    msg = paste0("left-hand side of `%provides%` ",
                 "is not a module name or a list of dependencies.")
  )

  if(is.list(lhs)) {
    name <- lhs$name
    dependencies <- lhs$dependencies
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
