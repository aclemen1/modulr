# Compute a SHA-1 digest of an R object
.hash <- function(object) {

  digest::digest(object, "sha1")

}

#' In order to know if a module definition has changed,
#' we compute a signature of it with a cryptographic hash.
#' @export
# TODO: write documentation
get_signature <- function(name) {

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

  assertthat::assert_that(
    assertthat::is.string(name),
    is.function(factory),
    is.null(dependencies) || (
      is.list(dependencies) && (
        setequal(names(dependencies), names(formals(factory))) || (
          assertthat::are_equal(length(dependencies),
                                length(formals(factory))) &&
            is.null(names(dependencies))))))

  if(is.null(dependencies)) dependencies <- list()

  timestamp <- Sys.time()

  register <- .internals()$register

  if(.is_undefined(name)) {

    if(.is_regular(name))
      .message_meta(sprintf("defining [%s] ...", name))

    register[[name]]$name <- name
    register[[name]]$dependencies <- dependencies
    register[[name]]$factory <- factory
    register[[name]]$signature <- .hash(c(
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

    previous_signature <- register[[name]]$signature
    signature <- .hash(c(
      deparse(dependencies),
      deparse(factory)))
    if(signature != previous_signature) {

      .message_meta(sprintf("re-defining [%s] ...", name))

      register[[name]]$dependencies <- dependencies
      register[[name]]$factory <- factory
      register[[name]]$signature <- signature
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
    assign(".Last.name", name, pos = modulr_env)

  invisible(function(...) make(name, ...))

}

#' Get module factory.
#'
#' @export
# TODO: write documentation
get_factory <- function(name) {

  load_module(name)

  assertthat::assert_that(.is_defined(name))

  .internals()$register[[name]]$factory

}

#' Remove all module definitions.
#'
#' @export
# TODO: write documentation
reset <- function() {

  .message_meta("resetting package")

  .onLoad()

  invisible()

}

#' Undefine module.
#'
#' @export
# TODO: write documentation

undefine <- function(name) {

  assertthat::assert_that(.is_defined_regular(name))

  register <- .internals()$register

  .message_meta(sprintf("undefining [%s]", name))

  register[[name]] <- NULL

  assign("register", register, pos = modulr_env)

  invisible()

}

#' Touch module.
#'
#' @export
# TODO: write documentation
touch <- function(name) {

  assertthat::assert_that(.is_defined_regular(name))

  register <- .internals()$register

  .message_meta(sprintf("touching [%s]", name))

  register[[name]]$instance <- NULL
  register[[name]]$instanciated <- F
  register[[name]]$calls <- 0
  register[[name]]$duration <- NA_integer_
  register[[name]]$timestamp <- Sys.time()

  assign("register", register, pos = modulr_env)
  if(.is_regular(name))
    assign(".Last.name", name, pos = modulr_env)

  module_option(name)$unset()

  invisible()

}

#' Syntactic sugar to require dependencies, to be used in conjunction with \%provides\%.
#'
#' @export
`%requires%` = function(lhs, rhs) {

  assertthat::assert_that(
    assertthat::is.string(lhs),
    is.list(rhs) || is.null(rhs)
    )

  list(name = lhs, dependencies = rhs)

}

#' Syntactic sugar to provide a factory, can be used in conjunction with \%requires\%.
#'
#' @export
`%provides%` = function(lhs, rhs) {

  assertthat::assert_that(
    is.function(rhs),
    assertthat::is.string(lhs) || (
      is.list(lhs) &
        setequal(names(lhs), c("name", "dependencies"))))

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

