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

  assertthat::assert_that(assertthat::is.flag(load))

  if(.is_undefined(name) & load) {

    if(.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)
  }

  assertthat::assert_that(.is_defined(name))

  factory <- modulr_env$register[[c(name, "factory")]]
  if(!is.null(modulr_env$register[[c(name, "compressed")]]))
    factory <- .decompress(
      factory, modulr_env$register[[c(name, "compressed")]])

  .hash(c(
    deparse(modulr_env$register[[c(name, "dependencies")]]),
    deparse(factory)))

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
define <- function(name, dependencies, factory, compress = "gzip") {

  .message_meta(sprintf("Entering define() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("define is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assertthat::assert_that(
    is.null(compress) ||
      compress %in% c("gzip", c("gzip", "bzip2", "xz")),
    msg = paste0("compress can take the following values: ",
                 "NULL, \"gzip\", \"bzip2\" or \"xz\".")
    )

  assertthat::assert_that(
    assertthat::is.string(name),
    is.function(factory))

  assertthat::assert_that(
    .is_regular(name) | .is_reserved(name),
    msg = "module name is not regular nor reserved.")

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

  store <- function(factory) {
    if(!is.null(compress)) {
      if(.is_regular(name))
        .message_meta(sprintf("Compressing definition factory ... "),
                      verbosity = 2)
      pack <- .compress(factory, type = compress)
      factory_size <- utils::object.size(factory)
      pack_size <- utils::object.size(pack)
      if(.is_regular(name))
        .message_meta(
          sprintf(
            "Compression factor: %.0fx (%s instead of %s)",
            factory_size / pack_size,
            format(pack_size, units = "auto"),
            format(factory_size, units = "auto")),
          verbosity = 2)
      pack
    } else factory
  }

  if(.is_undefined(name)) {

    .message_meta(
      sprintf("Defining '%s' ...", name), {
        modulr_env$register[[name]] <- list()
        modulr_env$register[[c(name, "name")]] <- name
        modulr_env$register[[c(name, "dependencies")]] <- dependencies
        modulr_env$register[[c(name, "compressed")]] <- compress
        modulr_env$register[[c(name, "factory")]] <- store(factory)
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
    if(digest != previous_digest ||
         ifelse(is.null(compress), "", compress) !=
         ifelse(is.null(modulr_env$register[[c(name, "compressed")]]),
                "", modulr_env$register[[c(name, "compressed")]])) {

      .message_meta(sprintf("Re-defining '%s' ...", name), {
        modulr_env$register[[c(name, "dependencies")]] <- dependencies
        modulr_env$register[[c(name, "compressed")]] <- compress
        modulr_env$register[[c(name, "factory")]] <- store(factory)
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
    assertthat::assert_that(.is_regular(name))
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

  assertthat::assert_that(assertthat::is.flag(load))

  if(.is_undefined(name) & load) {

    if(.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)
  }

  assertthat::assert_that(.is_defined(name))

  factory <-
    modulr_env$register[[c(name, "factory")]]
  if(!is.null(modulr_env$register[[c(name, "compressed")]]))
    factory <- .decompress(
      factory, type = modulr_env$register[[c(name, "compressed")]])

  factory

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
            call. = FALSE, immediate. = TRUE)
  }

  assertthat::assert_that(.is_defined_regular(name))

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

  assertthat::assert_that(.is_defined_regular(name))

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
            call. = FALSE, immediate. = TRUE)
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
