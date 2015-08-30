# In order to know if a module definition has changed,
# we compute a signature of it with a cryptographic hash.
.signature <- function(name) {

  register <- get("register", pos = modulr_env)

  module <- register[[name]]

  if(!is.null(module)) {
    return(digest::digest(c(
      deparse(module$dependencies),
      deparse(module$factory)),
      "sha1"))
  }

  invisible(NULL)
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

  if(!(is.character(name) & isTRUE(length(name) == 1)))
     stop("Type mismatch, string expected for name.", call. = F)
  if(!(is.list(dependencies) | is.null(dependencies)))
     stop("Type mismatch, list expected for dependencies.", call. = F)
  if(!is.function(factory))
     stop("Type mismatch, function expected for factory.", call. = F)

  register <- get("register", pos = modulr_env)

  if(is.null(register[[name]])) {
    if(!(name %in% RESERVED_NAMES))
      .message_meta(sprintf("defining [%s] ...",
                           name), level = 1)

    if(is.null(dependencies)) dependencies <- list()
    if(isTRUE(length(dependencies) != length(formals(factory)))) {
      stop("Cardinality mismatch, ",
           "number of dependencies and factory formals expected to be equal.",
           call. = F)
    }

    register[[name]]$name <- name
    register[[name]]$dependencies <- dependencies
    register[[name]]$factory <- factory
    register[[name]]$signature <- digest::digest(c(
      deparse(dependencies),
      deparse(factory)), "sha1")
    register[[name]]$instance <- NULL
    register[[name]]$instanciated <- F
    register[[name]]$first_instance <- T
    register[[name]]$timestamp <- Sys.time()
  } else if(!(name %in% RESERVED_NAMES)) {
    previous_signature <- register[[name]]$signature
    signature <- digest::digest(c(
      deparse(dependencies),
      deparse(factory)), "sha1")
    if(signature != previous_signature) {
      if(!(name %in% RESERVED_NAMES))
        .message_meta(sprintf("re-defining [%s] ...",
                             name), level = 1)

      if(isTRUE(length(dependencies) != length(formals(factory)))) {
        stop("Cardinality mismatch, ",
             "number of dependencies and factory formals expected to be equal.",
             call. = F)
      }

      register[[name]]$dependencies <- dependencies
      register[[name]]$factory <- factory
      register[[name]]$signature <- signature
      register[[name]]$instance <- NULL
      register[[name]]$instanciated <- F
      register[[name]]$first_instance <- F
      register[[name]]$timestamp <- Sys.time()
    }
  } else {
    return(invisible(NULL))
  }

  assign("register", register, pos = modulr_env)

  invisible(function(...) make(name, ...))

}

#' Get module factory.
#'
#' @export
# TODO: write documentation
get_factory <- function(name) {

  if(!(is.character(name) & isTRUE(length(name) == 1)))
    stop("Type mismatch, string expected for name.", call. = F)

  register <- get("register", pos = modulr_env)

  module <- register[[name]]

  if(!is.null(module)) {
    return(module$factory)
  }

  invisible(NULL)
}

#' Remove all module definitions.
#'
#' @export
# TODO: write documentation
reset <- function() {
  .message_meta("resetting package")
  .onLoad()
  invisible(T)
}

#' Undefine module.
#'
#' @export
# TODO: write documentation

undefine <- function(name) {

  if(!(is.character(name) & isTRUE(length(name) == 1)))
    stop("Type mismatch, string expected for name.", call. = F)

  if(!(name %in% RESERVED_NAMES)) {
    .message_meta(sprintf("undefining [%s]", name), level = 1)
    register <- get("register", pos = modulr_env)
    if(is.null(register[[name]])) return(invisible(NULL))
    register[[name]] <- NULL
    assign("register", register, pos = modulr_env)
    return(invisible(T))
  }

  invisible(NULL)
}

#' Touch module.
#'
#' @export
# TODO: write documentation
touch <- function(name) {

  if(!(is.character(name) & isTRUE(length(name) == 1)))
    stop("Type mismatch, string expected for name.", call. = F)

  if(!(name %in% RESERVED_NAMES)) {
    .message_meta(sprintf("touching [%s]", name), level = 1)
    register <- get("register", pos = modulr_env)

    if(is.null(register[[name]])) {
      message_warn("Module not found.")
      return(invisible(NULL))
    }

    register[[name]]$instance <- NULL
    register[[name]]$instanciated <- F

    register[[name]]$timestamp <- Sys.time()
    assign("register", register, pos = modulr_env)

    module_option(name)$unset()

    return(invisible(T))
  }

  invisible(NULL)
}

#' Syntactic sugar to require dependencies, to be used in conjunction with \%provides\%.
#'
#' @export
`%requires%` = function(lhs, rhs) {
  list(name=as.character(lhs), dependencies=as.list(rhs))
}

#' Syntactic sugar to provide a factory, can be used in conjunction with \%requires\%.
#'
#' @export
`%provides%` = function(lhs, rhs) {
  if(!is.function(rhs))
    stop("Type mismatch, factory/function expected on RHS.", call. = F)
  if(is.list(lhs)) {
    if(!identical(names(lhs), c("name", "dependencies")))
      stop("Type mismatch, dependencies/list expected on LHS.", call. = F)
    name <- lhs$name
    dependencies <- lhs$dependencies
  } else {
    name <- as.character(lhs)
    dependencies <- list()
  }
  factory <- rhs
  do.call(define, args = list(name, dependencies, factory),
          envir = parent.frame())
}
