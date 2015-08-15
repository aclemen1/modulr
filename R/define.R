#' Get module defininition.
#'
#' @export
get_definition <- function(name) {
  wrapper = function(debug = F,
                     force = F)
    instanciate(name,
                debug = debug,
                force = force)
  invisible(wrapper)
}

#' Get module factory.
#'
#' @export
get_factory <- function(name) {
  register <- get("register", pos = modulr_env)
  if(!is.null(register[[name]])) {
    return(register[[name]]$factory)
  }
}

# get_definition <- function(name) {
#   wrapper = function(force_reinstanciate = F,
#                      force_redefine_reinstanciate = F,
#                      force_reinstanciate_all = F,
#                      force_redefine_reinstanciate_all = F)
#     instanciate(name,
#                 force_reinstanciate = force_reinstanciate,
#                 force_redefine_reinstanciate = force_redefine_reinstanciate,
#                 force_reinstanciate_all = force_reinstanciate_all,
#                 force_redefine_reinstanciate_all =
#                   force_redefine_reinstanciate_all)
#
#   invisible(wrapper)
# }

.signature <- function(name) {
  register <- get("register", pos = modulr_env)
  module <- register[[name]]
  digest::digest(c(
    deparse(module$dependencies),
    deparse(module$factory)), "sha1")
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

define <- function(name, dependencies, factory) {
  #   if(exists(".__filename__", where = parent.frame())) {
  #     message("Module imported.")
  #     filename = get(".__filename__", pos = parent.frame())
  #     print(filename)
  #   }

  register <- get("register", pos = modulr_env)

  if(is.null(register[[name]])) {
    if(!(name %in% RESERVED_NAMES))
      message_meta(sprintf("defining [%s] ...",
                           name), level = 1)

    if(length(dependencies) != length(formals(factory))) {
      stop("Dependencies mismatch.", call. = F)
    }

    register[[name]]$name <- name
    register[[name]]$dependencies <- dependencies
    register[[name]]$factory <- factory
    register[[name]]$signature <- digest(c(
      deparse(dependencies),
      deparse(factory)), "sha1")
    register[[name]]$instance <- NULL
    register[[name]]$instanciated <- F
    register[[name]]$first_instance <- T
    register[[name]]$reinstanciate_children <- T
  } else {
    previous_signature <- register[[name]]$signature
    signature <- digest(c(
      deparse(dependencies),
      deparse(factory)), "sha1")
    if(signature != previous_signature) {
      if(!(name %in% RESERVED_NAMES))
        message_meta(sprintf("re-defining [%s] ...",
                             name), level = 1)

      if(length(dependencies) != length(formals(factory))) {
        stop("Dependencies mismatch.", call. = F)
      }

      register[[name]]$dependencies <- dependencies
      register[[name]]$factory <- factory
      register[[name]]$signature <- signature
      register[[name]]$instance <- NULL
      register[[name]]$instanciated <- F
      register[[name]]$first_instance <- F
      register[[name]]$reinstanciate_children <- T
    } else {
      register[[name]]$reinstanciate_children <- F
    }
  }

  assign("register", register, pos = modulr_env)

  get_definition(name)
}


#' Remove all module definitions.
#'
#' @export

reset <- function() {
  message_meta("resetting package")
  assign("register", NULL, pos = modulr_env)
  .onLoad()
}

#' Undefine module.
#'
#' @export

undefine <- function(name) {
  if(!(name %in% RESERVED_NAMES)) {
    message_meta(sprintf("undefining [%s]", name), level = 1)
    register <- get("register", pos = modulr_env)
    register[[name]] <- NULL
    assign("register", register, pos = modulr_env)
  }
}

#' Touch module.
#'
#' @export

touch <- function(name) {
  if(!(name %in% RESERVED_NAMES)) {
    message_meta(sprintf("touching [%s]", name), level = 1)
    register <- get("register", pos = modulr_env)
    register[[name]]$signature <- 0
    register[[name]]$reinstanciate_children <- T
    assign("register", register, pos = modulr_env)
    module_option(name)$unset()
  }
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
    stop("Type mismatch, factory needed on RHS.", call. = F)
  if(is.list(lhs)) {
    if(!identical(names(lhs), c("name", "dependencies")))
      stop("Type mismatch, dependencies needed on LHS.", call. = F)
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
