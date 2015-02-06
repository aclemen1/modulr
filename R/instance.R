#' Instanciate a module.
#'
#' @export

instanciate <- function(name, debug = F, force = F) {
  all_dependencies <-
    .define_all_dependent_modules(
      name,
      debug)
  dependency_graph <- .build_dependency_graph(all_dependencies)
  ordered_names <- .topological_sort(dependency_graph)
  if(is.null(ordered_names)) ordered_names <- name
  for(ordered_name in ordered_names) {
    register <- get("register", pos = modulr_env)
    module <- register[[ordered_name]]
    if(is.null(module))
      stop("Module '", ordered_name, "' not defined.")
    reinstanciated_by_parent <- any(unlist(lapply(
      module$dependencies,
      function(name) {
        register[[name]]$reinstanciate_children
      })))
    if(reinstanciated_by_parent) {
      #       if(module$instanciated & ordered_name != name) {
      #         message("Module '", ordered_name, "' needs reinstanciation by parent.")
      #       }
      module$reinstanciate_children <- T
    }
    if(!module$instanciated
       | reinstanciated_by_parent
       | ((debug | force) & ordered_name == name)) {
      if (!(ordered_name %in% RESERVED_NAMES))
        message_open(sprintf("Module '%s'", ordered_name))
      env = new.env()
      assign(".__name__", ordered_name, pos = env)
      if(length(module$dependencies) > 0) {
        args <- lapply(module$dependencies,
                       function(name) register[[name]]$instance)
        # tricky bug solution, see below
        module$instance <- evalq(do.call(
          eval(parse(text=deparse(module$factory))),
          args = args), envir = env)
      } else {
        # the deparse %>% parse %>% eval trick solves the following bug
        # WOKS:
        # module$instance <- evalq(do.call(function() {get("variable", pos = env)}, args = list()), envir = env)
        # DOES NOT WORK:
        # f <- function() {get("variable", pos = env)}
        # module$instance <- evalq(do.call(f, args = list()), envir = env)
        # WORKAROUND:
        # module$instance <- evalq(do.call(eval(parse(text=deparse(f))), args = list()), envir = env)
        module$instance <- evalq(do.call(
          eval(parse(text=deparse(module$factory))),
          args = list()), envir = env)
      }
      if (!(ordered_name %in% RESERVED_NAMES))
        if(module$instanciated | !module$first_instance)
          message_close("Re-Instanciated")
      else
        message_close("Instanciated")
      module$instanciated <- T
      module$first_instance <- F
      register[[ordered_name]] <- module
      assign("register", register, pos = modulr_env)
    }
  }

  for(name in ordered_names) {
    register <- get("register", pos = modulr_env)
    register[[name]]$reinstanciate_children <- F
    assign("register", register, pos = modulr_env)
  }

  get("register", pos = modulr_env)[[name]]$instance
}


#' Syntactic sugar to instanciate a module.
#'
#' @export
`%<<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         instanciate(rhs, debug = T), pos = 1)
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%=>>%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(rhs))
  assign(variable_name,
         instanciate(lhs, debug = T), pos = 1)
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         instanciate(rhs, debug = F), pos = 1)
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%=>%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(rhs))
  assign(variable_name,
         instanciate(lhs, debug = F), pos = 1)
}
