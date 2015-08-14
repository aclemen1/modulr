#' Instanciate a module.
#'
#' @export

instanciate <- function(name, debug = F, force = F) {
  message_meta(
    sprintf("instanciating [%s] ...",
            name))

  all_dependencies <-
    .define_all_dependent_modules(
      name,
      debug)
  dependency_graph <- .build_dependency_graph(all_dependencies)
  layered_names <- .topological_sort_by_layers(dependency_graph)
  if(length(layered_names) == 0) layered_names <- list(`1` = name)
  message_meta(
    sprintf("requiring %d modules on %d layers",
            length(unlist(layered_names)),
            length(layered_names)),
    level = 1)
  specs <- system.time({
    for(layer in names(layered_names)) {
      layer_idx <- as.numeric(layer)
      ordered_names <- layered_names[[layer]]
      message_meta(sprintf("resolving layer %d/%d: %d module(s) ...",
                           layer_idx, length(layered_names),
                           length(ordered_names)),
                   level = 2)
      for(ordered_name_idx in c(1:length(ordered_names))) {
        ordered_name <- ordered_names[ordered_name_idx]
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
          module$reinstanciate_children <- T
        }
        if(!module$instanciated
           | reinstanciated_by_parent
           | ((debug | force) & ordered_name == name)) {
          if (!(ordered_name %in% RESERVED_NAMES))
            if(module$instanciated | !module$first_instance)
              message_meta(sprintf("re-instanciating [%s] ...",
                                   ordered_name),
                           level = 3)
            else
              message_meta(sprintf("instanciating [%s] ...",
                                   ordered_name),
                           level = 3)
          specs <- system.time({
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
              # WORKS:
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
          })
          module$instanciated <- T
          module$first_instance <- F
          register[[ordered_name]] <- module
          assign("register", register, pos = modulr_env)
        }
      }
    }
    for(name in unlist(layered_names)) {
      register <- get("register", pos = modulr_env)
      register[[name]]$reinstanciate_children <- F
      assign("register", register, pos = modulr_env)
    }
  })
  message_meta(sprintf("DONE in %.3f secs", specs[["elapsed"]]),
               level = 1)

  get("register", pos = modulr_env)[[name]]$instance
}


#' Syntactic sugar to instanciate a module.
#'
#' @export
`%<<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         instanciate(rhs, debug = T), pos = parent.frame())
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%=>>%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(rhs))
  assign(variable_name,
         instanciate(lhs, debug = T), pos = parent.frame())
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         instanciate(rhs, debug = F), pos = parent.frame())
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%=>%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(rhs))
  assign(variable_name,
         instanciate(lhs, debug = F), pos = parent.frame())
}
