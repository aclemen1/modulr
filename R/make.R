#' Make module
#'
#' @export
# TODO: write documentation
make <- function(name) {

  if(exists(".__name__")) {
    stop("Unauthorized make call from within a module.", call. = F)
  }

  .message_meta(sprintf("making [%s] ...", name), {
    .message_meta("checking definitions ...", {
      all_dependencies <-
        .define_all_dependent_modules(name)
    }, verbosity = 2)

    dependency_graph <- .build_dependency_graph(all_dependencies)
    layered_names <- .topological_sort_by_layers(dependency_graph)
    if(is.null(layered_names)) layered_names <- list(`1` = name)

    .message_meta(
      sprintf("found %d dependencies(s) with %d modules(s) on %d layer(s)",
              nrow(dependency_graph),
              length(unlist(layered_names)),
              length(layered_names)),
      {
        for(layer in names(layered_names)) {
          layer_idx <- as.numeric(layer)
          ordered_names <- layered_names[[layer]]

          for(ordered_name_idx in c(1:length(ordered_names))) {

            ordered_name <- ordered_names[ordered_name_idx]
            register <- get("register", pos = modulr_env)
            module <- register[[ordered_name]]
            if(is.null(module))
              stop("Module '", ordered_name, "' not defined.", call. = F)
            reinstanciated_by_parent <- any(unlist(lapply(
              module$dependencies,
              function(name) {
                register[[name]]$timestamp >= module$timestamp
              })))
            if(!module$instanciated
               | reinstanciated_by_parent
               | (ordered_name == name &
                    .signature(ordered_name) != .signature(name))) {
              .message_meta(sprintf("making [%s] ...", ordered_name), {
                env = new.env()
                assign(".__name__", ordered_name, pos = env)
                if(length(module$dependencies) > 0) {
                  args <-
                    lapply(
                      module$dependencies,
                      function(name) {
                        register[[.resolve_mapping(name, ordered_name)]]$instance
                      }
                    )
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
                module$instanciated <- T
                module$first_instance <- F

                module$timestamp <- Sys.time()
                register[[ordered_name]] <- module
                assign("register", register, pos = modulr_env)
              }, verbosity = 1)
            }
          }
        }

      }, verbosity = 2)
  }, verbosity = 2)

  get("register", pos = modulr_env)[[name]]$instance
}

#' Syntactic sugar to make a module.
#'
#' @export
# TODO: write documentation
`%<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         make(rhs), pos = parent.frame())
}

#' Syntactic sugar to make a module.
#'
#' @export
# TODO: write documentation
`%=>%` <- function(lhs, rhs) eval(substitute(`%<=%`(rhs, lhs)),
                                  envir = parent.frame())

#' Syntactic sugar to make a module and assign it in an enclosing frame.
#'
#' @export
# TODO: write documentation
`%<<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         make(rhs), pos = parent.frame(), inherits = T)
}

#' Syntactic sugar to make a module and assign it in an enclosing frame.
#'
#' @export
# TODO: write documentation
`%=>>%` <- function(lhs, rhs) eval(substitute(`%<<=%`(rhs, lhs)),
                                   envir = parent.frame())

