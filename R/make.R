#' Make module
#'
#' @export
# TODO: write documentation
# TODO: test .Last.name assignments
make <- function(name) {

  if(exists(".__name__")) {
    stop("Unauthorized call from a module.", call. = F)
  }

  if(missing(name)) {
    name <- setdiff(
      get0(".Last.name", envir = modulr_env, ifnotfound = NULL),
      RESERVED_NAMES)
  }

  .message_meta(sprintf("making [%s] ...", name), {

    .message_meta("checking definitions ...", {

      all_dependencies <-
        .define_all_dependent_modules(name)

    }, verbosity = 2)

    assign(".Last.name", name, pos = modulr_env)

    register <- .internals()$register

    register[[name]]$calls <- register[[name]]$calls + 1
    assign("register", register, pos = modulr_env)

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

            assertthat::assert_that(.is_defined(ordered_name))

            register <- .internals()$register

            module <- register[[ordered_name]]

            reinstanciated_by_parent <- any(unlist(lapply(
              module$dependencies,
              function(name) {
                register[[name]]$timestamp >= module$timestamp
              })))

            if(!module$instanciated
               | reinstanciated_by_parent
               | (ordered_name == name &
                    get_signature(ordered_name) != get_signature(name))) {

              .message_meta(sprintf("making [%s] ...", ordered_name), {

                timestamp <- Sys.time()

                env = new.env()

                assign(".__name__", ordered_name, pos = env)

                if(length(module$dependencies) > 0) {

                  args <-
                    lapply(
                      module$dependencies,
                      function(name) {
                        register[[
                          .resolve_mapping(name, ordered_name)]]$instance
                      }
                    )

                  # WORKS:
                  # module$instance <- evalq(do.call(
                  #   function() {get("variable", pos = env)},
                  #   args = list()), envir = env)
                  #
                  # DOES NOT WORK:
                  # f <- function() {get("variable", pos = env)}
                  # module$instance <-
                  #   evalq(do.call(f, args = list()), envir = env)
                  #
                  # WORKAROUND:
                  # module$instance <-
                  #   evalq(do.call(eval(parse(text=deparse(f))),
                  #   args = list()), envir = env)

                  module$instance <- evalq(do.call(
                    eval(parse(text=deparse(module$factory))),
                    args = args), envir = env)

                } else {

                  module$instance <- evalq(do.call(
                    eval(parse(text=deparse(module$factory))),
                    args = list()), envir = env)

                }

                module$duration <- as.numeric(Sys.time() - timestamp)
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

  .internals()$register[[name]]$instance

}

#' @export
# TODO: test it
# TODO: write documentation
make_all <- function(regexp, all = F, error = stop, ...) {

  assertthat::assert_that(
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(all),
    is.function(error))

  module_names <- list_modules(regexp, all, wide = F)

  rs <- list()
  for(name in module_names) {

    rs[[name]] <- tryCatch(make(name, ...),
             error = error)

    cat("\n")

  }

  invisible(rs)

}

#' @export
# TODO: test it
# TODO: write documentation
make_tests <- function(...) {

  rs <-
    make_all(
      "/test$", all = F,
      error = function(e) {
        message(sprintf("Error: %s", e$message))
        return(F)
      },
      ...)

  if(length(rs) > 0) stop("Test(s) failed.", call. = F)

  invisible()

}

#' Syntactic sugar to make a module.
#'
#' @export
# TODO: write documentation
`%<=%` <- function(lhs, rhs) {

  assertthat::assert_that(
    assertthat::is.string(rhs)
  )

  assign(as.character(substitute(lhs)), make(rhs), pos = parent.frame())

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

  assertthat::assert_that(
    assertthat::is.string(rhs)
  )

  assign(as.character(substitute(lhs)), make(rhs),
         pos = parent.frame(), inherits = T)

}

#' Syntactic sugar to make a module and assign it in an enclosing frame.
#'
#' @export
# TODO: write documentation
`%=>>%` <- function(lhs, rhs) eval(substitute(`%<<=%`(rhs, lhs)),
                                   envir = parent.frame())
