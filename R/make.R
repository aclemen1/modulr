#' Make module
#'
#' @export
# TODO: write documentation
make <- function(name = modulr_env$.Last.name) { # Exclude Linting

  .message_meta(sprintf("Entering make() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  .message_meta(sprintf("Making '%s' ...", name), {

    .message_meta("Checking definitions ...", {

      all_dependencies <-
        .define_all_dependent_modules(name)

    },
    verbosity = 2)

    if(.is_regular(name))
      modulr_env$.Last.name <- name # Exclude Linting

    modulr_env$register[[c(name, "calls")]] <-
      modulr_env$register[[c(name, "calls")]] + 1

    dependency_graph <- .build_dependency_graph(all_dependencies)

    layered_names <- .topological_sort_by_layers(dependency_graph)

    if(is.null(layered_names)) layered_names <- list(`1` = name)

    .message_meta(
      sprintf("found %d dependencies(s) with %d modules(s) on %d layer(s)",
              nrow(dependency_graph),
              length(unlist(layered_names)),
              length(layered_names)
      ), {

        for(layer in names(layered_names)) {

          ordered_names <- layered_names[[layer]]

          for(ordered_name_idx in c(1:length(ordered_names))) {

            ordered_name <- ordered_names[ordered_name_idx]

            assertthat::assert_that(.is_defined(ordered_name))

            reinstanciated_by_parent <- any(unlist(lapply(
              modulr_env$register[[c(ordered_name, "dependencies")]],
              function(name) {
                modulr_env$register[[c(name, "timestamp")]] >=
                  modulr_env$register[[c(ordered_name, "timestamp")]]
              })))

            if(!modulr_env$register[[c(ordered_name, "instanciated")]]
               | reinstanciated_by_parent
               | (ordered_name == name &
                    get_digest(ordered_name) != get_digest(name))) {

              .message_meta(sprintf("Making '%s' ...", ordered_name), {

                timestamp <- Sys.time()

                env <- new.env()

                env$.__name__ <- ordered_name

                args <- list()

                if(length(modulr_env$register[[
                  c(ordered_name, "dependencies")]]) > 0) {

                  args <-
                    lapply(
                      modulr_env$register[[
                        c(ordered_name, "dependencies")]],
                      function(name) {
                        modulr_env$register[[c(
                          .resolve_mapping(name, ordered_name), "instance")]]
                      }
                    )

                }

                factory <- modulr_env$register[[c(ordered_name, "factory")]]

                environment(factory) <- env

                instance <- do.call(
                  factory,
                  args = args, quote = TRUE, envir = env)

                modulr_env$register[[c(ordered_name, "instance")]] <- instance

                modulr_env$register[[c(ordered_name, "duration")]] <-
                  as.numeric(Sys.time() - timestamp)
                modulr_env$register[[c(ordered_name, "instanciated")]] <-
                  TRUE
                modulr_env$register[[c(ordered_name, "first_instance")]] <-
                  FALSE
                modulr_env$register[[c(ordered_name, "timestamp")]] <-
                  Sys.time()

              },
              verbosity = 1)

            }

          }

        }

      },
      verbosity = 2)

  },
  verbosity = 2)

  instance <- modulr_env$register[[c(name, "instance")]]

  invisible(instance)

}

#' @export
# TODO: write documentation
make_all <- function(regexp, reserved = FALSE, error = stop, ...) {

  .message_meta("Entering make_all() ...",
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("make_all is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assertthat::assert_that(
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(reserved),
    is.function(error))

  module_names <- list_modules(regexp, reserved = reserved, wide = FALSE)

  rs <- list()
  for (name in module_names) {

    rs[[name]] <- tryCatch(make(name, ...),
                           error = error)

  }

  invisible(rs)

}

#' @export
# TODO: write documentation
make_tests <- function(...) {

  .message_meta("Entering make_tests() ...",
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    warning("make_tests is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  module_names <- list_modules("\\/tests?$", reserved = FALSE, wide = FALSE)

  rs <- list()
  for (name in module_names) {

    rs[[name]] <- tryCatch({

      lrs <- make(name, ...)

      assertthat::assert_that(
        assertthat::is.flag(lrs) || is.null(lrs),
        msg = "test is not returning a boolean or NULL.")

      if(!(is.null(lrs) || lrs))
        stop("Test failed.")

      invisible(T)

    },

    error = function(e) {

      message(sprintf("Error: %s", e$message))

      return(invisible(F))

    })

  }

  if(!all(unlist(rs))) stop("FAILED.", call. = FALSE)

  if(sample(5, 1) == 1) {
    message("PASSED. ", sample(PRAISE, 1), ".") # nocov
  } else {
    message("PASSED.") # nocov
  }

  invisible()

}

#' @export
# TODO: write documentation
make_test <- make_tests

#' Syntactic sugar to make a module.
#'
#' @export
# TODO: write documentation
`%<=%` <- function(lhs, rhs) {

  if(.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assertthat::assert_that(
    assertthat::is.string(rhs),
    msg = "right-hand side of `%<=%` is not a string."
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

  if(.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assertthat::assert_that(
    assertthat::is.string(rhs),
    msg = "right-hand side of `%<<=%` is not a string."
  )

  assign(as.character(substitute(lhs)), make(rhs),
         pos = parent.frame(), inherits = TRUE)

}

#' Syntactic sugar to make a module and assign it in an enclosing frame.
#'
#' @export
# TODO: write documentation
`%=>>%` <- function(lhs, rhs) eval(substitute(`%<<=%`(rhs, lhs)),
                                   envir = parent.frame())
