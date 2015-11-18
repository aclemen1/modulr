#' Make a Module.
#'
#' Make or remake a module.
#'
#' @inheritParams define
#' @param ... For \code{make}, further arguments to be passed for evaluation to
#'   the resulting function, if any. For \code{make_all_tests}, further
#'   arguments to be passed to \code{\link{load_all_modules}}.
#'
#' @return The object resulting of the evaluation of the provider function of
#'   the module. If the object is a function and arguments are passed, returns
#'   the object resulting of the evaluation of the function on these arguments.
#'
#' @details
#'
#' A call to the \code{make} function triggers a series of actions, which are
#' actually the core purposes of the modulr package.
#' \enumerate{
#' \item All dependencies are visited and defined, recursively. This process is
#' based on the explicit and implicit rules of name resolution, as explained in
#' \code{\link{define}}. In particular, the configurations set by
#' \code{\link{root_config}}, \code{\link{paths_config}}, and
#' \code{\link{maps_config}} are taken into account and every module for which
#' changes are detected is automatically redefined.
#' \item Along the lines of this recursive process, an internal representation
#' of the dependencies and the relations between them is constructed. This
#' provides a directed graph, which vertices represent the modules to be
#' evaluated, and edges represent constraints on evaluations that must be
#' performed before others.
#' \item If no cycle among dependencies is detected, the graph is then a
#' Directed Acyclic Graph (DAG), and a so called topological sorting can be
#' performed on it to compute a well ordered sequence of evaluations.
#' \item Each module provider is then evaluated in the order, or re-evaluated if
#' outdated, with all its dependencies passed as arguments. A module is
#' considered outdated when it has been explicitly \code{\link{touch}}ed or if
#' one of its dependencies has been redefined or is itself outdated. The result
#' of the evaluation of every module provider is stored in the modulr internal
#' state, so that it can be reused when appropriate, without re-evaluation.
#' }
#'
#' The \code{make_all} function applies \code{make} to each defined module. If a
#' \code{regexp} is specified, this applies only to modules which name satisfies
#' the regular expression. Similarily, the \code{make_tests} function applies to
#' each module which name contains \code{/test/} or \code{/tests/}. It is also
#' possible to run tests on all modules defined in a named directory with
#' \code{make_all_tests}.
#'
#' @section Syntactic Sugars:
#'  \preformatted{variable \%<=\% name}
#'  \preformatted{name \%=>\% variable}
#'  \preformatted{variable \%<<=\% name}
#'  \preformatted{name \%=>>\% variable}
#'
#' The expressions \code{variable \%<=\% name} and \code{name \%=>\% variable}
#' (respectively \code{variable \%<<=\% name} and \code{name \%=>>\% variable})
#' are just \emph{syntactic sugars} for the expression
#' \code{variable <- make(name)} (respectively \code{variable <<- make(name)}).
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{.Last.name}}, \code{\link{plot_dependencies}},
#'   \code{\link{import_module}}, \code{\link{make}},
#'   \code{\link{maps_config}}, \code{\link{paths_config}}, \code{\link{reset}},
#'   and \code{\link{touch}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() {
#'   message("Generating timestamp ...")
#'   format(Sys.time(), "%H:%M:%OS6")
#' })
#' make("foo") # timestamp evaluated at *make-time*, ...
#' make("foo") # only once
#'
#' reset()
#' define("foo", NULL, function() function() {
#'   message("Generating timestamp ...")
#'   format(Sys.time(), "%H:%M:%OS6")
#' })
#' foo <- make("foo")
#' foo() # timestamp evaluated at *run-time*, ...
#' foo() # again, ...
#' foo() # and again
#'
#' reset()
#' define("foo", NULL, function() {
#'   library(memoise)
#'   memoise(function() {
#'     message("Generating timestamp ...")
#'     format(Sys.time(), "%H:%M:%OS6")
#'   })
#' })
#' foo <- make("foo")
#' foo() # timestamp evaluated at *run-time*, but ...
#' foo() # only once
#'
#' reset()
#' define("foo", NULL, function() function(a) a + 1)
#' foo <- make("foo"); foo(1)
#' make("foo", 1)
#'
#' reset()
#' define("A", NULL, function() "(A)")
#' define("B", NULL, function() "(B)")
#' define("C", list(a = "A"), function(a) paste0("(", a, "C)"))
#' define("D", list(a = "A", b = "B"), function(a, b) paste0("(", a, b, "D)"))
#' define("E", list(d = "D"), function(d) paste0("(", d, "E)"))
#' define("F", list(c = "C", d = "D", e = "E"),
#'   function(c, d, e) paste0("(", c, d, e, "F)"))
#' make()
#' define("B", NULL, function() "(B')")
#' make("F")
#' plot_dependencies()
#'
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() "Hello World!")', file = tmp_file)
#' root_config$set(tmp_dir)
#' set_verbosity(1L)
#' make("foo")
#' make("foo")
#' touch("foo")
#' make("foo")
#' unlink(tmp_dir, recursive = TRUE)
#'
#' \dontrun{
#' reset()
#' # https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3
#' "modulr/vault" %imports%
#'   paste0("https://gist.githubusercontent.com/aclemen1/",
#'     "3fcc508cb40ddac6c1e3/raw/modulr-vault.Rmd")
#' list_modules()
#' make_tests()
#' make("modulr/vault/example")
#' touch("modulr/vault")
#' make_all()}
#'
#' @aliases %<=% %=>% %<<=% %=>>%
#' @export
make <- function(name = .Last.name, ...) {

  .message_meta(sprintf("Entering make() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(.is_conform(name))

  if (.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (.is_nested_load()) {
    warning("make calls are nested. Stopping recursion.",
            call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  verbosity_level <- get_verbosity()

  .message_meta(sprintf("Making '%s' ...", name), {

    .message_meta("Visiting and defining dependencies ...", {

      all_dependencies <-
        .define_all_dependent_modules(name)

    },
    verbosity = 2)

    if (.is_regular(name))
      modulr_env$.Last.name <- name

    modulr_env$register[[c(name, "calls")]] <-
      modulr_env$register[[c(name, "calls")]] + 1

    .message_meta("Constructing dependency graph", {

      dependency_graph <- .build_dependency_graph(all_dependencies)

    },
    ok = TRUE, verbosity = 2)

    if (nrow(dependency_graph) == 0) {
      deps_count <- 0
      layers <- list(name)
      layers_count <- 1
    } else {
      deps_count <- length(unique(unlist(dependency_graph))) - 1
      .message_meta(
        if (deps_count > 1)
          sprintf(
            "Sorting %d dependencies with %d relations",
            deps_count,
            nrow(dependency_graph)), {

              layers <- .topological_sort_by_layers(dependency_graph)

              layers_count <- length(layers)

              if(deps_count > 1 && layers_count > 1 &&
                   2 <= verbosity_level) {
                message(
                  if (layers_count == 2) {
                    "on 1 layer, "
                  } else {
                    sprintf("on %d layers, ", layers_count - 1)
                  },
                  appendLF = FALSE)
              }
            },
        ok = TRUE, verbosity = 2)
    }

    .message_meta(
      if (deps_count > 1)
        "Evaluating new and outdated dependencies ...", {

          digest <- get_digest(name)
          nodes <- unlist(layers, use.names = FALSE)
          nodes_count <- length(nodes)
          eval_counter <- 0

          for(layer_idx in c(1:layers_count)) {

            ordered_names <- layers[[layer_idx]]

            for (ordered_name_idx in c(1:length(ordered_names))) {

              eval_counter <- eval_counter + 1

              ordered_name <- ordered_names[ordered_name_idx]

              assert_that(.is_defined(ordered_name))

              reinstanciated_by_parent <- any(unlist(lapply(
                modulr_env$register[[c(ordered_name, "dependencies")]],
                function(name) {
                  modulr_env$register[[c(name, "timestamp")]] >=
                    modulr_env$register[[c(ordered_name, "timestamp")]]
                })))

              if (!modulr_env$register[[c(ordered_name, "instanciated")]]
                  | reinstanciated_by_parent
                  | (ordered_name == name &
                       get_digest(ordered_name) != digest)) {

                .message_meta(
                  if (eval_counter != nodes_count)
                    sprintf(
                      "Evaluating #%d/%d (layer #%d/%d): '%s' ...",
                      eval_counter, nodes_count - 1,
                      layer_idx, layers_count - 1,
                      ordered_name), {

                        timestamp <- Sys.time()

                        args <- list()

                        if (length(modulr_env$register[[
                          c(ordered_name, "dependencies")]]) > 0) {

                          args <-
                            lapply(
                              modulr_env$register[[
                                c(ordered_name, "dependencies")]],
                              function(name) {
                                modulr_env$register[[c(
                                  .resolve_mapping(name, ordered_name),
                                  "instance", "value")]]
                              }
                            )

                        }

                        provider <-
                          modulr_env$register[[
                            c(ordered_name, "provider")]]

                        instance <- withVisible(do.call(
                          provider,
                          args = args, quote = TRUE,
                          envir = new.env(parent = baseenv())))

                        modulr_env$register[[
                          c(ordered_name, "instance")]] <- instance
                        modulr_env$register[[
                          c(ordered_name, "duration")]] <-
                          as.numeric(Sys.time() - timestamp)
                        modulr_env$register[[
                          c(ordered_name, "instanciated")]] <- TRUE
                        modulr_env$register[[
                          c(ordered_name, "first_instance")]] <- FALSE
                        modulr_env$register[[
                          c(ordered_name, "timestamp")]] <- Sys.time()

                        gc(verbose = FALSE)

                      },
                  verbosity = 1)

              }

            }
          }

        },
      verbosity = 2)

    instance <- modulr_env$register[[c(name, "instance")]]

  },
  verbosity = 2)

  .message_meta(sprintf("DONE ('%s')", name), {

    if(!missing(...) && is.function(instance[["value"]])) {
      return(eval(substitute(instance[["value"]](...)), # Exclude Linting
                  envir = parent.frame(1L)))
    } else {
      return(
        ifelse(instance[["visible"]], identity, invisible)(instance[["value"]])
      )
    }

  },
  verbosity = 2)

}

#' @rdname make
#' @param regexp A regular expression. If not missing, the regular expression
#'  is used to filter the names of the modules to be made.
#' @param reserved A flag. Should special modules with a reserved name be
#'   considered?
#' @param error A function. This function is triggered on error.
#' @export
make_all <- function(regexp, reserved = FALSE, error = stop) {

  .message_meta("Entering make_all() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("make_all is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(reserved),
    is.function(error))

  module_names <- list_modules(regexp, reserved = reserved, wide = FALSE)

  rs <- list()
  for (name in module_names) {

    rs[[name]] <- tryCatch(make(name),
                           error = error)

  }

  invisible(rs)

}

#' @rdname make
#' @inheritParams make_all
#' @export
make_tests <- function() {

  .message_meta("Entering make_tests() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("make_tests is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  module_names <- list_modules("\\/tests?$", reserved = FALSE, wide = FALSE)

  rs <- list()
  for (name in module_names) {

    rs[[name]] <- tryCatch({

      lrs <- make(name)

      assert_that(
        assertthat::is.flag(lrs) || is.null(lrs),
        msg = "test is not returning a boolean or NULL.")

      if (!(is.null(lrs) || lrs))
        stop("Test failed.")

      invisible(T)

    },

    error = function(e) {

      message(sprintf("Error: %s", e$message))

      return(invisible(F))

    })

  }

  if (!all(unlist(rs))) stop("FAILED.", call. = FALSE)

  if (sample(5, 1) == 1) {
    message("PASSED. ", sample(PRAISE, 1), ".") # nocov
  } else {
    message("PASSED.") # nocov
  }

  invisible()

}

#' @rdname make
#' @export
make_all_tests <- function(...) {

  .message_meta("Entering make_all_tests() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("make_all_tests is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  load_all_modules(...)

  make_tests()
}

#' @export
`%<=%` <- function(variable, name) {

  if (.is_called_from_within_module()) {
    warning("`%<=%` or `%=>%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.string(name),
    msg = "right-hand side of `%<=%` is not a string."
  )

  assign(as.character(substitute(variable)), make(name), pos = parent.frame())

}

#' @export
`%=>%` <- function(name, variable) eval(substitute(`%<=%`(variable, name)),
                                        envir = parent.frame())

#' @export
`%<<=%` <- function(variable, name) {

  if (.is_called_from_within_module()) {
    warning("`%<<=%` or `%=>>%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.string(name),
    msg = "right-hand side of `%<<=%` is not a string."
  )

  assign(as.character(substitute(variable)), make(name),
         pos = parent.frame(), inherits = TRUE)

}

#' @export
`%=>>%` <- function(name, variable) eval(substitute(`%<<=%`(variable, name)),
                                         envir = parent.frame())

#' Touch a Module.
#'
#' Touch a module by marking it as outdated and by resetting its default
#' configuration, if appropriate.
#'
#' @inheritParams define
#'
#' @details
#'
#' See \code{\link{make}} and \code{\link{module_options}}.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{.Last.name}}, \code{\link{plot_dependencies}},
#'   \code{\link{make}}, \code{\link{module_options}},
#'   and \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() format(Sys.time(), "%H:%M:%OS6"))
#' make()
#' make()
#' touch()
#' make()
#'
#' reset()
#' define("A", NULL, function() "(A)")
#' define("B", NULL, function() "(B)")
#' define("C", list(a = "A"), function(a) paste0("(", a, "C)"))
#' define("D", list(a = "A", b = "B"), function(a, b) paste0("(", a, b, "D)"))
#' define("E", list(d = "D"), function(d) paste0("(", d, "E)"))
#' define("F", list(c = "C", d = "D", e = "E"),
#'   function(c, d, e) paste0("(", c, d, e, "F)"))
#' make()
#' touch("B")
#' make("F")
#' plot_dependencies()
#'
#' @export
touch <- function(name = .Last.name) {

  .message_meta(sprintf("Entering touch() for '%s' ...", name),
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("touch is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(.is_defined_regular(name))

  .message_meta(sprintf("Touching '%s'", name), {

    modulr_env$register[[c(name, "instance")]] <- NULL
    modulr_env$register[[c(name, "instanciated")]] <- F
    modulr_env$register[[c(name, "digest")]] <- NULL
    modulr_env$register[[c(name, "timestamp")]] <- Sys.time()

    if (.is_regular(name))
      modulr_env$.Last.name <- name

    # Deprecated, kept for backward compatibility.
    suppressWarnings(module_options(name)$unset())

  },
  ok = TRUE, verbosity = 2)

  invisible()

}
