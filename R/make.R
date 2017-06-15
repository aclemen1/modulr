# TODO test that!
.align_named_vectors <- function(ab, bc) {
  bc <- bc[!duplicated(names(bc))]
  selected_ab <- ab[ab %in% names(bc)]
  selected_bc <- bc[names(bc) %in% ab]
  aligned_ab <- selected_ab[order(selected_ab)]
  aligned_bc <- selected_bc[order(names(selected_bc))]
  assert_that(
    assertthat::are_equal(length(aligned_bc), length(aligned_ab)),
    isTRUE(!any(duplicated(names(aligned_ab)))),
           msg = "dependencies must occur only once.")
  stats::setNames(aligned_bc, names(aligned_ab))
}

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
#'   \code{\link{import_module}}, \code{\link{maps_config}},
#'   \code{\link{paths_config}}, \code{\link{reset}}, and \code{\link{touch}}.
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
#' define("foo", NULL, function() function(a) a + 1L)
#' foo <- make("foo"); foo(1L)
#' make("foo", 1L)
#' do_make("foo", args = list(a = 1L))
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

  if (.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (.is_nested_load()) {
    warning("loading calls are nested. Stopping recursion.",
            call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  do_make(name = name, args = list(...))

}

DEFAULT_DIGITS = 2L

#' @rdname make
#' @inheritParams define
#' @param args A list of arguments to be passed to the resulting function, if
#'   any. The \code{names} attribute of \code{args} gives the argument names.
#' @param quote A flag. Should the arguments be quoted?
#' @param envir An environment within which to evaluate the function call, if
#'   any. This will be most useful if the arguments are symbols or quoted
#'   expressions.
#' @aliases do.make
#' @export
do_make <- function(name = .Last.name, args = list(),
                    quote = FALSE, envir = parent.frame(1L)) {

  assert_that(is.list(args), msg = "second argument must be a list.")

  .message_meta(sprintf("Entering make() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(.is_conform(name))

  if (.is_called_from_within_module()) {
    warning("do_make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (.is_nested_load()) {
    warning("loading calls are nested. Stopping recursion.",
            call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  verbosity_level <- get_verbosity()


  .message_meta(sprintf("Making '%s' ...", name), {

    start_time <- Sys.time()

    .message_meta("Visiting and defining dependencies ...", {

      all_dependencies <-
        .define_all_dependent_modules(name)

    },
    verbosity = 2L)

    name <- all_dependencies[1L]

    if (.is_regular(name))
      .modulr_env$injector$.Last.name <- name

    .modulr_env$injector$registry[[c(name, "calls")]] <-
      .modulr_env$injector$registry[[c(name, "calls")]] + 1L

    .message_meta("Constructing dependency graph", {

      dependency_graph <- .build_dependency_graph(all_dependencies)

    },
    ok = TRUE, verbosity = 2L)

    if (nrow(dependency_graph) == 0L) {
      deps_count <- 0L
      layers <- list(name)
      layers_count <- 1L
    } else {
      deps_count <- length(unique(unlist(dependency_graph))) - 1L
      .message_meta(
        if (deps_count > 1L)
          sprintf(
            "Sorting %d dependencies with %d relations",
            deps_count,
            nrow(dependency_graph)), {

              layers <- .topological_sort_by_layers(dependency_graph)

              layers_count <- length(layers)

              if (deps_count > 1L && layers_count > 1L &&
                    2L <= verbosity_level) {
                cat(
                  if (layers_count == 2L) {
                    "on 1 layer, "
                  } else {
                    sprintf("on %d layers, ", layers_count - 1L)
                  }
                )
              }
            },
        ok = TRUE, verbosity = 2L)
    }

    .message_meta(
      if (deps_count > 1L)
        "Evaluating new and outdated dependencies ...", {

          digest <- get_digest(name)
          nodes <- unlist(layers, use.names = FALSE)
          nodes_count <- length(nodes)
          eval_counter <- 0L

          for (layer_idx in c(1L:layers_count)) {

            ordered_names <- layers[[layer_idx]]

            for (ordered_name_idx in seq_len(length(ordered_names))) {

              eval_counter <- eval_counter + 1L

              ordered_name <- ordered_names[ordered_name_idx]

              assert_that(.is_defined(ordered_name))

              reinstanciated_by_parent <- any(unlist(lapply(
                all_dependencies[
                  names(all_dependencies) %in%
                    .modulr_env$injector$registry[[c(ordered_name,
                                                     "dependencies")]]],
                function(name) {
                  .modulr_env$injector$registry[[c(name, "timestamp")]] >=
                    .modulr_env$injector$registry[[c(ordered_name,
                                                     "timestamp")]]
                })))

              if (!.modulr_env$injector$registry[[c(ordered_name,
                                                    "instanciated")]]
                  | reinstanciated_by_parent
                  | (ordered_name == name &
                       get_digest(ordered_name) != digest)) {

                .message_meta(
                  if (eval_counter != nodes_count)
                    sprintf(
                      "Evaluating #%d/%d (layer #%d/%d): '%s' ...",
                      eval_counter, nodes_count - 1L,
                      layer_idx, layers_count - 1L,
                      ordered_name), {

                        timestamp <- Sys.time()

                        args_ <- list()

                        if (length(.modulr_env$injector$registry[[
                          c(ordered_name, "dependencies")]]) > 0L) {

                          args_ <-
                            lapply(
                              .align_named_vectors(
                                unlist(.modulr_env$injector$registry[[
                                  c(ordered_name, "dependencies")]]),
                                all_dependencies
                              ),
                              function(name) {
                                .modulr_env$injector$registry[[c(
                                  name, "instance", "value")]]
                              }
                            )
                        }

                        provider <-
                          .modulr_env$injector$registry[[
                            c(ordered_name, "provider")]]

                        instanciate <- function() {
                          .__instanciator__ <- TRUE # Exclude Linting
                          instance <- withVisible(do.call(
                            provider, args = args_))
                          assign("instance", instance, pos = parent.frame())
                          .modulr_env$injector$registry[[
                            c(ordered_name, "instance")]] <- instance
                          .modulr_env$injector$registry[[
                            c(ordered_name, "duration")]] <-
                            as.numeric(Sys.time() - timestamp)
                          .modulr_env$injector$registry[[
                            c(ordered_name, "instanciated")]] <- TRUE
                          .modulr_env$injector$registry[[
                            c(ordered_name, "first_instance")]] <- FALSE
                          .modulr_env$injector$registry[[
                            c(ordered_name, "timestamp")]] <- Sys.time()
                        }
                        instanciate()

                      },
                  verbosity = 1L)

              }

            }
          }

        },
      verbosity = 2L)

    if (!exists("instance", inherits = FALSE)) {
      instance <- .modulr_env$injector$registry[[c(name, "instance")]]
    }

  },
  verbosity = 2L)

  duration <- Sys.time() - start_time

  .message_meta(
    sprintf(
      "DONE ('%s' in %s)", name,
      format(duration, digits = getOption("modulr.digits",
                                          default = DEFAULT_DIGITS))), {

      if (length(args) > 0L && is.function(instance[["value"]])) {
        return(do.call(instance[["value"]], args = args,
                       quote = quote, envir = envir))
      } else {
        return(
          ifelse(instance[["visible"]], identity, invisible)(instance[["value"]])
        )
      }

    },
    verbosity = 2L)

}

#' @rdname make
#' @export
do.make <- do_make

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

  if (sample(5L, 1L) == 1L) {
    message("PASSED. ", sample(PRAISE, 1L), ".") # nocov
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
#' reset()
#' define("foo", list(bar = "bar"), function(bar) bar)
#' define("bar#1.0.0", NULL, function() "bar v1.0.0")
#' define("bar#1.0.1", NULL, function() "bar v1.0.1")
#' make("foo")
#' maps_config$set(foo = list("bar" = "bar#1.0.0"))
#' make("foo")
#' touch("foo")
#' make("foo")
#'
#' @export
touch <- function(name = .Last.name) {

  .message_meta(sprintf("Entering touch() for '%s' ...", name),
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("touch is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  name <- .get_name(name, load = FALSE)

  assert_that(.is_defined_regular(name))

  .message_meta(sprintf("Touching '%s'", name), {

    .modulr_env$injector$registry[[c(name, "dependencies")]] <-
      lapply(lapply(
        lapply(.modulr_env$injector$registry[[c(name, "aliases")]],
               .resolve_namespace, name),
        `[[`, "resolved"), unname)
    .modulr_env$injector$registry[[c(name, "instanciated")]] <- F
    .modulr_env$injector$registry[[c(name, "digest")]] <- NULL
    .modulr_env$injector$registry[[c(name, "timestamp")]] <- Sys.time()

    if (.is_regular(name))
      .modulr_env$injector$.Last.name <- name

    # Deprecated, kept for backward compatibility.
    suppressWarnings(module_options(name)$unset())

  },
  ok = TRUE, verbosity = 2L)

  invisible()

}

#' Interactively Find, Make and Bind a Module.
#'
#' Interactively find, make, and bind a module.
#'
#' @inheritParams define
#' @param suffix A string (character vector of lenght one).
#'  Suffix added to variable names.
#' @param replace A flag. Should existing bindings be replaced?
#' @param execute A flag. Should binding be automatically made?
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
#' @seealso \code{\link{make}}.
#'
#' @examples
#' "foobar" %provides% "Hello World"
#' "foo" %provides% "Hello"
#' \dontrun{hit("foo")}
#' \dontrun{hit(foo)}
#' @export
hit <- function(name, suffix = getOption("modulr.hit_suffix"),
                replace = TRUE, execute = FALSE) {
  assert_that(
    assertthat::is.string(suffix),
    assertthat::is.flag(replace),
    assertthat::is.flag(execute)
  )
  make_unique_ <- function(name, ls = ls(parent.frame(), all.names = TRUE)) {
    utils::tail(make.unique(c(ls, name)), 1L)
  }
  name_string <- as.character(substitute(name))
  roots <-
    as.vector(stats::na.omit(vapply(
      root_config$get_all()[[1L]],
      function(path) {
        if (.dir_exists(path)) normalizePath(path) else NA_character_
      }, FUN.VALUE = "character")))
  candidates <-
    unique(list.files(
      path = roots,
      pattern = sprintf(".*(?:%s).*\\.[rR](?:(?:md)|(?:nw))?$", name_string),
      ignore.case = TRUE, recursive = TRUE, full.names = TRUE))
  modules <- unique(c(
    list_modules(sprintf("/?[^/]*(?:%s)[^/]*$", name_string),
                 wide = FALSE, cols = "name"),
    grep(
      sprintf("/?[^/]*(?:%s)[^/]*$", name_string),
      as.vector(stats::na.omit(vapply(candidates, function(candidate) {
        name <- .extract_name(candidate, strict = TRUE)
        if (is.null(name)) NA_character_ else name
      },
      FUN.VALUE = "character"))), ignore.case = TRUE, value = TRUE)))
  modules <-
    Filter(function(module) try(isTRUE(!is.null(find_module(module))),
                                silent = TRUE), modules)
  len <- length(modules)
  if (len == 0L) {
    message("No module found.")
    return(invisible(NULL))
  } else {
    if (is.symbol(substitute(name))) {
      bindings <-
        paste0(
          sub(.version_hash_string_regex, "", unlist(
            lapply(strsplit(modules, "/", fixed = TRUE),
                   utils::tail, 1L))
          ), suffix)
    } else {
      bindings <- rep(name_string, len)
    }
    ls_ <- ls(parent.frame(), all.names = TRUE)
    bindings <-
      vapply(bindings, function(name) {
        if (replace) name else make_unique_(name, ls = ls_)
      },
      FUN.VALUE = "character")
    cmds <- sprintf("%s %%<=%% \"%s\"", bindings, modules)
    if (length(bindings) > 1L) {
      cat(sprintf("[%d] %s", seq_along(cmds), cmds), sep = "\n")
    }
  }
  if (interactive()) {
    repeat {
      if (length(bindings) > 1L) {
        ans <- gsub("^\\s+|\\s+$", "",
                    readline("Apply? (default 1, ESC to abort) "))
        ans <- ifelse(ans == "", "1", ans)
        ans <- suppressWarnings(as.integer(ans))
      } else ans <- 1L
      if (ans %in% seq_along(bindings)) {
        # nocov start
        if (requireNamespace("rstudioapi", quietly = TRUE)) {
          rstudioapi::sendToConsole(cmds[ans], execute = execute)
        } else if (execute) {
          assign(
            bindings[ans],
            do.call(make, list(name = modules[ans])),
            pos = parent.frame())
          message("Module evaluation result bound to ",
                  sQuote(bindings[ans]), ".")
        } else {
          print(cmds[ans])
        }
        # nocov end
        break;
      }
    }
  }
  invisible(modules)
}

DEFAULT_HIT_SUFFIX <- "_"
