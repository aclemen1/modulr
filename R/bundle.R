# nocov start

#' Bundle a Module (experimental).
#'
#' Bundle a module for script use.
#'
#' @inheritParams define
#' @inheritParams make
#' @param ... For \code{bundle}, further arguments to be passed
#'   for evaluation to the resulting function, if any (see \code{\link{make}}).
#'   For \code{do_bundle}, further arguments to be passed to
#'   \code{\link{cat}}.
#'
#' @details
#'
#' Experimental.
#'
#' @export
bundle <- function(name = .Last.name, ...) {

  if (.is_called_from_within_module()) {
    warning("bundle is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  do_bundle(name = name, args = list(...))

}

#' @rdname bundle
#' @inheritParams make
#' @inheritParams do_make
#' @param pre_hook An expression. Code to be evaluated before the script.
#' @param post_hook An expression. Code to be evaluated after the script. The
#'   `result` variable contains the result of the script evaluation.
#' @export
do_bundle <- function(name = .Last.name, args = list(),
                     quote = FALSE, envir = parent.frame(1L),
                     pre_hook = NULL, post_hook = NULL, ...) {

  assert_that(is.list(args), msg = "second argument must be a list.")

  assert_that(
    mode(pre_hook) %in% c("NULL", "call", "expression", "(", "function"),
    msg = "pre_hook must be an expression")

  assert_that(
    mode(post_hook) %in% c("NULL", "call", "expression", "(", "function"),
    msg = "post_hook must be an expression")

  .message_meta(sprintf("Entering bundle() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(.is_conform(name))

    if (.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  register <- .modulr_env$injector$register
  .Last.name <- .modulr_env$injector$.Last.name
  config <- .modulr_env$injector$config
  verbosity <- .modulr_env$injector$verbosity
  stash <- .modulr_env$injector$stash

  rollback <- function() {
    .modulr_env$injector$register <- register
    .modulr_env$injector$.Last.name <- .Last.name
    .modulr_env$injector$config <- config
    .modulr_env$injector$verbosity <- verbosity
    .modulr_env$injector$stash <- stash
  }

  .message_meta(sprintf("Bundling '%s' ...", name), {

    .message_meta("Visiting and defining dependencies ...", {

      all_dependencies <-
        .define_all_dependent_modules(name)

    },
    verbosity = 2)

    if (.is_regular(name))
      .modulr_env$injector$.Last.name <- name

    .modulr_env$injector$register[[c(name, "calls")]] <-
      .modulr_env$injector$register[[c(name, "calls")]] + 1

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

              if (deps_count > 1 && layers_count > 1)
                message(sprintf("%d layers, ", layers_count - 1),
                        appendLF = FALSE)

            },
        ok = TRUE, verbosity = 2)
    }

    .message_meta(
      if (deps_count > 0)
        "Appending dependencies ...", {

          batch <- character(0)

          for (layer_idx in c(1:layers_count)) {

            ordered_names <- layers[[layer_idx]]

            for (ordered_name_idx in seq_len(length(ordered_names))) {

              ordered_name <- ordered_names[ordered_name_idx]

              assert_that(.is_defined(ordered_name))

              if (.is_regular(ordered_name)) {
                batch <- paste(
                  batch,
                  paste(
                    sprintf("# ---- Define '%s' --------", ordered_name),
                    sprintf(""),
                    sprintf("%s", .module_to_string(ordered_name)),
                    sep = "\n"),
                  sep = "\n")
              }

            }
          }

        })

  },
  verbosity = 2)

  .message_meta(sprintf("DONE ('%s')", name), {

    bundle <- paste(c(
      sprintf("#!%s R", system("which env", intern = TRUE)),
      sprintf(""),
      sprintf("# ---- BEGIN SCRIPT --------"),
      sprintf(""),
      sprintf("(function() {"),
      sprintf(""),
      sprintf("# ---- Setup --------"),
      sprintf(""),
      sprintf("wd <- setwd(\"%s\")", getwd()),
      sprintf("on.exit(setwd(wd), add = TRUE)", name),
      sprintf("library(modulr)"),
      sprintf("stash_id <- modulr::stash(\"Pre-bundle for '%s'.\")", name),
      sprintf("on.exit(modulr::unstash(id = stash_id), add = TRUE)", name),
      sprintf("modulr::reset(all = FALSE)"),
      sprintf("modulr::root_config$set(%s)",
              deparse(root_config$get_all()[[1]])),
      if (!is.null(pre_hook)) {
        paste(
          sprintf(""),
          sprintf("# ---- Pre Hook --------"),
          sprintf(""),
          sprintf("eval(%s)",
                  paste(deparse(substitute(pre_hook)), collapse = "\n")),
          sep = "\n"
        )
      },
      paste(
        batch,
        sprintf(""),
        paste(
          sprintf("# ---- Make '%s' --------", name),
          sprintf(""),
          sprintf(
            paste0(
              "result <- ",
              "do_make(name = \"%s\", args = %s, quote = %s, envir = %s)"
            ),
            name, deparse(args), deparse(quote), deparse(substitute(envir))),
          sep = "\n"),
        sep = "\n"),
      if (!is.null(post_hook)) {
        paste(
          sprintf(""),
          sprintf("# ---- Post Hook --------"),
          sprintf(""),
          sprintf("eval(%s)",
                  paste(deparse(substitute(post_hook)), collapse = "\n")),
          sep = "\n"
        )
      },
      sprintf(""),
      sprintf("})()"),
      sprintf(""),
      sprintf("# ---- END SCRIPT --------")),
      collapse = "\n")

    cat(bundle, ...)

    return(invisible(bundle))

  },
  verbosity = 2)

}

#' @rdname bundle
#' @export
do.bundle <- do_bundle

# nocov end
