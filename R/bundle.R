#' Bundle a Module.
#'
#' Bundle a module and its dependencies for standalone use.
#'
#' @param ... Module name as first argument. Further arguments can be passed
#'   for evaluation to the resulting function, if any (see \code{\link{make}}).
#'
#' @details
#'
#' TODO documentation
#'
#' @export
bundle <- function(...) {

  if (nargs() == 0L) {
    name <- .Last.name
    args <- list() # Exclude Linting
  } else {
    name <- list(...)[[1L]]
    args <- substitute(list(...))[-2L]
  }

  .message_meta(sprintf("Entering batchify() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(.is_conform(name))

  if (.is_called_from_within_module()) {
    warning("make is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  register <- modulr_env$register
  .Last.name <- modulr_env$.Last.name
  config <- modulr_env$config
  verbosity <- modulr_env$verbosity
  stash <- modulr_env$stash

  rollback <- function() {
    modulr_env$register <- register
    modulr_env$.Last.name <- .Last.name
    modulr_env$config <- config
    modulr_env$verbosity <- verbosity
    modulr_env$stash <- stash
  }

  .message_meta(sprintf("Turning '%s' into a batch file ...", name), {

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

            for (ordered_name_idx in c(1:length(ordered_names))) {

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

    batch <- paste(
      batch,
      paste(
        sprintf("# ---- Make '%s' --------", name),
        sprintf(""),
        sprintf("result <- make(\"%s\")", name),
        sprintf("if(is.function(result)) {"),
        sprintf("  result <- do.call(result, args = %s)",
                deparse(substitute(args))),
        sprintf("}"),
        sprintf("return(result)"),
        sep = "\n"),
      sep = "\n")

    batch <- paste(
      sprintf("# ---- BEGIN SCRIPT --------"),
      sprintf(""),
      sprintf("# ---- Setup --------"),
      sprintf(""),
      sprintf("(function() {"),
      sprintf("library(modulr)"),
      sprintf("modulr::reset()\n"),
      batch,
      sprintf("})()"),
      sprintf(""),
      sprintf("# ---- END SCRIPT --------"),
      sep = "\n")

    cat(batch)
    return(invisible(batch))

  },
  verbosity = 2)

}
