#' Tweak a module to a future.
#'
#' @param original_name A module name.
#' @param name A new module name.
#' @param dependencies A list of dependencies.
#' @param strategy The evaluation function (or name of it) to use for resolving
#'   a future. If NULL, then the current strategy is returned.
#' @param lazy Is the strategy lazy?
#' @param ... Further arguments passed to \code{\link{get_provider}}.
#'
#' @section Warning:
#' This is an experimental feature subject to changes.
#'
#' @export
futurize <- function(
  original_name, name = paste(original_name, "future", sep = "/"),
  dependencies = get_dependencies(original_name),
  strategy = NULL, lazy = FALSE, ...) {

  # nocov start
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("package 'future' is needed for this function to work. ",
         "Please install it.",
         call. = FALSE)
  }
  # nocov end

  if (.is_called_from_within_module()) {
    warning("futurize is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  if (is.null(strategy)) strategy <- future::plan()

  provider <- eval(parse(text = deparse(
    substitute(
      function() {
        future::future({
          do.call(
            provider_,
            args = lapply(
              lapply(
                as.list(names(dependencies)),
                get, pos = environment()),
              function(arg) {
                if (inherits(arg, "Future")) future::value(arg) else arg
              }
            )
          )
        },
        evaluator = strategy,
        lazy = lazy)
      },
      list(
        provider_ =
          eval(parse(text = deparse(get_provider(original_name, ...)),
                     keep.source = TRUE))
        )
      )
    ), keep.source = TRUE))

  define(name, dependencies, provider)
}
