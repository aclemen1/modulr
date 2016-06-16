#' Plot Dependencies.
#'
#' Plot the directed acyclic graph (DAG) of modules and dependencies.
#'
#' @inheritParams make
#' @param group A character vector of module names (cf. \code{\link{define}}) to
#'   include as a subset of the graph nodes.
#'
#' @seealso \code{\link{define}} and \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() NULL)
#' define("bar", list(m = "modulr"), function(m) NULL)
#' define("foobar", list(f = "foo", b = "bar"), function(f, b) NULL)
#' define("foobuzz", list(f = "foo", b = "buzz"), function(f, b) NULL)
#' define("fizz", list(f1 = "foobar", f2 = "foobuz"), function(f1, f2) NULL)
#' wait <- function() invisible(readline(prompt="Press [enter] to continue"))
#' plot_dependencies("foobar", reserved = FALSE); wait()
#' plot_dependencies("foobar"); wait()
#' plot_dependencies(reserved = FALSE); wait()
#' plot_dependencies()
#'
#' @aliases graph_dependencies
#' @export
plot_dependencies <- function(group, reserved = TRUE) {

  .message_meta("Entering plot_dependencies() ...",
                verbosity = +Inf)

  # nocov start
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("networkD3 is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # nocov end

  if (.is_called_from_within_module()) {
    warning("plot_dependencies is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    missing(group) || is.character(group),
    assertthat::is.flag(reserved)
  )

  universe <- .modulr_env$injector$register

  if (!missing(group)) {

    sub <- .define_all_dependent_modules(group)

    register <- .modulr_env$injector$register

    universe <- register[names(register) %in% sub]

  }

  if (length(universe) > 0) {

    deps <- Reduce(rbind, Map(function(module) {

      deps <- intersect(unlist(module$dependencies),
                        names(universe))

      data.frame(module = deps, dependency = rep(module$name, length(deps)),
                 # MAYBE: adapt values for nicer output
                 value = rep(1, length(deps)),
                 stringsAsFactors = FALSE)

    },

    universe))

    if (!reserved) {

      deps <-
        deps[!(deps$module %in% RESERVED_NAMES |
                   deps$dependency %in% RESERVED_NAMES), ]

    }

    if (isTRUE(nrow(deps) > 0)) {

      nodes <- unique(unlist(deps[, names(deps) != "value"]))

      deps$source <-
        as.integer(factor(deps$module, levels = nodes)) - 1
      deps$target <-
        as.integer(factor(deps$dependency, levels = nodes)) - 1

      return(networkD3::sankeyNetwork(
        Links = deps,
        Nodes = data.frame(node = nodes),
        Source = "source",
        Target = "target",
        NodeID = "node",
        Value = "value",
        fontSize = 10))
    }

  }

  message("No dependency to graph.")

  invisible()

}

#' @export
# nocov start
graph_dependencies <- function(...) {
  .deprecated("plot_dependencies")
  eval(substitute(plot_dependencies(...)), envir = parent.frame(1L))
}
# nocov end
