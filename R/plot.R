#' Plot Dependencies.
#'
#' Plot the directed acyclic graph (DAG) of modules and dependencies.
#'
#' @inheritParams make
#' @inheritParams networkD3::sankeyNetwork
#' @param group A character vector of module names (cf. \code{\link{define}}) to
#'   include as a subset of the graph nodes.
#' @param regexp A regular expression. If not missing, the regular expression is
#'   used to filter the names of the modules to be plotted.
#' @param ... Further arguments to be passed to
#'   \code{networkD3::\link[networkD3]{sankeyNetwork}}.
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
plot_dependencies <- function(group, regexp, reserved = TRUE,
                              fontSize = 13, ...) {

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
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(reserved)
  )

  if (missing(group)) {
    universe <- .modulr_env$injector$registry
  } else {
    sub <- .define_all_dependent_modules(group)
    universe <-
      .modulr_env$injector$registry[
        names(.modulr_env$injector$registry) %in% sub]
  }
  namespaces <- names(universe)

  if (length(universe) > 0) {

    deps <- Reduce(rbind, Map(function(module) {

      deps <- intersect(unlist(module$dependencies),
                        namespaces)

      data.frame(module = deps, dependency = rep(module$name, length(deps)),
                 # MAYBE: adapt values for nicer output
                 value = rep(1, length(deps)),
                 stringsAsFactors = FALSE)

    },

    universe))

    if (!reserved) {

      deps <- deps[
        !vapply(deps$module, FUN = .is_regular, FUN.VALUE = TRUE) &
          !vapply(deps$dependency, FUN = .is_regular, FUN.VALUE = TRUE), ]

    }

    if (!missing(regexp)) {

      keep_mods <-
        grepl(regexp, deps$module) | deps$module %in% group
      keep_deps <-
        grepl(regexp, deps$dependency) | deps$dependency %in% group
      to_collapse <- unique(c(deps$module[!keep_mods],
                              deps$dependency[!keep_deps]))

      deps$value <- NULL
      inc <- table(deps[[1]][row(deps[-1])], unlist(deps[-1]))
      cols_not_in_rows <- setdiff(dimnames(inc)[[2]], dimnames(inc)[[1]])
      row_to_add <- array(0, dim = c(length(cols_not_in_rows), ncol(inc)),
                          dimnames = list(cols_not_in_rows, dimnames(inc)[[2]]))
      inc <- rbind(inc, row_to_add)
      rows_not_in_cols <- setdiff(dimnames(inc)[[1]], dimnames(inc)[[2]])
      col_to_add <- array(0, dim = c(nrow(inc), length(rows_not_in_cols)),
                          dimnames = list(dimnames(inc)[[1]], rows_not_in_cols))
      inc <- cbind(inc, col_to_add)

      for (module in to_collapse) {
        li <- unname(inc[module, ])
        col <- unname(inc[, module])
        if (any(col == 1)) {
          inc[col == 1, ] <-
          pmax(inc[col == 1, , drop = FALSE],
               t(array(li, rev(dim(inc[col == 1, , drop = FALSE])))))
        }
        inc <- inc[dimnames(inc)[[1]] != module, dimnames(inc)[[2]] != module]
      }

      inc <- as.table(inc)

      if (length(dim(inc)) != 2) {
        deps <- deps[F, ]
      } else {
        deps <- as.data.frame(as.table(inc))
        names(deps) <- c("module", "dependency", "value")
        deps <- deps[deps$value == 1, ]
      }

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
        fontSize = fontSize,
        ...))
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
