#' Plot Dependencies.
#'
#' Plot the directed acyclic graph (DAG) of modules and dependencies.
#'
#' @inheritParams make
#' @param group A character vector of module names (cf. \code{\link{define}}) to
#'   include as a subset of the graph nodes.
#' @param regexp A regular expression. If not missing, the regular expression is
#'   used to filter the names of the modules to be plotted.
#' @param render_engine A function. Rendering engine used to plot the
#'   dependencies.
#' @param ... Further arguments to be passed to \code{render_engine}.
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
#' plot_dependencies("foobar", reserved = FALSE)
#' \dontrun{wait()}
#' plot_dependencies("foobar")
#' \dontrun{wait()}
#' plot_dependencies(reserved = FALSE)
#' \dontrun{wait()}
#' plot_dependencies()
#'
#' @aliases graph_dependencies
#' @export
plot_dependencies <- function(group, regexp, reserved = TRUE,
                              render_engine = sankey_engine,
                              ...) {

  .message_meta("Entering plot_dependencies() ...",
                verbosity = +Inf)

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

  if (length(universe) > 0L) {

    deps <- Reduce(rbind, Map(function(module) {

      deps <- intersect(
        unlist(lapply(
          module$dependencies,
          function(alias) find_module(alias)[["name"]])),
        namespaces)

      data.frame(
        module = deps,
        dependency = rep(find_module(module$name)[["name"]], length(deps)),
        # MAYBE: adapt values for nicer output
        value = rep(1L, length(deps)),
        stringsAsFactors = FALSE)

    },

    universe))

    keep_mods <- TRUE
    keep_deps <- TRUE

    if (!reserved) {
      keep_mods <-
        keep_mods & vapply(deps$module, FUN = .is_regular, FUN.VALUE = TRUE)
      keep_deps <-
        keep_deps & vapply(deps$dependency, FUN = .is_regular, FUN.VALUE = TRUE)
    }

    if (!missing(regexp)) {

      keep_mods <- keep_mods &
        grepl(regexp, deps$module) | deps$module %in% group
      keep_deps <- keep_deps &
        grepl(regexp, deps$dependency) | deps$dependency %in% group
    }

    to_collapse <- unique(c(deps$module[!keep_mods],
                            deps$dependency[!keep_deps]))

    if (length(to_collapse) > 0L) {

      deps$value <- NULL
      inc <- table(deps[[1L]][row(deps[-1L])], unlist(deps[-1L]))
      cols_not_in_rows <- setdiff(dimnames(inc)[[2L]], dimnames(inc)[[1L]])
      row_to_add <- array(0L, dim = c(length(cols_not_in_rows), ncol(inc)),
                          dimnames =
                            list(cols_not_in_rows, dimnames(inc)[[2L]]))
      inc <- rbind(inc, row_to_add)
      rows_not_in_cols <- setdiff(dimnames(inc)[[1L]], dimnames(inc)[[2L]])
      col_to_add <- array(0L, dim = c(nrow(inc), length(rows_not_in_cols)),
                          dimnames =
                            list(dimnames(inc)[[1L]], rows_not_in_cols))
      inc <- cbind(inc, col_to_add)

      for (module in to_collapse) {
        li <- unname(inc[module, ])
        col <- unname(inc[, module])
        if (any(col == 1L)) {
          inc[col == 1L, ] <-
          pmax(inc[col == 1L, TRUE, drop = FALSE],
               t(array(li, rev(dim(inc[col == 1L, TRUE, drop = FALSE])))))
        }
        inc <- inc[dimnames(inc)[[1L]] != module, dimnames(inc)[[2L]] != module]
      }

      inc <- as.table(inc)

      if (length(dim(inc)) != 2L) {
        deps <- deps[F, ]
      } else {
        deps <- as.data.frame(as.table(inc))
        names(deps) <- c("module", "dependency", "value")
        deps <- deps[deps$value == 1L, ]
      }

    }

    if (isTRUE(nrow(deps) > 0L)) {

      return(do.call(
        render_engine, args = list(deps = deps, ...)))

    }

  }

  message("No dependency to graph.")

  invisible()

}

#' Plot Dependencies with Sankey Engine.
#'
#' Plot the directed acyclic graph (DAG) of modules and dependencies with a
#' Sankey diagram.
#'
#' @inheritParams networkD3::sankeyNetwork
#' @param deps A data frame of modules and their dependencies.
#' @param ... Further arguments to be passed to
#'   \code{networkD3::\link[networkD3]{sankeyNetwork}}.
#'
#' @seealso \code{\link{plot_dependencies}}.
#' @export
sankey_engine <- function(deps, ...) {

  # nocov start
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("package 'networkD3' is needed for this function to work. ",
         "Please install it.",
         call. = FALSE)
  }
  # nocov end

  args <- list(...)

  if (is.null(args[["fontSize"]])) args[["fontSize"]] <- 13.0

  nodes <- unique(unlist(deps[, names(deps) != "value"]))

  deps$source <-
    as.integer(factor(deps$module, levels = nodes)) - 1L
  deps$target <-
    as.integer(factor(deps$dependency, levels = nodes)) - 1L

  return(do.call(networkD3::sankeyNetwork, args = c(list(
    Links = deps,
    Nodes = data.frame(node = nodes),
    Source = "source",
    Target = "target",
    NodeID = "node",
    Value = "value"), args)))

}

#' Plot Dependencies with Chord Engine.
#'
#' Plot the directed acyclic graph (DAG) of modules and dependencies with a
#' bipartite Chord diagram.
#'
#' @param deps A data frame of modules and their dependencies.
#' @param ... Further arguments to be passed to
#'   \code{chorddiag::chorddiag}.
#'
#' @seealso \code{\link{plot_dependencies}}.
#' @export
chord_engine <- function(deps, ...) {

  # nocov start
  if (!requireNamespace("chorddiag", quietly = TRUE)) {
    stop("package 'chorddiag' is needed for this function to work. ",
         "Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("package 'RColorBrewer' is needed for this function to work. ",
         "Please install it.",
         call. = FALSE)
  }
  # nocov end

  args <- list(...)

  if (is.null(args[["groupnameFontsize"]]))
    args[["groupnameFontsize"]] <- 11.0

  if (is.null(args[["groupnamePadding"]]))
    args[["groupnamePadding"]] <- 10.0

  deps <- deps[c("dependency", "module")]
  names(deps) <- rev(names(deps))
  levels <- unique(c(as.character(deps[[1L]]), as.character(deps[[2L]])))
  deps[[1L]] <- factor(deps[[1L]], levels = levels)
  deps[[2L]] <- factor(deps[[2L]], levels = levels)
  deps <- table(deps)

  deps <- deps[rowSums(deps) > 0L, colSums(deps) > 0L, drop = FALSE]

  row_ord <- order(rowSums(deps), rownames(deps))
  col_ord <- order(-colSums(deps), colnames(deps), decreasing = TRUE)

  do.call(chorddiag::chorddiag, args = c(list(
    deps[row_ord, col_ord, drop = FALSE],
    type = "bipartite",
    showTicks = FALSE,

    groupColors =
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(9L, "Set1"))(
        length(levels))), args))

}

#' @export
# nocov start
graph_dependencies <- function(...) {
  .deprecated("plot_dependencies")
  eval(substitute(plot_dependencies(...)), envir = parent.frame(1L))
}
# nocov end
