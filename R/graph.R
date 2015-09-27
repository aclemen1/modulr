#' Dependency graph
#'
#' @export
# TODO: write documentation
graph_dependencies <- function(group, reserved = TRUE) {

  .message_meta("Entering graph_dependencies() ...",
                verbosity = +Inf)

  # nocov start
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("networkD3 is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # nocov end

  if (.is_called_from_within_module()) {
    warning("graph_dependencies is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    missing(group) || is.character(group),
    assertthat::is.flag(reserved)
  )

  universe <- modulr_env$register

  if (!missing(group)) {

    sub <- .define_all_dependent_modules(group)

    register <- modulr_env$register

    universe <- register[names(register) %in% sub]

  }

  if (length(universe) > 0) {

    deps <- Reduce(rbind, Map(function(module) {

      deps <- intersect(unlist(module$dependencies),
                        names(universe))

      data.frame(module=deps, dependency=rep(module$name, length(deps)),
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
