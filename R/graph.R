#' Dependency graph
#'
#' @export
# TODO: write documentation
graph_dependencies <- function(group, special = T) {

  .message_meta("Entering graph_dependencies() ...",
                verbosity = +Inf)

  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("networkD3 is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(.is_called_from_within_module()) {
    warning("graph_dependencies is called from within a module.",
            call. = F, immediate. = T)
  }

  assertthat::assert_that(
    missing(group) || is.character(group),
    assertthat::is.flag(special)
  )

  universe <- .internals()$register

  if(!missing(group)) {

    sub <- .define_all_dependent_modules(group)

    register <- .internals()$register

    universe <- register[names(register) %in% sub]

  }

  if(length(universe) > 0) {

    deps <- Reduce(rbind, Map(function(module) {

      deps <- intersect(unlist(module$dependencies),
                        names(universe))

      data.frame(module=deps, dependency=rep(module$name, length(deps)),
                 # MAYBE: adapt values for nicer output
                 value = rep(1, length(deps)),
                 stringsAsFactors = F)

    }, universe))

    if(!special) {

      deps <-
        deps[, !(deps$from %in% RESERVED_NAMES | deps$to %in% RESERVED_NAMES)]

    }

    if(isTRUE(nrow(deps) > 0)) {

      nodes <- .topological_sort_with_layer(deps[, names(deps) != "value"])

      deps$source <- as.integer(factor(deps$module, levels = nodes$node)) - 1
      deps$target <- as.integer(factor(deps$dependency, levels = nodes$node)) - 1

      return(networkD3::sankeyNetwork(
        Links = deps,
        Nodes = nodes["node"],
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
