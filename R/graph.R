#' Dependency graph
#'
#' @export
# TODO: write documentation
dependency_graph <- function(name, include_special = T) {
  if(!missing(name)) {
    sub <- .define_all_dependent_modules(name)
    universe <- .internals()$register[names(.internals()$register) %in% sub]
  } else {
    universe <- .internals()$register
  }
  if(isTRUE(length(universe) > 0)) {
    deps <- Reduce(rbind, Map(function(module) {
      deps <- intersect(unlist(module$dependencies),
                        names(universe))
      data.frame(dependency=rep(module$name, length(deps)), module=deps,
                 # TODO: adapt values for nicer output
                 value = rep(1, length(deps)),
                 stringsAsFactors = F)
    }, universe))
    if(!include_special) {
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
  warning("No dependency to graph.", call. = F, immediate. = T)
  invisible(F)
}
