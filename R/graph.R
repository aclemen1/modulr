#' Dependency graph
#'
#' @export
# TODO: write documentation
graph_dependencies <- function(group, special = T) {

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

  message("No dependency to graph.", call. = F)

  invisible()

}
