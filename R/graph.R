#' Register graph
#'
#' @export
register_graph <- function() {
  adj <- Reduce(rbind, Map(function(module) {
    deps <- unlist(module$dependencies)
    data.frame(from=rep(module$name, length(deps)), to=deps)
  }, .internals()$register))
  graph.data.frame(adj)
}
