#' Adjacency Matrix
#'
#' @export
deps_matrix <- function() {
  table(Reduce(rbind, Map(function(module) {
    deps <- unlist(module$dependencies)
    data.frame(from=rep(module$name, length(deps)), to=deps)
  }, .internals()$register)))
}

# register_graph <- function() {
#   deps <- deps_matrix()
#   chordDiagram(deps, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3))
#   circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#     xlim = get.cell.meta.data("xlim")
#     ylim = get.cell.meta.data("ylim")
#     sector.name = get.cell.meta.data("sector.index")
#     circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
#                 niceFacing = TRUE, adj = c(0, 0.5))
#   }, bg.border = NA)
# }

#' Dependency graph
#'
#' @export
dependency_graph <- function(include_special = T) {
  deps <- Reduce(rbind, Map(function(module) {
    deps <- unlist(module$dependencies)
    data.frame(from=rep(module$name, length(deps)), to=deps, stringsAsFactors = F)
  }, .internals()$register))
  if(!include_special) {
    deps <- subset(deps, !(from %in% RESERVED_NAMES | to %in% RESERVED_NAMES))
  }
  nodes <- .topological_sort_with_layer(deps)
#   forceNetwork(Links = deps %>% mutate(source = as.integer(factor(from, levels = nodes$node)) - 1, target = as.integer(factor(to, levels = nodes$node)) - 1), Nodes = nodes %>% mutate(size = layer), Source = "source", Target = "target", NodeID = "node", Nodesize = "size", Group = "layer", legend = F, opacity = 1, zoom = T, opacityNoHover = 1, fontSize = 10)
  deps$source <- as.integer(factor(deps$to, levels = nodes$node)) - 1
  deps$target <- as.integer(factor(deps$from, levels = nodes$node)) - 1
  deps$value <- 1
  sankeyNetwork(
    Links = deps,
    Nodes = nodes["node"],
    Source = "source",
    Target = "target",
    NodeID = "node",
    Value = "value",
    fontSize = 10)
}
