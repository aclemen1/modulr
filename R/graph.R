#' Adjacency Matrix
#'
#' @export
deps_matrix <- function() {
  table(Reduce(rbind, Map(function(module) {
    deps <- unlist(module$dependencies)
    data.frame(from=rep(module$name, length(deps)), to=deps)
  }, .internals()$register)))
}

#' Register graph
#'
#' @export
register_graph <- function() {
  deps <- deps_matrix()
  chordDiagram(deps, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.3))
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
}
