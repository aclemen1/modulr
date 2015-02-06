#' Plot dependencies by complexity
#'
#' @export
plot_complexity <- function(graph) {
  set.seed(12345)
  coord <- layout.kamada.kawai(graph)
  better_coord <- layout.drl(graph, use.seed = T, seed = coord)
  V(graph)$size=neighborhood.size(graph, order=length(V(graph)), mode="out")
  plot(graph, edge.arrow.size = 0.2, edge.curved = T, layout = better_coord)
}

#' Plot dependencies by usage
#'
#' @export
plot_usage <- function(graph) {
  set.seed(12345)
  coord <- layout.kamada.kawai(graph)
  better_coord <- layout.drl(graph, use.seed = T, seed = coord)
  V(graph)$size=neighborhood.size(graph, order=length(V(graph)), mode="in")
  plot(graph, edge.arrow.size = 0.2, edge.curved = T, layout = better_coord)
}
