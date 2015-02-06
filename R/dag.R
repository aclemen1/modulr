.build_dependency_graph <- function(all_dependencies) {
  from <- c()
  to <- c()
  for(name in all_dependencies) {
    dependencies <- get("register", pos = modulr_env)[[name]]$dependencies
    if(length(dependencies) > 0) {
      array <- rbind(unlist(dependencies), name, deparse.level = 0)
      from <- c(from, array[1, ])
      to <- c(to, array[2, ])
    }
  }
  list(
    from = from,
    to = to
  )
}

.topological_sort <- function(graph) {
  if(length(graph$from) > 0)
    pooh::tsort(graph$from, graph$to)
}
