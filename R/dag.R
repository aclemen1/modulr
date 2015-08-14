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
  data.frame(
    from = from,
    to = to,
    stringsAsFactors = F
  )
}

.topological_sort_with_layer <- function(graph) {
  if(nrow(graph)) {
    node <- unique(unlist(graph, use.names = F))
    node_length <- length(node)

    nodes <- data.frame(node = node,
                        from = node,
                        deps_idx = 1,
                        stringsAsFactors = F)
    while(!all(is.na(nodes$from))) {
      names(nodes)[names(nodes)=="from"] <- "to"
      nodes <- merge(nodes, graph, by = "to", all.x = T)
      nodes <- subset(nodes, select = -to)
      nodes <- transform(nodes, deps_idx = ifelse(is.na(from), deps_idx, deps_idx + 1))
      #     nodes <- nodes %>%
      #       rename(to = from) %>%
      #       left_join(graph, by = "to") %>%
      #       select(-to) %>%
      #       mutate(deps_idx = ifelse(is.na(from), deps_idx, deps_idx + 1))
      if(max(nodes$deps_idx) > node_length) {
        stop("Cycle detected.")
      }
    }
    nodes <- aggregate(deps_idx ~ node, data = nodes, max)
    names(nodes)[names(nodes)=="deps_idx"] <- "layer"
    nodes <- nodes[order(nodes$layer),]
    #   nodes <- nodes %>%
    #     select(-from) %>%
    #     group_by(node) %>%
    #     summarize(layer = max(deps_idx)) %>%
    #     ungroup %>%
    #     arrange(layer)

    nodes
  }
}

.topological_sort_by_layers <- function(graph) {
  if(nrow(graph)) {
    nodes <- .topological_sort_with_layer(graph)
    layers <- with(nodes, split(node, layer))
    layers
  }
}

.topological_sort <- function(graph) {
  .topological_sort_with_layer(graph)$node
}

