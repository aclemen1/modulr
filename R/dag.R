# We need to figure out the directed acyclic graph (DAG) of the dependencies.
.build_dependency_graph <- function(all_dependencies = NULL) {

  assertthat::assert_that(is.null(all_dependencies) ||
                            is.character(all_dependencies))

  dependency <- c()
  module <- c()

  for (name in all_dependencies) {

    dependencies <- .internals()$register[[name]]$dependencies

    if(isTRUE(length(dependencies) > 0)) {

      array <- rbind(unlist(lapply(dependencies, .resolve_mapping, name)),
                     name, deparse.level = 0)

      dependency <- c(dependency, array[1, ])
      module <- c(module, array[2, ])

    }

  }

  data.frame(
    module = module,
    dependency = dependency,
    stringsAsFactors = F
  )

}

# A topological sort, grouped by independent modules into layers.
.topological_sort_with_layer <- function(graph) {

  assertthat::assert_that(
    is.data.frame(graph),
    nrow(graph) == 0 || setequal(names(graph), c("module", "dependency"))
  )

  if(nrow(graph) > 0) {

    node <- unique(unlist(graph, use.names = F))
    node_length <- length(node)

    nodes <-
      data.frame(
        node = node,
        dependency = node,
        deps_idx = 1,
        stringsAsFactors = F)

    while(!all(is.na(nodes$dependency))) {

      names(nodes)[names(nodes) == "dependency"] <- "module"
      nodes <- merge(nodes, graph, by = "module", all.x = T)
      nodes <- nodes[, names(nodes) != "module"]
      nodes <- transform(
        nodes,
        deps_idx = ifelse(
          is.na(nodes$dependency), nodes$deps_idx, nodes$deps_idx + 1))

      if(max(nodes$deps_idx) > node_length)
        stop("Cycle detected.", call. = F)

    }

    nodes <- aggregate(deps_idx ~ node, data = nodes, max)
    names(nodes)[names(nodes) == "deps_idx"] <- "layer"
    nodes <- nodes[order(nodes$layer),]
    row.names(nodes) <- seq_len(nrow(nodes))

    return(nodes)

  }

}

.topological_sort_by_layers <- function(graph) {

  assertthat::assert_that(
    is.data.frame(graph),
    nrow(graph) == 0 || setequal(names(graph), c("module", "dependency"))
  )

  if(nrow(graph) > 0) {

    nodes <- .topological_sort_with_layer(graph)

    layers <- split(nodes$node, nodes$layer)

    return(layers)

  }

}

.compute_adjacency_matrix <- function(group) {

  assertthat::assert_that(is.null(group) || is.character(group))

  register <- .internals()$register

  table(

    Reduce(rbind, Map(function(name) {

      deps <- factor(unlist(register[[name]]$dependencies), levels = group)

      data.frame(from=factor(rep(name, length(deps)), levels = group), to=deps)

    },

    group)))

}
