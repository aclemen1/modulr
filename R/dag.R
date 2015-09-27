# We need to figure out the directed acyclic graph (DAG) of the dependencies.
.build_dependency_graph <- function(all_dependencies = NULL) {

  assert_that(is.null(all_dependencies) ||
                            is.character(all_dependencies))

  dependency <- c()
  module <- c()

  for (name in all_dependencies) {

    assert_that(.is_defined(name))

    dependencies <- modulr_env$register[[c(name, "dependencies")]]

    if (isTRUE(length(dependencies) > 0)) {

      array <- rbind(unlist(lapply(dependencies, .resolve_mapping, name)),
                     name, deparse.level = 0)

      dependency <- c(dependency, array[1, ])
      module <- c(module, array[2, ])

    }

  }

  data.frame(
    module = module,
    dependency = dependency,
    stringsAsFactors = FALSE
  )

}

.topological_sort <- function(graph) {

  assert_that(
    is.data.frame(graph),
    nrow(graph) == 0 || setequal(names(graph), c("module", "dependency"))
  )

  if (nrow(graph) > 0) {
    pooh::tsort(graph$dependency, graph$module)
  }

}

.compute_adjacency_matrix <- function(group) {

  assert_that(is.null(group) || is.character(group))

  table(

    Reduce(rbind, Map(function(name) {

      deps <- factor(unlist(modulr_env$register[[c(name, "dependencies")]]),
                     levels = group)

      data.frame(from=factor(rep(name, length(deps)), levels = group), to=deps)

    },

    group)))

}
