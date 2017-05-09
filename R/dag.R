# We need to figure out the directed acyclic graph (DAG) of the dependencies.
.build_dependency_graph <- function(named_dependencies = NULL) {

  assert_that(
    is.null(named_dependencies) ||
      (is.character(named_dependencies) && !is.null(names(named_dependencies))))

  if (is.null(named_dependencies)) return(data.frame())

  dependency <- c()
  module <- c()

  named_dependencies <- unique(as.array(named_dependencies))

  names_of_deps <- names(named_dependencies)

  for (name in named_dependencies) {

    assert_that(.is_defined(name))

    dependencies <- .modulr_env$injector$registry[[c(name, "dependencies")]]
    resolved_dependencies <-
      unique(unname(named_dependencies[names_of_deps %in% dependencies]))

    if (isTRUE(length(resolved_dependencies) > 0L)) {

      array <-
        rbind(resolved_dependencies,
              name, deparse.level = 0L)

      dependency <- c(dependency, array[1L, ])
      module <- c(module, array[2L, ])

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
    nrow(graph) == 0L || setequal(names(graph), c("module", "dependency"))
  )

  if (nrow(graph) > 0L) {

    pooh::tsort(graph$dependency, graph$module)

  }

}

.topological_sort_by_layers <- function(graph) {

  assert_that(
    is.data.frame(graph),
    nrow(graph) == 0L || setequal(names(graph), c("module", "dependency"))
  )

  if (nrow(graph) > 0L) {

    ordered_names <- .topological_sort(graph)

    deps <- Map(
      function(name) {
        deps_ <- graph[graph[["module"]] == name, ][["dependency"]]
        if (length(deps_) == 0L) return(NULL)
        deps_
      },
      ordered_names)

    layers <- list()

    while (length(deps) > 0L) {

      idx <- vapply(
        seq_len(length(deps)),
        FUN = function(n) any(deps[[n]] %in% names(deps)[1L:n]),
        FUN.VALUE = TRUE)

      layers[[length(layers) + 1L]] <- names(deps)[!idx]

      deps <- deps[idx]

    }

    return(layers)

  }

}

.compute_adjacency_matrix <- function(group) {

  assert_that(is.null(group) || is.character(group))

  table(

    Reduce(rbind, Map(function(name) {

      deps <-
        factor(unlist(.modulr_env$injector$registry[[c(name, "dependencies")]]),
               levels = group)

      data.frame(
        from = factor(rep(name, length(deps)), levels = group),
        to = deps
      )

    },

    group)))

}
