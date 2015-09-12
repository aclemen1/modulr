context("dag")

test_that(".build_dependency_graph can return no dependency", {
  reset()
  expect_equal(nrow(.build_dependency_graph()), 0)
  expect_equal(nrow(.build_dependency_graph("undefined/module")), 0)
})

test_that(".build_dependency_graph figures out the DAG of dependencies", {
  reset()
  define("module_1", NULL, function() NULL)
  define("module_2", list("module_1"), function(m1) NULL)
  define("module_3", list("module_2"), function(m2) NULL)
  define("module_4", list("module_1", "module_2"), function(m1, m2) NULL)
  expect_equal(nrow(.build_dependency_graph("module_1")), 0)
  expect_equal(
    .build_dependency_graph("module_2"),
    data.frame(
      module = "module_2",
      dependency = "module_1",
      stringsAsFactors = F))
  expect_equal(
    .build_dependency_graph("module_3"),
    data.frame(
      module = "module_3",
      dependency = "module_2",
      stringsAsFactors = F))
  expect_equal(
    .build_dependency_graph(c("module_4", "module_3", "module_2")),
    data.frame(
      module = c("module_4", "module_4", "module_3", "module_2"),
      dependency = c("module_1", "module_2", "module_2", "module_1"),
      stringsAsFactors = F))

})

test_that(".topological_sort_with_layer is NULL on singletons", {
  reset()
  graph <- .build_dependency_graph("modulr")
  expect_null(.topological_sort_with_layer(graph))
  })

test_that(".topological_sort_with_layer has two lines on pairs", {
  reset()
  define("module_1", list("modulr"), function(m) NULL)
  graph <- .build_dependency_graph(c("module_1", "modulr"))
  expect_equal(
    .topological_sort_with_layer(graph),
    data.frame(
      node = c("modulr", "module_1"),
      layer = c(1, 2),
      stringsAsFactors = F))
})

test_that(".topological_sort_with_layer can return two layers on triples", {
  reset()
  define("module_1", list("modulr"), function(m) NULL)
  define("module_2", list("modulr"), function(m) NULL)
  graph <- .build_dependency_graph(c("module_2", "module_1", "modulr"))
  expect_equal(
    .topological_sort_with_layer(graph),
    data.frame(
      node = c("modulr", "module_1", "module_2"),
      layer = c(1, 2, 2),
      stringsAsFactors = F))
})

test_that(".topological_sort_with_layer can return three layers on triples", {
  reset()
  define("module_1", list("modulr"), function(m) NULL)
  define("module_2", list("module_1"), function(m) NULL)
  graph <- .build_dependency_graph(c("module_2", "module_1", "modulr"))
  expect_equal(
    .topological_sort_with_layer(graph),
    data.frame(
      node = c("modulr", "module_1", "module_2"),
      layer = c(1, 2, 3),
      stringsAsFactors = F))

  reset()
  define("module_1", list("modulr"), function(m) NULL)
  define("module_2", list("module_1", "modulr"), function(m1, m2) NULL)
  graph <- .build_dependency_graph(c("module_2", "module_1", "modulr"))
  expect_equal(
    .topological_sort_with_layer(graph),
    data.frame(
      node = c("modulr", "module_1", "module_2"),
      layer = c(1, 2, 3),
      stringsAsFactors = F))
})

test_that(".topological_sort_by_layers returns vectors for each layer", {
  reset()
  define("module_1", list("modulr"), function(m) NULL)
  define("module_1bis", list("modulr"), function(m) NULL)
  define("module_2", list("module_1"), function(m) NULL)
  graph <- .build_dependency_graph(
    c("module_2", "module_1bis", "module_1", "modulr"))
  expect_equal(
    .topological_sort_by_layers(graph),
    list(`1` = "modulr", `2` = c("module_1", "module_1bis"), `3` = "module_2"))
})
