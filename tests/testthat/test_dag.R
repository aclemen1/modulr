context("dag")

test_that(".build_dependency_graph can return no dependency", {
  reset()
  expect_equal(nrow(.build_dependency_graph()), 0)
  expect_error(.build_dependency_graph("undefined/module"))
})

test_that(".build_dependency_graph figures out the DAG of dependencies", {
  define("module_1", NULL, function() NULL)
  define("module_2", list("module_1"), function(m1) NULL)
  define("module_3", list("module_2"), function(m2) NULL)
  define("module_4", list("module_1", "module_2"), function(m1, m2) NULL)
  expect_equal(
    nrow(.build_dependency_graph(setNames("module_1", "module_1"))),
    0)
  expect_equal(
    .build_dependency_graph(setNames(c("module_1", "module_2"),
                                     c("module_1", "module_2"))),
    data.frame(
      module = "module_2",
      dependency = "module_1",
      stringsAsFactors = F))
  expect_equal(
    .build_dependency_graph(setNames(c("module_2", "module_3"),
                                     c("module_2", "module_3"))),
    data.frame(
      module = "module_3",
      dependency = "module_2",
      stringsAsFactors = F))
  expect_equal(
    .build_dependency_graph(
      setNames(c("module_4", "module_3", "module_2", "module_1"),
               c("module_4", "module_3", "module_2", "module_1"))),
    data.frame(
      module = c("module_4", "module_4", "module_3", "module_2"),
      dependency = c("module_2", "module_1", "module_2", "module_1"),
      stringsAsFactors = F))

})

test_that(".topological_sort is NULL on singletons", {
  reset()
  graph <- .build_dependency_graph(setNames(MODULR_NAME, MODULR_NAME))
  expect_null(.topological_sort(graph))
})

test_that(".topological_sort has two lines on pairs", {
  reset()
  define("module_1", list(MODULR_NAME), function(m) NULL)
  graph <- .build_dependency_graph(setNames(c("module_1", MODULR_NAME),
                                            c("module_1", MODULR_NAME)))
  expect_equal(
    .topological_sort(graph),
    c(MODULR_NAME, "module_1"))
})
