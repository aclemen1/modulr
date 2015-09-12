context("graph")

test_that("correct Sankey graph is returned for dependent modules", {
  if (!requireNamespace("networkD3", quietly = TRUE))
    skip("networkD3 not installed")
  reset()
  define("module_layer1_1", NULL, function() NULL)
  define("module_layer1_2", NULL, function() NULL)
  define("module_layer1_3", NULL, function() NULL)
  define("module_layer2_1", list("module_layer1_1", "module_layer1_2"),
         function(m1, m2) NULL)
  define("module_layer3_1", list("module_layer2_1", "module_layer1_3"),
         function(m1, m2) NULL)

  graph_1 <- graph_dependencies("module_layer3_1")

  expect_equal(
    graph_1$x[c("links", "nodes")],
    structure(list(links = structure(list(source = c(3, 4, 2, 1), target = c(2, 2, 0, 0), value = c(1, 1, 1, 1)), .Names = c("source", "target", "value"), row.names = c(NA, -4L), class = "data.frame"), nodes = structure(list(name = structure(c(5L, 3L, 4L, 1L, 2L), .Label = c("module_layer1_1", "module_layer1_2", "module_layer1_3", "module_layer2_1", "module_layer3_1"), class = "factor")), .Names = "name", row.names = c(NA, -5L), class = "data.frame")), .Names = c("links", "nodes"))) # Exclude Linting

  graph_2 <- graph_dependencies("module_layer2_1")

  expect_equal(
    graph_2$x[c("links", "nodes")],
    structure(list(links = structure(list(source = c(1,2), target = c(0, 0), value = c(1, 1)), .Names = c("source", "target", "value"), row.names = c(NA, -2L), class = "data.frame"), nodes = structure(list(name = structure(c(3L, 1L, 2L), .Label = c("module_layer1_1", "module_layer1_2", "module_layer2_1"), class = "factor")), .Names = "name", row.names = c(NA, -3L), class = "data.frame")), .Names = c("links", "nodes"))) # Exclude Linting

  })

test_that("correct Sankey graph is returned for the whole register", {
  if (!requireNamespace("networkD3", quietly = TRUE))
    skip("networkD3 not installed")
  reset()
  define("module_layer1_1", list(modulr = "modulr"), function(modulr) NULL)
  define("module_layer1_2", NULL, function() NULL)
  define("module_layer1_3", NULL, function() NULL)
  define("module_layer2_1", list("module_layer1_1", "module_layer1_2"),
         function(m1, m2) NULL)
  define("module_layer3_1", list("module_layer2_1", "module_layer1_3"),
         function(m1, m2) NULL)

  expect_equal(graph_dependencies(), graph_dependencies("module_layer3_1"))
  expect_equal(graph_dependencies(reserved = FALSE),
               graph_dependencies("module_layer3_1", reserved = FALSE))

})

test_that("an error message is raised for an undefined module name", {
  if (!requireNamespace("networkD3", quietly = TRUE))
    skip("networkD3 not installed")
  reset()
  define("module_layer1_1", NULL, function() NULL)

  expect_error(graph_dependencies("undefined_module"))
})

test_that("graph_dependencies calls are warned from within a module", {
  if (!requireNamespace("networkD3", quietly = TRUE))
    skip("networkD3 not installed")
  reset()
  define("module", NULL, function() {
    graph_dependencies()
  })
  expect_warning(make("module"))
})
