context("plot")

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

  graph_1 <- plot_dependencies("module_layer3_1")

  # Begin Exclude Linting
  expect_equal(
    graph_1$x[c("links", "nodes")],
    structure(list(links = structure(list(source = c(0, 1, 2, 3),
      target = c(2, 2, 4, 4), value = c(1, 1, 1, 1)), .Names = c("source",
      "target", "value"), row.names = c(NA, -4L), class = "data.frame"),
      nodes = structure(list(name = structure(c(1L, 2L, 4L, 3L,
      5L), .Label = c("module_layer1_1", "module_layer1_2", "module_layer1_3",
      "module_layer2_1", "module_layer3_1"), class = "factor"),
          group = structure(c(1L, 2L, 4L, 3L, 5L), .Label = c("module_layer1_1",
          "module_layer1_2", "module_layer1_3", "module_layer2_1",
          "module_layer3_1"), class = "factor")), .Names = c("name",
      "group"), row.names = c(NA, -5L), class = "data.frame")),
      .Names = c("links", "nodes")))
  # End Exclude Linting


  graph_2 <- plot_dependencies("module_layer2_1")

  # Begin Exclude Linting
  expect_equal(
    graph_2$x[c("links", "nodes")],
    structure(list(links = structure(list(source = c(0, 1), target = c(2,
2), value = c(1, 1)), .Names = c("source", "target", "value"), row.names = c(NA,
-2L), class = "data.frame"), nodes = structure(list(name = structure(1:3, .Label = c("module_layer1_1",
"module_layer1_2", "module_layer2_1"), class = "factor"), group = structure(1:3, .Label = c("module_layer1_1",
"module_layer1_2", "module_layer2_1"), class = "factor")), .Names = c("name",
"group"), row.names = c(NA, -3L), class = "data.frame")), .Names = c("links",
"nodes")))
  # End Exclude Linting

})

test_that("correct Sankey graph is returned for the whole registry", {
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

  expect_equal(plot_dependencies(), plot_dependencies("module_layer3_1"))
  expect_equal(plot_dependencies(reserved = FALSE),
               plot_dependencies("module_layer3_1", reserved = FALSE))

})

test_that("an error message is raised for an undefined module name", {
  if (!requireNamespace("networkD3", quietly = TRUE))
    skip("networkD3 not installed")
  reset()
  define("module_layer1_1", NULL, function() NULL)

  expect_error(plot_dependencies("undefined_module"))
})

test_that("plot_dependencies calls are warned from within a module", {
  if (!requireNamespace("networkD3", quietly = TRUE))
    skip("networkD3 not installed")
  reset()
  define("module", NULL, function() {
    plot_dependencies()
  })
  expect_warning(make("module"))
})
