#' Unit tests.
#'
#' @export
run_tests <- function() {
  # TODO: forker https://github.com/cran/RUnit
  # et dans R/runit.r, adapter .sourceTestFile()
  # pour qu'un tangle ait lieu sur les Rmd

  runit_options <- getOption("RUnit")
  #runit_options$silent = T
  #runit_options$verbose = 0
  options("RUnit" = runit_options)
  suites <- paths_config$get_all()
  for(suite_name in names(suites)) {
    suite_path <- suites[[suite_name]]
    test_suite <- defineTestSuite(
      suite_name,
      dirs = suite_path,
      testFileRegexp = ".+\\.R$",
      testFuncRegexp = "^test\\..+")
    test_result <- runTestSuite(test_suite)
    printTextProtocol(test_result)
  }
}
