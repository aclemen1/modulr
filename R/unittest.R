#' Unit tests.
#'
#' @export
run_tests <- function(module_name) {
  # TODO: forker https://github.com/cran/RUnit
  # et dans R/runit.r, adapter .sourceTestFile()
  # pour qu'un tangle ait lieu sur les Rmd

  runit_options <- getOption("RUnit")
  runit_options$silent = T
  runit_options$verbose = 0
  options("RUnit" = runit_options)
  if(missing(module_name)) {
    suites <- paths_config$get_all()
    errors <- 0
    failures <- 0
    for(suite_name in names(suites)) {
      suite_path <- suites[[suite_name]]
      test_suite <- RUnit::defineTestSuite(
        suite_name,
        dirs = unique(dirname(list.files(suite_path,
                                         recursive = T,
                                         full.names = T,
                                         all.files = F))),
        testFileRegexp = ".*[^_]\\.R$",
        testFuncRegexp = "^test\\..+")
      test_result <- RUnit::runTestSuite(test_suite)
      errors <- errors + test_result[[suite_name]]$nErr
      failures <- failures + test_result[[suite_name]]$nFail
      RUnit::printTextProtocol(test_result)
    }
  } else {
    errors <- 0
    failures <- 0
    file <- .resolve_path(module_name)
    test_suite <- RUnit::defineTestSuite(
      module_name,
      dirs = dirname(file),
      testFileRegexp = sprintf("^%s.R$", basename(file)),
      testFuncRegexp = "^test\\..+")
    test_result <- RUnit::runTestSuite(test_suite)
    errors <- errors + test_result[[module_name]]$nErr
    failures <- failures + test_result[[module_name]]$nFail
    RUnit::printTextProtocol(test_result)
  }
  if(errors + failures) stop(.call = F)
}
