increment_skipCalls_ <- function(args, increment = 1L) {
  if (!is.null(args[["skipCalls"]]) && is.numeric(args[["skipCalls"]])) {
    args[["skipCalls"]] <- args[["skipCalls"]] + increment
  } else {
    args[["skipCalls"]] <- increment
  }
  args
}

#' Environment, Module, and Pipe Browser.
#'
#' Interrupt the execution of an expression or a pipe and allow the inspection
#' of the environment where browser was called from.
#'
#' @param ... Further arguments to be passed to \code{\link[base]{browser}}.
#'
#' @details
#'
#' See \code{\link[base]{browser}}.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' (function() {
#'   "foobar" %>%
#'     browser() %>%
#'     print
#' })()}
#'
#' reset()
#' define("foo", NULL, function() {foo <- "bar"; browser()})
#' make()
#'
#' reset()
#' define("foobar", NULL, function() {
#'   library(magrittr)
#'   "foobar" %>%
#'     browser %>%
#'     print
#' })
#' \dontrun{make()}
#' @export
browser <- function(...) {
  if (!interactive()) {
    if (exists("function_list", where = parent.frame(2L)))
      return(list(...)[[1L]])
  } else {
    if (exists(".__name__", where = parent.frame(1L))) {
      module_name <- get(".__name__", pos = parent.frame(1L))
      if (module_name != "__main__") get_breadcrumbs(NULL, verbose = TRUE)
    }
    if (exists("function_list", where = parent.frame(2L))) {
      message(
        "Use ", sQuote("."), " to get the left-hand side value of the pipe.")
      # args <- list(...)
      # args[["skipCalls"]] <- 8L
      args <- increment_skipCalls_(list(...), 8L)
      on.exit(return(args[[1L]]))
      do.call(base::browser, args = tail(args, -1L), envir = parent.frame(1L))
    } else {
      # args <- list(...)
      # args[["skipCalls"]] <- 2L
      args <- increment_skipCalls_(list(...), 2L)
      do.call(base::browser, args = args, envir = parent.frame(1L))
    }
  }
}
