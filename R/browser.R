prompt_ <- function(fun) {
  paste(paste(">", deparse(body(fun), width.cutoff = 78L)), collapse = "\n")
}

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
#'     browser() %>%
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
      function_list_ <- get("function_list", pos = parent.frame(2L))
      i_ <- get("i", pos = parent.frame(2L))
      k_ <- get("k", pos = parent.frame(2L))
      message(paste(unlist(c(
        if (i_ %in% 1L:2L) list("> .") else if (i_ > 2L) list("> ..."),
        lapply(function_list_[max(1L, i_ - 1L):min(i_ + 1L, k_)], prompt_),
        if (i_ < k_ - 1L) list("> ...")
        )),
        collapse = " %>% \n"))
      args <- increment_skipCalls_(list(...), 3L)
      do.call(
        function(...) {
          do.call(base::browser, args = list(...), envir = parent.frame(1L))
          return(args[[1L]])
        },
        args = utils::tail(args, -1L), envir = parent.frame(1L))
    } else {
      args <- increment_skipCalls_(list(...), 2L)
      do.call(base::browser, args = args, envir = parent.frame(1L))
    }
  }
}
