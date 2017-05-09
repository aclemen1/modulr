#' Get Breadcrumbs.
#'
#' Get breadcrumbs and output a message.
#'
#' @param void Any object. Not used.
#' @param verbose A flag. Should a message be outputted?
#'
#' @return An ordered character vector containing the names of the nested
#'   modules.
#'
#' @details
#'
#' Breadcrumbs allow to keep track of locations within modules. This is
#' particularily useful to circumvent an error occurring in a chain of nested
#' dependent modules, for instance. See \code{\link{reactivate_breadcrumbs}}.
#'
#' @seealso \code{\link{reactivate_breadcrumbs}}, \code{\link{define}},
#' \code{\link{make}}, and \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() function() get_breadcrumbs())
#' define("bar", list(foo = "foo"), function(foo) function() foo())
#' define("foobar", list(bar = "bar"), function(bar) (bar()))
#' make()
#'
#' reset()
#' reactivate_breadcrumbs()
#' define("foo", NULL, function() function() stop("error in 'foo'"))
#' define("bar", list(foo = "foo"), function(foo) function() foo())
#' define("foobar", list(bar = "bar"), function(bar) bar())
#' \dontrun{make()}
#' @export
get_breadcrumbs <- function(void, verbose = TRUE) {

  assert_that(assertthat::is.flag(verbose))

  bc <- unique(c(
    "__main__",
    unique(
      unlist(
        Filter(function(x) !is.na(x),
               lapply(evalq(sys.frames(), envir = parent.frame(2L)),
                      function(frame) {
                        .get_0(".__name__", envir = frame,
                               ifnotfound = NA, inherits = TRUE)
                      }))))))

  if (length(bc) > 1L && verbose)
    message(sprintf("modulr breadcrumbs: %s",
                    paste(sprintf("'%s'", bc), collapse = " > ")))

  invisible(bc)

}

.is_installed_bc <- function(handler = getOption("error")) {

  assert_that(is.language(handler) || is.null(handler))

  if (is.null(handler)) return(FALSE)

  any(grepl("modulr\\:\\:get\\_breadcrumbs\\(\"installed\"\\)",
            format(handler)))

}

#' Re-Activate Breadcrumbs.
#'
#' Re-activate breadcrumbs messages on error.
#'
#' @details
#'
#' Breadcrumbs messages are activated by default when the modulr package is
#' loaded. This is done by binding a wrapper function to the
#' \code{\link[=options]{error}} option. The function wraps any other previously
#' binded function and the \code{\link{get_breadcrumbs}} function together.
#' Since IDEs like RStudio are likely to reset this option during the lifetime
#' of an R session, \code{reactivate_breadcrumbs} can be useful to re-install
#' the wrapper function.
#'
#' @examples
#' reset()
#' options(error = browser)
#' reactivate_breadcrumbs()
#' define("foo", NULL, function() function() stop("error in 'foo'"))
#' define("bar", list(foo = "foo"), function(foo) function() foo())
#' define("foobar", list(bar = "bar"), function(bar) bar())
#' \dontrun{make()}
#'
#' @export
reactivate_breadcrumbs <- function() {

  handler <- getOption("error")

  if (!.is_installed_bc(handler)) {

    wrapper <- function() {
      modulr::get_breadcrumbs("installed")
      eval(parse(text = deparse(handler)), envir = parent.frame())
      if (!interactive()) stop(call. = FALSE) # nocov
    }

    options(error = wrapper, show.error.locations = "top")

  }

  handler

}
