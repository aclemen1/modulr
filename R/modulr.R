#' modulr -- Module Pattern and DI in R
#'
#' modulr is a Module Pattern and Dependency Injection implementation in R.
#' Module Pattern and DI allows you to encapsulate pieces of code into useful singleton units,
#' namely modules that register their capabilities, export values and rely on other modules as dependencies.
#' modulr is widely inspired from RequireJS and AngularJS for Javascript.
#'
#' @docType package
#' @name modulr
#' @author Alain Clément-Pavon <\email{alain.clement-pavon@@unil.ch}>

NULL

RESERVED_NAMES <- c("modulr")

modulr_env <- new.env()

install_error <- function() {
  current_error <- deparse(getOption("error"))
  options(error = function() {
    message("modulr error info: ")
    show_breadcrumbs()
    eval(parse(text = `current_error`))
  })
}
