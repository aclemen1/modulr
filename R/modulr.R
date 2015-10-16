#' modulr -- A Dependency Injection Framework for R
#'
#' @description
#' This package contains a Dependency Injection framework for R. It allows to
#' organize programs into discrete, modular, and loosely coupled units. By
#' nature, such modules are easy to develop, debug, test, reuse, share and
#' maintain in a wide range of common situations. This package may also allow
#' for higher compliance with best practices in software engineering, provably
#' within small- to mid-sized teams of developers and R users.
#'
#' @details
#' To learn more about modulr, start with the vignettes and use
#' \code{browseVignettes(package = "modulr")}.
#'
#' For a complete list of functions,
#' use \code{library(help = "modulr")}.
#'
#' @docType package
#' @name modulr
#' @author Alain Clément-Pavon <\email{alain.clement-pavon@@unil.ch}>
NULL

modulr_env <- new.env(parent = emptyenv())

#' @name .Last.name
#' @aliases .Last.name
#' @rdname last_name
#' @title Name of Last Used Module.
#' @description The name of the last used module.
#' @usage .Last.name
#' @details
#' A module is considered used last if it has just been:
#' \itemize{
#'  \item defined or imported with a name without special meaning and not
#'    reserved (see \code{\link{define}}),
#'  \item made or touched with a non-reserved name.
#' }
#' Other actions on modules do not alter the value of \code{.Last.name}.
#' @section Warning:
#' Do not assign to \code{.Last.name} in the workspace, because this will always
#' mask the object of the same name in package:modulr.
#' @return The name of the last used module.
#' @seealso \code{\link{define}}.
#' @examples
#' define("foo", NULL, function() NULL)
#' .Last.name
#' define("bar/test", NULL, function() NULL)
#' .Last.name
#' make("bar/test")
#' .Last.name
globalVariables(c(".Last.name"))

if (packageVersion("assertthat") >= package_version("0.1.0.99")) {
  assert_that <- assertthat::assert_that
} else {
  assert_that <- function(..., msg) {
    do.call(assertthat::assert_that, list(...), envir = parent.frame())
  }
}

#' Set and Get Verbosity Level.
#'
#' Set and get the global verbosity level.
#'
#' @param level A scalar (integer vector of length one),
#'  possibly \code{-Inf} or \code{+Inf}.
#'
#' @details
#'
#' Messages are generated, accordingly to the following levels:
#'
#' \describe{
#' \item{Level 0.}{None.}
#' \item{Level 1.}{Operations modifying the internal state.}
#' \item{Level 2 (default).}{All operations.}
#' \item{Level \code{+Inf}.}{Debugging informations.}
#' }
#'
#' @seealso \code{\link{define}}, \code{\link{make}}, \code{\link{reset}}, and
#'   \code{\link{touch}}.
#'
#' @examples
#' reset()
#' set_verbosity(+Inf)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' reset()
#' set_verbosity(2)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' reset()
#' set_verbosity(1)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' reset()
#' set_verbosity(0)
#' define("foo", NULL, function() "Hello World")
#' define("bar", list(f = "foo"), function(f) sprintf("*%s*", f))
#' make()
#' touch("foo")
#' make("bar")
#'
#' @export
set_verbosity <- function(level = 2) {

  assertthat::assert_that(assertthat::is.scalar(level))

  modulr_env$verbosity <- level

}

#' @rdname set_verbosity
#' @export
get_verbosity <- function() {

  modulr_env$verbosity

}

# the base::get0 function exsists only since R 3.2
.get_0 <- function(var, ..., ifnotfound = NULL) {
  if (exists(var, ...)) get(var, ...) else ifnotfound
}

.dir_exists <- function(file) {
  isTRUE(file.info(file)[1, "isdir"])
}

PRAISE <- c(
  "Outstanding",
  "I'm so proud of you",
  "You're great",
  "Wonderful",
  "Great job",
  "Terrific",
  "You're super",
  "Great smile",
  "You're the best",
  "Perfect",
  "Way to go",
  "Good for you",
  "Fabulous",
  "You're delightful",
  "You did it",
  "You make me happy",
  "You're an inspiration",
  "Great",
  "Excellent",
  "Thanks for sharing",
  "Super work",
  "Marvelous",
  "I trust you",
  "You're getting there",
  "Fantastic",
  "You're special",
  "You deserve a star",
  "Very good",
  "I'm impressed",
  "Exceptional",
  "Thanks for caring",
  "You're very responsible",
  "You're a joy to be around",
  "You're tops",
  "Nice work",
  "You're a gem",
  "Dynamite",
  "Hurray for you",
  "You're so creative",
  "You're a champ",
  "Beautiful",
  "Great imagination",
  "You'll get it",
  "Keep up the good work",
  "You're very brave",
  "Good sport",
  "Sounds great",
  "You've got what it takes",
  "You're #1",
  "How clever",
  "How thoughtful",
  "You're on the mark",
  "You're the greatest",
  "I've got faith in you",
  "Well done",
  "How artistic",
  "What careful work",
  "Exceptional",
  "That's neat",
  "Wonderful imagination",
  "You're right",
  "You brighten my day",
  "Delightful idea",
  "Super job")
