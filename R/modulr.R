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

RESERVED_NAMES <- c("modulr")

modulr_env <- new.env(parent = emptyenv())

#' @export
# TODO: write documentation
set_verbosity <- function(value) {

  .message_meta("Entering set_verbosity() ...",
                verbosity = +Inf)

  assertthat::assert_that(assertthat::is.scalar(value))

  assign("verbosity", value, pos = modulr_env)

}

# the base::get0 function exsists only since R 3.2
.get_0 <- function(var, ..., ifnotfound = NULL) {
  if(exists(var, ...)) get(var, ...) else ifnotfound
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
