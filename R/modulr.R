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

  assertthat::assert_that(assertthat::is.scalar(value))

  assign("verbosity", value, pos = modulr_env)

}

#' @export
# TODO: test that
# TODO: write documentation
stash <- function(comment = NA_character_) {

  assertthat::assert_that(is.na(comment) || assertthat::is.string(comment),
                          msg = "comment is not a string.")

  timestamp <- Sys.time()

  len_plus_1 <- length(modulr_env$stash) + 1

  .message_meta(sprintf("Stashing modulr state, stash #%s ...",
                        len_plus_1), verbosity = 2)

  modulr_env$stash[[len_plus_1]] <- list(
    register = modulr_env$register,
    .Last.name = modulr_env$.Last.name,
    config = modulr_env$config,
    verbosity = modulr_env$verbosity,
    timestamp = timestamp,
    comment = comment
  )

  invisible()

}

#' @export
# TODO: test that
# TODO: write documentation
unstash <- function(id = length(modulr_env$stash)) {

  len <- length(modulr_env$stash)

  assertthat::assert_that(len > 0, msg = "nothing to unstash.")
  assertthat::assert_that(assertthat::is.count(id))
  assertthat::assert_that(id <= len, msg = "id is out of bounds.")

  stash <- modulr_env$stash[[len]]

  .message_meta(sprintf("Unstashing modulr state, stash #%s ...", id),
                verbosity = 2)

  modulr_env$stash <- modulr_env$stash[-id]
  modulr_env$register <- stash[["register"]]
  modulr_env$.Last.name <- stash[[".Last.name"]]
  modulr_env$config <- stash[["config"]]
  modulr_env$verbosity <- stash[["verbosity"]]

  invisible()

}

#' @export
# TODO: test that
# TODO: write documentation
list_stashes <- function() {

  if(length(modulr_env$stash) == 0) {
    message("No stash found.")
    return(invisible())
  }

  timestamps <-
    format(
      do.call(c, Map(function(stash) stash$timestamp, modulr_env$stash)),
      "%c")

  comments <-
    do.call(c, Map(function(stash) stash$comment, modulr_env$stash))

  ids <- seq_along(timestamps)

  data.frame(id = ids, timestamp = timestamps, comment = comments,
             stringsAsFactors = F)

}

#' @export
# TODO: test that
# TODO: write documentation
remove_stash <- function(id, all) {

  assertthat::assert_that(
    (missing(id) & !missing(all)) | (!missing(id) & missing(all)),
    msg = "call is ambiguous (two arguments passed)."
    )

  if(!missing(all)) {

    assertthat::assert_that(
      assertthat::is.flag(all))
    assertthat::assert_that(all, msg = "all is not TRUE.")

    .message_meta("Removing all stashes ...", verbosity = 2)
    modulr_env$stash <- list()

  }

  if(!missing(id)) {

    assertthat::assert_that(
      assertthat::is.count(id))
    assertthat::assert_that(
      id <= length(modulr_env$stash),
      msg = "id is out of bounds.")

    .message_meta(sprintf("Removing stash #%s ...",
                          id), verbosity = 2)

    modulr_env$stash[[id]] <- NULL

  }

  invisible()
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
