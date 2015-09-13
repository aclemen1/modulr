#' @export
# TODO: test that
# TODO: write documentation
stash <- function(comment = NA_character_) {

  .message_meta("Entering stash() ...",
                verbosity = +Inf)

  assertthat::assert_that(is.na(comment) || assertthat::is.string(comment),
                          msg = "comment is not a string.")

  timestamp <- Sys.time()

  len_plus_1 <- length(modulr_env$stash) + 1

  .message_meta(sprintf("Stashing modulr state, stash #%s ...",
                        len_plus_1), verbosity = 2)

  modulr_env$stash[[len_plus_1]] <- list(
    register = modulr_env$register,
    .Last.name = modulr_env$.Last.name, # Exclude Linting
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
  .message_meta("Entering unstash() ...",
                verbosity = +Inf)

  len <- length(modulr_env$stash)

  assertthat::assert_that(len > 0, msg = "nothing to unstash.")
  assertthat::assert_that(assertthat::is.count(id))
  assertthat::assert_that(id <= len, msg = "id is out of bounds.")

  stash <- modulr_env$stash[[id]]

  .message_meta(sprintf("Unstashing modulr state, stash #%s ...", id),
                verbosity = 2)

  modulr_env$stash <- modulr_env$stash[-id]
  modulr_env$register <- stash[["register"]]
  modulr_env$.Last.name <- stash[[".Last.name"]] # Exclude Linting
  modulr_env$config <- stash[["config"]]
  modulr_env$verbosity <- stash[["verbosity"]]

  invisible()

}

#' @export
# TODO: test that
# TODO: write documentation
list_stashes <- function() {

  .message_meta("Entering list_stashes() ...",
                verbosity = +Inf)

  if(length(modulr_env$stash) == 0) {
    message("No stash found.")
    return(invisible())
  }

  timestamps <-
    format(
      do.call(c, Map(function(stash) stash[["timestamp"]], modulr_env$stash)),
      "%c")

  comments <-
    do.call(c, Map(function(stash) stash[["comment"]], modulr_env$stash))

  ids <- seq_along(timestamps)

  data.frame(id = ids, timestamp = timestamps, comment = comments,
             stringsAsFactors = FALSE)

}

#' @export
# TODO: test that
# TODO: write documentation
remove_stash <- function(id, all) {

  .message_meta("Entering remove_stash() ...",
                verbosity = +Inf)

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
