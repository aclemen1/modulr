#' Stash and Unstash Modules. List and Remove Stashes.
#'
#' Stash and unstash all modules and internal state. List and remove stashes.
#'
#' @param comment A string (character vector of length one).
#'
#' @return A scalar (integer vector of length one) containing the stash ID.
#'
#' @details
#'
#' \code{stash} allows to stash all modules and the internal state of modulr.
#' Each stash is pushed on a stack and can be commented for later reference.
#' \code{unstash} pops a stash from the stack and restores the state
#' accordingly. \code{list_stashes} lists the stack, showing ids, timestamps and
#' comments for each stash. \code{remove_stash} removes one or all stashes from
#' the stack.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{define}}, \code{\link{list_modules}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' reset()
#' remove_stash(all = TRUE)
#' define("foo", NULL, function() NULL)
#' stash("'foo' only")
#' list_stashes()
#' define("bar", NULL, function() NULL)
#' stash("'foo' and 'bar'")
#' list_stashes()
#' list_modules()
#' unstash(1L)
#' list_modules()
#'
#' @export
stash <- function(comment = NA_character_) {

  .message_meta("Entering stash() ...",
                verbosity = +Inf)

  assert_that(is.na(comment) || assertthat::is.string(comment),
                          msg = "comment is not a string.")

  timestamp <- Sys.time()

  len_plus_1 <- length(.modulr_env$injector$stash) + 1L

  .message_meta(
    sprintf("Stashing modulr state, stash #%s", len_plus_1), {
      .modulr_env$injector$stash[[len_plus_1]] <- list(
        registry = .modulr_env$injector$registry,
        .Last.name = .modulr_env$injector$.Last.name,
        config = .modulr_env$injector$config,
        verbosity = .modulr_env$injector$verbosity,
        timestamp = timestamp,
        comment = comment
      )
    },
    ok = TRUE, verbosity = 2L)

  return(invisible(len_plus_1))

}

#' @rdname stash
#' @param id A scalar (integer vector of length one).
#' @export
unstash <- function(id = length(.modulr_env$injector$stash)) {
  .message_meta("Entering unstash() ...",
                verbosity = +Inf)

  len <- length(.modulr_env$injector$stash)

  assert_that(len > 0L, msg = "nothing to unstash.")
  assert_that(assertthat::is.count(id))
  assert_that(id <= len, msg = "id is out of bounds.")

  stash <- .modulr_env$injector$stash[[id]]

  .message_meta(
    sprintf("Unstashing modulr state, stash #%s", id), {

      .modulr_env$injector$stash <- .modulr_env$injector$stash[-id]
      .modulr_env$injector$registry <- stash[["registry"]]
      .modulr_env$injector$.Last.name <- stash[[".Last.name"]]
      .modulr_env$injector$config <- stash[["config"]]
      .modulr_env$injector$verbosity <- stash[["verbosity"]]

    },
    ok = TRUE, verbosity = 2L)

  invisible()

}

#' @rdname stash
#' @export
list_stashes <- function() {

  .message_meta("Entering list_stashes() ...",
                verbosity = +Inf)

  if (length(.modulr_env$injector$stash) == 0L) {
    message("No stash found.")
    return(invisible())
  }

  timestamps <-
    format(
      do.call(c, Map(function(stash) stash[["timestamp"]],
                     .modulr_env$injector$stash)),
      "%c")

  comments <-
    do.call(c, Map(function(stash) stash[["comment"]],
                   .modulr_env$injector$stash))

  ids <- seq_along(timestamps)

  data.frame(id = ids, timestamp = timestamps, comment = comments,
             stringsAsFactors = FALSE)

}

#' @rdname stash
#' @inheritParams unstash
#' @param all A flag. Should all stashes be removed?
#' @export
remove_stash <- function(id, all) {

  .message_meta("Entering remove_stash() ...",
                verbosity = +Inf)

  assert_that(
    (missing(id) & !missing(all)) | (!missing(id) & missing(all)),
    msg = "call is ambiguous (two arguments passed)."
  )

  if (!missing(all)) {

    assert_that(
      assertthat::is.flag(all))
    assert_that(all, msg = "all is not TRUE.")

    .message_meta("Removing all stashes", {

      .modulr_env$injector$stash <- list()

    },
    ok = TRUE, verbosity = 2L)

  }

  if (!missing(id)) {

    assert_that(
      assertthat::is.count(id))
    assert_that(
      id <= length(.modulr_env$injector$stash),
      msg = "id is out of bounds.")

    .message_meta(
      sprintf("Removing stash #%s", id), {

        .modulr_env$injector$stash[[id]] <- NULL

      },
      ok = TRUE, verbosity = 2L)

  }

  invisible()

}
