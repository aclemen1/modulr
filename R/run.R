#' Run a Module (experimental).
#'
#' Run a module as a standalone script. Asynchronous functions are based on
#' forking and so are not available on Windows.
#'
#' @inheritParams make
#' @param ... For \code{run} and \code{run_async}, further arguments to be
#'   passed for evaluation to the resulting function, if any (see
#'   \code{\link{make}}). For \code{do_run_async}, further arguments to be
#'   passed to \code{\link[parallel]{mcparallel}}.
#'
#' @details
#'
#' Experimental.
#'
#' @export
run <- function(name = .Last.name, ...) {

  if (.is_called_from_within_module()) {
    warning("run is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  do_run(name = name, args = list(...))

}

#' @rdname run
#' @inheritParams do_bundle
#' @export
do_run <- function(name = .Last.name, args = list(),
                   quote = FALSE, envir = parent.frame(1L),
                   pre_hook = NULL, post_hook = NULL) {

  if (.is_called_from_within_module()) {
    warning("do_run is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  capture.output(script <- do_bundle(
    name = name, args = args,
    quote = quote, envir = envir,
    pre_hook = pre_hook, post_hook = post_hook))

  (local(eval(parse(text = script))))

}

#' @rdname run
#' @export
do.run <- do_run

#' @rdname run
#' @export
run_async <- function(name = .Last.name, ...) {

  if (.is_called_from_within_module()) {
    warning("do_run_async is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  do_run_async(name = name, args = list(...))

}

#' @rdname run
#' @export
do_run_async <- function(name = .Last.name, args = list(),
                         quote = FALSE, envir = parent.frame(1L),
                         pre_hook = NULL, post_hook = NULL, ...) {

  if (.is_called_from_within_module()) {
    warning("do_run_async is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  on.exit(
    message(
      "Provided `job <- .Last.value`, use `job$collect()` ",
      "to check and collect the result. ",
      "Similarily, use `job$terminate()` ",
      "to terminate the parallel process."))

  timestamp <- Sys.time()
  job <- parallel::mcparallel(do_run(
    name = name, args = args,
    quote = quote, envir = envir,
    pre_hook = pre_hook, post_hook = post_hook),
    name = name, ...)
  result <- NULL
  done <- FALSE

  terminate_ <- function() {
    if (!is.null(job)) {
      for (i in c(1L:3L)) {
        if (tools::pskill(job$pid)) break
        Sys.sleep(i)
      }
      if (tools::pskill(job$pid, signal = tools::SIGKILL)) {
        job <<- NULL
        return(TRUE)
      }
    }
  }

  terminate <- function() {
    if (!is.null(job)) {
      if (terminate_()) {
        .message_info(
          sprintf("Parallel job '%s' successfully terminated",
                  name))
      } else {
        .message_stop(
          sprintf("Something went wrong with terminating parallel job '%s'.",
                  name))
      }
    } else {
      .message_info(
        sprintf("Parallel job '%s' already terminated.",
                name))
    }
  }

  restitute_ <- function(result) {
      if ("try-error" %in% class(result)) {
        stop(attr(result, "condition"))
      } else {
        return(result)
      }
  }

  collect <- function(wait = FALSE, ...) {
    if (done) return(restitute_(result))
    rs <- parallel::mccollect(job, wait = wait, ...)
    if (is.null(rs)) {
      .message_info(
        sprintf("Parallel job '%s' still running. Please try again later.",
                name))
    } else {
      on.exit(
        if (!isTRUE(terminate_())) .message_stop(
          sprintf("Something went wrong with terminating parallel job '%s'.",
                  name))
      )
      result <<- rs[[1L]]
      done <<- TRUE
      return(restitute_(result))
    }
  }

  list(
    timestamp = timestamp,
    name = job$name,
    pid = job$pid,
    collect = collect,
    terminate = terminate
  )

}

#' @rdname run
#' @export
do.run_async <- do_run_async
