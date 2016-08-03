# nocov start

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

  utils::capture.output(script <- do_bundle(
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
      "Your job has PID ", job$pid, ". ",
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
        .message_info("Job successfully terminated")
      } else {
        .message_stop(
          sprintf("Something went wrong terminating job '%s' (PID %d).",
                  name, job$pid))
      }
    } else {
      .message_info("Job already terminated.")
    }
  }

  restitute_ <- function(result) {
      if (inherits(result, "try-error")) {
        stop(attr(result, "condition"))
      } else {
        return(result)
      }
  }

  collect <- function(wait = FALSE, ...) {

    is_running <- !is.null(job) &&
      system(sprintf("ps -p %d", as.integer(job$pid)),
             ignore.stdout = TRUE, ignore.stderr = TRUE) == 0

    if (done) {
      if (is_running) {
        sprintf(
          "Job '%s' (PID %d) has not been terminated correctly. Trying again.",
          name, job$pid)
        on.exit(
          if (!isTRUE(terminate_())) .message_stop(
            sprintf("Something went wrong terminating job '%s' (PID %d).",
                    name, job$pid))
        )
      }
      return(restitute_(result))
    }

    if (is.null(job)) {
      if (is_running) {
        sprintf(
          "Job '%s' (PID %d) has not been terminated correctly. Trying again.",
          name, job$pid)
        on.exit(
          if (!isTRUE(terminate_())) .message_stop(
            sprintf("Something went wrong terminating job '%s' (PID %d).",
                    name, job$pid))
        )
      } else {
        .message_info("Job has been terminated. No result available.")
      }
      return(invisible(NULL))
    }

    rs <- parallel::mccollect(job, wait = wait, ...)

    if (is.null(rs)) {
      .message_info(
        sprintf("Job '%s' (PID %d) still running. Please try again later.",
                name, job$pid))
    } else {
      on.exit(
        if (!isTRUE(terminate_())) .message_stop(
          sprintf("Something went wrong terminating job '%s' (PID %d).",
                  name, job$pid))
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

# Begin Exclude Linting
# #' @rdname run
# #' @export
# list_async_jobs <- function(intern = FALSE) {
#   jobs <- grep(
#     "parallel:::.slaveR",
#     system("ps -o pid,cmd", intern = TRUE),
#     value = T)
#   if(length(jobs) > 0) {
#     if (!intern) {
#       cat(jobs, collapse = "\n")
#       return(invisible(paste(jobs, collapse = "\n")))
#     } else jobs
#   }
# }
#
# #' @rdname run
# #' @export
# terminate_async_jobs <- function() {
#   pids <- as.integer(unlist(
#     lapply(strsplit(list_async_jobs(intern = TRUE)," ", fixed = T),
#            function(x) x[1])))
#   if(length(pids) > 0)
#     tools::pskill(pids)
# }
# End Exclude Linting

# nocov end
