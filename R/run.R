#' Run a Module.
#'
#' Run a module as a standalone script.
#'
#' @param ... Module name as first argument. Further arguments can be passed
#'   for evaluation to the resulting function, if any (see \code{\link{make}}).
#'
#' @details
#'
#' TODO documentation
#'
#' @export
run <- function(...) {
  script <- capture.output(bundle(...))
  local(eval(parse(text = script)))
}

#' Run a Module Asynchronously.
#'
#' Run a module asynchronously as a standalone script.
#'
#' @param ... Module name as first argument. Further arguments can be passed
#'   for evaluation to the resulting function, if any (see \code{\link{make}}).
#'
#' @details
#'
#' TODO documentation
#'
#' @export
run_async <- function(...) {

  on.exit(
    message(
      "Provided `job <- .Last.value`, use `job$collect()` ",
      "to check and collect the result. ",
      "Similarily, use `job$terminate()` ",
      "to terminate the parallel process."))

  name <- list(...)[[1]]

  timestamp <- Sys.time()
  job <- parallel::mcparallel(run(...), name = name)
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
