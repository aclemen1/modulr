.message_meta <- function(msg, expr = NULL, ok = FALSE, verbosity = 0L) {

  assert_that(
    assertthat::is.string(msg) || is.null(msg),
    assertthat::is.scalar(verbosity),
    assertthat::is.flag(ok))

  verbose <- verbosity <= get_verbosity()

  level <-
    .get_0(".message_level", envir = .modulr_env$injector, ifnotfound = 0L)
  on.exit(.modulr_env$injector$.message_level <- level)

  if (verbose && !is.null(msg)) {

    out <- sprintf(
      "[%s] ",
      format(Sys.time(), format = "%c"))

    if (level > 0L) {
      out <- paste0(out, sprintf(
        "%s ",
        paste(
          if (level <= 6L)
            rep("*", level) else
              sprintf("**...* (%d)", level),
          collapse = "")))
    }

    out <- paste0(out, msg, ifelse(ok, " ... ", ""))

    cat(out); if (!ok) cat("\n")

  }

  if (verbose && !is.null(msg))
    .modulr_env$injector$.message_level <- level + 1L

  ok_msg <- "OK"

  if (!is.null(expr)) {
    tryCatch({
      force(expr)
    },
    error = function(e) {
      ok_msg <<- "FAILED"
    })
  }

  if (ok && verbose && !is.null(msg)) {
    cat(paste0(ok_msg, "\n"))
  }

}

.parse_message_args <- function(...) {

  kwargs <- list(...)

  if (length(kwargs) == 0L) {
    return(list(core = c()))
  }

  if (is.null(names(kwargs))) {
    return(list(core = unlist(kwargs)))
  }

  core <- unlist(kwargs[nchar(names(kwargs)) == 0L], use.names = FALSE)

  others <- kwargs[nchar(names(kwargs)) != 0L]

  return(c(list(core = core), others))

}

.message <- function(...) {

  kwargs <- .parse_message_args(...)

  if (length(kwargs[["core"]])) {

    out <- sprintf("[%s%s] ",
                   format(Sys.time(), format = "%c"),
                   if ("module_name" %in% names(kwargs)) {
                     sprintf(" %s", kwargs[["module_name"]])
                   } else "")

    level <-
      .get_0(".message_level", envir = .modulr_env$injector, ifnotfound = 0L)

    if (level > 0L) {
      out <- paste0(out, sprintf(
        "%s ", paste(
          if (level <= 6L)
            rep("*", level) else
              sprintf("**...* (%d)", level),
          collapse = "")))
    }

    out <- paste0(out, paste0(kwargs[["core"]], collapse = ""))

    if ("appendLF" %in% names(kwargs)) {
      appendLF <- kwargs[["appendLF"]] # Exclude Linting
    } else {
      appendLF <- TRUE # Exclude Linting
    }

    kwargs[["fun"]](out, appendLF = appendLF) # Exclude Linting

  }

}

.message_info <- function(...)
  .message(..., fun = function(...) message(...))

.message_warn <- function(...)
  .message(..., fun = function(...) warning(..., immediate. = TRUE,
                                            call. = FALSE))

.message_stop <- function(...)
  .message(..., fun = function(...) stop(..., call. = FALSE))
