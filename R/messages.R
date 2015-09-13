.message_meta <- function(msg, expr = NULL, verbosity = 0) {

  assertthat::assert_that(
    assertthat::is.string(msg),
    assertthat::is.scalar(verbosity))

  verbosity_level <- .get_0("verbosity", envir = modulr_env,
                          ifnotfound = 2)

  verbose <- verbosity <= verbosity_level

  level <- .get_0(".message_level", envir = modulr_env, ifnotfound = 0)
  on.exit(modulr_env$.message_level <- level)

  if(verbose) {

    out <- sprintf(
      "[%s] ",
      format(Sys.time(), format = "%c"))

    if(level > 0) {
      out <- paste0(out, sprintf(
        "%s ",
        paste(rep("*", level), collapse="")))
    }

    out <- paste0(out, msg)

    message(out, appendLF = TRUE)

  }

  if (verbose) modulr_env$.message_level <- level + 1

  if(!is.null(expr)) eval(expr)

}

.parse_message_args <- function(...) {

  kwargs <- list(...)

  if(length(kwargs) == 0) {
    return(list(core = c()))
  }

  if(is.null(names(kwargs))) {
    return(list(core = unlist(kwargs)))
  }

  core <- unlist(kwargs[nchar(names(kwargs)) == 0], use.names = FALSE)

  others <- kwargs[nchar(names(kwargs)) != 0]

  return(c(list(core = core), others))

}

.message <- function(...) {

  kwargs <- .parse_message_args(...)

  if(length(kwargs[["core"]])) {

    out <- sprintf("[%s%s] ",
                   format(Sys.time(), format = "%c"),
                   if("module_name" %in% names(kwargs)) {
                     sprintf(" %s", kwargs[["module_name"]])
                   } else "")

    level <- .get_0(".message_level", envir = modulr_env, ifnotfound = 0)

    if(level > 0) {
      out <- paste0(out, sprintf(
        "%s ", paste(rep("*", level), collapse="")))
    }

    out <- paste0(out, paste0(kwargs[["core"]], collapse = ""))

    kwargs[["fun"]](out, appendLF = TRUE) # Exclude Linting

  }

}

.message_info <- function(...)
  .message(..., fun = function(...) message(...))

.message_warn <- function(...)
  .message(..., fun = function(...) warning(..., immediate. = TRUE,
                                            call. = FALSE))

.message_stop <- function(...)
  .message(..., fun = function(...) stop(..., call. = FALSE))
