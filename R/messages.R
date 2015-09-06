.message_meta <- function(msg, expr = NULL, verbosity = 0) {

  assertthat::assert_that(
    assertthat::is.string(msg),
    assertthat::is.scalar(verbosity))

  verbosity_level <- get0("verbosity", envir = modulr_env,
                          ifnotfound = +Inf)

  verbose <- verbosity <= verbosity_level

  level <- get0(".message_level", envir = modulr_env, ifnotfound = 0)
  on.exit(assign(".message_level", level, pos = modulr_env))

  if(verbose) {

    out <- sprintf(
      "[%s] ",
      format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6"))

    if(level > 0) {
      out <- paste0(out, sprintf(
        "%s ",
        paste(rep("*", level), collapse="")))
    }

    out <- paste0(out, msg)

    message(out, appendLF = T)

  }

  if(verbose) assign(".message_level", level + 1, pos = modulr_env)

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

  core <- unlist(kwargs[nchar(names(kwargs)) == 0], use.names = F)

  others <- kwargs[nchar(names(kwargs)) != 0]

  return(c(list(core = core), others))

}

.message <- function(...) {

  kwargs <- .parse_message_args(...)

  if(length(kwargs$core)) {

    out <- sprintf("[%s%s] ",
                   format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6"),
                   if("module_name" %in% names(kwargs)) {
                     sprintf(" %s", kwargs$module_name)
                     } else {""})

    level <- get0(".message_level", envir = modulr_env, ifnotfound = 0)

    if(level > 0) {
      out <- paste0(out, sprintf(
        "%s ", paste(rep("*", level), collapse="")))
    }

    out <- paste0(out, paste0(kwargs$core, collapse = ""))

    kwargs$fun(out, appendLF = T)

  }

}

.message_info <- function(...)
  .message(..., fun = function(...) message(...))

.message_warn <- function(...)
  .message(..., fun = function(...) warning(..., immediate. = T, call. = F))

.message_stop <- function(...)
  .message(..., fun = function(...) stop(..., call. = F))
