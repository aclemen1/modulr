message_meta <- function(msg, expr = NULL, verbosity = 0, ...) {

  verbosity_level <- get0("verbosity_level", envir = modulr_env,
                          ifnotfound = +Inf)
  verbose <- verbosity <= verbosity_level

  level <- get0(".message_level", envir = modulr_env, ifnotfound = 0)
  on.exit(assign(".message_level", level, pos = modulr_env))

  if(!is.null(msg) & verbose) {
    message(sprintf(
      "[%s] ",
      format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6")),
      appendLF = F)
    if(level > 0) {
      message(sprintf(
        "%s ",
        paste(rep("*", level), collapse="")),
        appendLF = F)
    }
    message(msg, appendLF = T)
  }

  if(verbose) assign(".message_level", level + 1, pos = modulr_env)
  if(!is.null(expr)) eval(expr)
}


.set_message_level <- function(level) {
  assign("message_level", max(0, level), pos = modulr_env)
}

.get_message_level <- function() {
  get0("message_level", envir = modulr_env, ifnotfound = 0)
}

.reset_message_level <- function() .set_message_level(0)

.increment_message_level <- function() {
  .set_message_level(.get_message_level() + 1)
}

.decrement_message_level <- function() {
  .set_message_level(.get_message_level() - 1)
}

.parse_message_args <- function(...) {
  kwargs <- list(...)
  if(length(kwargs) == 0) {
    return(list(core = c()))
  }
  if(is.null(names(kwargs))) {
    return(list(core = unlist(kwargs)))
  }
  core <- unlist(kwargs[nchar(names(kwargs)) == 0])
  others <- kwargs[nchar(names(kwargs)) != 0]
  return(c(list(core = core), others))
}

.message <- function(...) {
  kwargs <- .parse_message_args(...)
  if(length(kwargs$core)) {
    out <- ""
    if("module_name" %in% names(kwargs)) {
      out <- paste0(out, sprintf(
        "[%s] ",
        #format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6"),
        kwargs$module_name))
      level <- .get_message_level()
      if(level > 0) {
        out <- paste0(out, sprintf(
          "%s ", paste(rep("*", level), collapse="")))
      }
    }
    out <- paste0(out, paste0(kwargs$core, collapse = ""))
    kwargs$fun(out, appendLF = T)
  }
}

message_info <- function(...)
  .message(..., fun = function(...) message(...))
message_warn <- function(...)
  .message(..., fun = function(...) warning(..., immediate. = T, call. = F))
message_stop <- function(...)
  .message(..., fun = function(...) stop(..., call. = F))
