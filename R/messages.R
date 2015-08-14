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

message_meta <- function(...) {
  kwargs <- .parse_message_args(...)
  if(length(kwargs$core)) {
    message(sprintf(
      "[%s] ",
      format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6")),
      appendLF = F)
    if("level" %in% names(kwargs)) {
      message(sprintf(
        "%s ",
        paste(rep("*", kwargs$level), collapse="")),
        appendLF = F)
    }
    message(kwargs$core, appendLF = T)
  }
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
      if("level" %in% names(kwargs)) {
        out <- paste0(out, sprintf(
          "%s ", paste(rep("*", kwargs$level), collapse="")))
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
