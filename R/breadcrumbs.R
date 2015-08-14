.__breadcrumbs__ <- function(void, verbose = T) {
  bc <- unique(
    unlist(
      Filter(function(x) !is.na(x) & !(x %in% c("modulr")),
             lapply(sys.frames(), function(frame) {
               unlist(mget(".__name__", envir = frame,
                           ifnotfound = NA, inherits = T),
                      use.names = F)
             }))))
  if(length(bc) & verbose)
    message(sprintf("modulr breadcrumbs: %s",
                    paste(sprintf("[%s]", bc), collapse = " > ")))
  invisible(bc)
}

.is_installed_bc <- function(handler = getOption("error")) {
  handler <- getOption("error")
  any(grepl("\\.\\_\\_breadcrumbs\\_\\_\\(\"installed\"\\)", format(handler)))
}

#' Activate breadcrumbs
#'
#' @export
activate_breadcrumbs <- function() {
  handler <- getOption("error")
  if(!.is_installed_bc(handler)) {
    if(is.null(handler)) {
      wrapper <- function() {
        .__breadcrumbs__('installed')
      }
    } else {
      wrapper <- function() {
        .__breadcrumbs__('installed')
        eval(parse(text = deparse(handler)))
      }
    }
    options(error = wrapper)
  }
}

#' Get and show breadcrumbs
#'
#' @export
breadcrumbs <- .__breadcrumbs__
