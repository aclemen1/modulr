#' Get and show breadcrumbs
#'
#' @export
# TODO: write documentation
.__breadcrumbs__ <- function(void, verbose = T) {

  assertthat::assert_that(assertthat::is.flag(verbose))

  bc <- unique(
    unlist(
      Filter(function(x) !is.na(x) & !(x %in% c("modulr")),
             lapply(sys.frames(), function(frame) {
               .get_0(".__name__", envir = frame,
                    ifnotfound = NA, inherits = T)
             }))))

  if(length(bc) & verbose)
    message(sprintf("modulr breadcrumbs: %s",
                    paste(sprintf("[%s]", bc), collapse = " > ")))

  invisible(bc)

}

.is_installed_bc <- function(handler = getOption("error")) {

  assertthat::assert_that(is.language(handler) || is.null(handler))

  if(is.null(handler)) return(F)

  any(grepl("\\.\\_\\_breadcrumbs\\_\\_\\(\"installed\"\\)", format(handler)))

}

#' Get and show breadcrumbs
#'
#' @export
# TODO: write documentation
breadcrumbs <- .__breadcrumbs__

#' Activate breadcrumbs
#'
#' @export
# TODO: write documentation
activate_breadcrumbs <- function() {

  handler <- getOption("error")

  if(!.is_installed_bc(handler)) {

    if(is.null(handler)) {
      wrapper <- function() {
        .__breadcrumbs__("installed")
      }

    } else {

      wrapper <- function() {
        .__breadcrumbs__("installed")
        eval(parse(text = deparse(handler)), envir = parent.frame())
      }

    }

    options(error = wrapper)

  }

}
