#' Get and show breadcrumbs
#'
#' @export
# TODO: write documentation
get_breadcrumbs <- function(void, verbose = TRUE) {

  assert_that(assertthat::is.flag(verbose))

  bc <- unique(
    unlist(
      Filter(function(x) !is.na(x) & !(x %in% c("modulr")),
             lapply(sys.frames(), function(frame) {
               .get_0(".__name__", envir = frame,
                    ifnotfound = NA, inherits = TRUE)
             }))))

  if (length(bc) > 0 && verbose)
    message(sprintf("modulr breadcrumbs: %s",
                    paste(sprintf("'%s'", bc), collapse = " > ")))

  invisible(bc)

}

.is_installed_bc <- function(handler = getOption("error")) {

  assert_that(is.language(handler) || is.null(handler))

  if (is.null(handler)) return(F)

  any(grepl("modulr\\:\\:get\\_breadcrumbs\\(\"installed\"\\)",
            format(handler)))

}

#' Activate breadcrumbs
#'
#' @export
# TODO: write documentation
activate_breadcrumbs <- function() {

  handler <- getOption("error")

  if (!.is_installed_bc(handler)) {

    wrapper <- function() {
      modulr::get_breadcrumbs("installed")
      eval(parse(text = deparse(handler)), envir = parent.frame())
      if(!interactive()) stop(call. = FALSE) # nocov
    }

    options(error = wrapper)

  }

}
