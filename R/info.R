.docstring <- function(fun) {

  assert_that(is.function(fun))

  docstring <- character(0)

  lines <- deparse(fun, control = "useSource")

  if(!isTRUE(length(lines) >= 2)) return(docstring)

  lines <- lines[2:length(lines)]

  empty <- rle(grepl("^\\s*$|^\\s*#[^']", lines))
  start <- empty$lengths[1][empty$values[1]]

  if(isTRUE(start >= 1))
    lines <- lines[(start + 1):length(lines)]

  comment <- rle(grepl("^\\s*#'", lines))
  end <- comment$lengths[1][comment$values[1]]

  if(!isTRUE(end >= 1)) return(docstring)

  lines <- lines[1:end]

  docstring_raw <- sub("^\\s*#'", "", lines)

  docstring_raw_margin <-
    min(Filter(
      function(x) x >= 0,
      vapply(docstring_raw,
             function(x)
               attr(gregexpr("^[ \t]*", x)[[1]], "match.length"),
             FUN.VALUE = 0)))

  docstring_lines <-
    vapply(docstring_raw,
           function(x)
             substr(x, min(nchar(x), docstring_raw_margin + 1), nchar(x)),
           FUN.VALUE = "", USE.NAMES = F)

  docstring <- paste(c(docstring_lines, ""), collapse = "\n")

  docstring
}

#' Output Docstring Info.
#'
#' Output the docstring info of a module.
#'
#' @inheritParams define
#' @param load A flag. Should the module be loaded?
#'
#' @details
#' A docstring is intended to document a module and provide the user with the
#' ability to inspect it at run time, for instance as an interactive help
#' system, or as metadata. Formally, it is a block of commented lines prefixed
#' with \code{#'} and located at the top of the module factory.
#'
#' The preferred formatting for a docstring is R Markdown, notabely for Modulr
#' Gears (see \code{\link{prepare_gear}}).
#'
#' @seealso \code{\link{define}}, \code{\link{prepare_gear}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' define("foo", NULL, function() {
#'  #' # Info
#'  #' This is a docstring for `foo`.
#' })
#' info("foo")
#'
#' \dontrun{
#' tmp <- tempfile(fileext = ".html")
#' cat((knitr::knit2html(text = info("foo"))), file = tmp)
#' if(interactive()) rstudio::viewer(tmp)
#' Sys.sleep(1); unlink(tmp)}
#' @aliases docstring
#' @export
info <- function(name = .Last.name, load = TRUE) {

  assert_that(
    .is_conform(name),
    assertthat::is.flag(load)
  )

  if (load) {

    if (.is_called_from_within_module()) {
      warning("publish_gear is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)

  }

  docstring <- .docstring(get_factory(name = name, load = FALSE))

  cat(docstring)

  invisible(docstring)
}
