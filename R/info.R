# parse the docstring of a function
.docstring <- function(fun, line_numbers = FALSE, sep = "\n") {

  assert_that(
    is.function(fun),
    assertthat::is.flag(line_numbers),
    assertthat::is.string(sep)
  )

  docstring <- character(0L)

  lines <- deparse(fun, control = "useSource")

  if (!isTRUE(length(lines) >= 2L)) return(docstring)

  lines <- lines[2:length(lines)]

  empty <- rle(grepl("^\\s*$|^\\s*#[^']", lines))
  start <- empty$lengths[1L][empty$values[1L]]

  if (isTRUE(start >= 1L))
    lines <- lines[(start + 1L):length(lines)]

  comment <- rle(grepl("^\\s*#'", lines))

  starts <-
    (utils::head(cumsum(c(0L, comment$lengths)) + 1L, -1L))[comment$value]
  ends <- cumsum(comment$lengths)[comment$values]

  line_offset <- as.integer(attr(fun, "srcref", exact = TRUE))[1L]
  line_string_width <- nchar(sprintf("%s", utils::tail(ends, 1L) + line_offset))

  for (bloc_idx in 1L:length(starts)) {
    start <- starts[bloc_idx]
    end <- ends[bloc_idx]

    if (isTRUE(end >= start)) {

      bloc_lines <- lines[start:end]

      docstring_raw <- sub("^\\s*#'", "", bloc_lines)

      docstring_raw_margin <-
        min(Filter(
          function(x) x >= 0L,
          vapply(docstring_raw,
                 function(x)
                   attr(gregexpr("^[ \t]*", x)[[1L]], "match.length"),
                 FUN.VALUE = 0L)))

      docstring_lines <-
        vapply(
          seq_len(length(docstring_raw)),
          function(idx) {
            x <- docstring_raw[idx]
            content <-
              substr(x, min(nchar(x), docstring_raw_margin + 1L), nchar(x))
            if (line_numbers && !is.na(line_offset)) {
              content <- paste(
                format(sprintf("[%s]", start + line_offset + idx),
                       justify = "right", width = line_string_width + 2L),
                content, sep = " ")
            }
            content
          },
          FUN.VALUE = "", USE.NAMES = F)

      if (grepl("\\b(?:@noRd)\\b", utils::tail(docstring_lines, 1L))) {
        docstring_lines <- utils::head(docstring_lines, -1L)
      }

      docstring <- paste(c(docstring, docstring_lines), collapse = "\n")

    }

    if (bloc_idx < length(starts))
      docstring <- paste(c(docstring, sep), collapse = "")

  }

  docstring
}

#' Output Docstring Info.
#'
#' Output the docstring info of a module.
#'
#' @inheritParams define
#' @param load A flag. Should the module be loaded?
#' @param line_numbers A flag. Should the source line numbers be outputed?
#' @param sep A string (character vector of length one) containing the separator
#'   between docstrings blocs.
#'
#' @details A docstring is intended to document a module and provide the user
#' with the ability to inspect it at run time, for instance as an interactive
#' help system, or as metadata. Formally, it is a block of commented lines
#' prefixed with \code{#'} and located at the top of the module provider.
#'
#' The preferred formatting for a docstring is R Markdown, notabely for Modulr
#' Gears (see \code{\link{prepare_gear}}).
#'
#' @seealso \code{\link{define}}, \code{\link{prepare_gear}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' define("foo", NULL, {
#'  #' # Info
#'  #' This is a docstring for `foo`.
#' })
#' print_info("foo")
#'
#' \dontrun{
#' tmp <- tempfile(fileext = ".html")
#' cat((knitr::knit2html(text = info("foo"))), file = tmp)
#' if(interactive()) rstudio::viewer(tmp)
#' Sys.sleep(1L); unlink(tmp)}
#' @aliases docstring info
#' @export
print_info <- function(name = .Last.name,
                       line_numbers = FALSE, sep = "\n",
                       load = TRUE) {

  assert_that(
    .is_conform(name),
    assertthat::is.flag(line_numbers),
    assertthat::is.string(sep),
    assertthat::is.flag(load)
  )

  if (.is_reserved(name)) {
    line_numbers <- FALSE
    sep <- "\n"
  }

  if (load) {

    if (.is_called_from_within_module()) {
      warning("publish_gear is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)

  }

  docstring <- .docstring(get_provider(name = name, load = FALSE),
                          line_numbers = line_numbers, sep = sep)

  cat(docstring)

  invisible(docstring)
}

#' @rdname print_info
#' @export
info <- print_info
