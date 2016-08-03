#' Import a Module.
#'
#' Import or re-import a module which is defined in an R, R Markdown or R Sweave
#' script at a given URL using the HTTP(S) protocol.
#'
#' @inheritParams define
#' @param url A string (character vector of length one). The URL must use the
#'  HTTP(S) protocol.
#' @param digest A string (character vector of length one). See
#'   \code{\link{get_digest}}.
#' @param force A flag. Should an already defined module be re-imported?
#' @param ... Further arguments passed to \code{httr::\link[httr]{GET}}.
#'
#' @return The result of the evaluation of the imported script.
#'
#' @details
#'
#' R Markdown and R Sweave files are accordingly tangled into R code, which is
#' in turn evaluated.
#'
#'  The imported module is rejected if
#'  \itemize{
#'    \item its name differs from the \code{name} argument,
#'    \item its digest differs from the \code{digest} argument.
#'  }
#'  In such a case, the internal state of modulr is rolled back.
#'
#' @section Syntactic Sugars:
#'  \preformatted{name \%imports\% url}
#'  \preformatted{name \%digests\% digest \%imports\% url}
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{httr::\link[httr]{GET}}, \code{\link{get_digest}},
#'  \code{\link{list_modules}}, \code{\link{make}}, and
#'  \code{\link{make_tests}}.
#'
#' @examples
#' \dontrun{
#' reset()
#' # https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3
#' "modulr/vault" %imports%
#'   paste0("https://gist.githubusercontent.com/aclemen1/",
#'     "3fcc508cb40ddac6c1e3/raw/modulr-vault.Rmd")
#' list_modules()
#' make("modulr/vault/example")
#' make_tests()}
#'
#' \dontrun{
#' reset()
#' # https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3
#' "modulr/vault_with_a_typo" %imports%
#'   paste0("https://gist.githubusercontent.com/aclemen1/",
#'     "3fcc508cb40ddac6c1e3/raw/modulr-vault.Rmd")
#' list_modules()}
#'
#' \dontrun{
#' reset()
#' # https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3
#' "modulr/vault" %digests%
#'   "with a wrong digest" %imports%
#'   paste0("https://gist.githubusercontent.com/aclemen1/",
#'     "3fcc508cb40ddac6c1e3/raw/modulr-vault.Rmd")
#' list_modules()}
#'
#' @aliases %digests% %imports%
#' @export
import_module <- function(name, url, digest = NULL,
                          force = FALSE, ...) {

  .message_meta(sprintf("Entering import_module() for '%s' ...", name),
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("import_module is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    .is_regular(name),
    assertthat::is.string(url),
    is.null(digest) || assertthat::is.string(digest),
    assertthat::is.flag(force)
  )

  try(load_module(name), silent = TRUE)

  if (force && .is_defined(name))
    undefine(name)

  if (.is_undefined(name)) {

    .message_meta(
      sprintf(
        "Importing '%s' %sfrom '%s' ...",
        name,
        ifelse(!is.null(digest),
               sprintf("with digest '%s' ", digest),
               ""),
        url), verbosity = 2)

    result <- httr::GET(url, ...)

    script <- httr::content(result, as = "text")

    registry <- .modulr_env$injector$registry
    .Last.name <- .modulr_env$injector$.Last.name
    config <- .modulr_env$injector$config
    verbosity <- .modulr_env$injector$verbosity
    stash <- .modulr_env$injector$stash

    rollback <- function() {
      .modulr_env$injector$registry <- registry
      .modulr_env$injector$.Last.name <- .Last.name
      .modulr_env$injector$config <- config
      .modulr_env$injector$verbosity <- verbosity
      .modulr_env$injector$stash <- stash
    }

    if (grepl("```\\s*\\{\\s*[rR]", script) ||
         grepl("<<[^>]*>>=[^@]*@", script)) {
      # Rmd import

      opat <- knitr::knit_patterns$get()
      oopts_knit <- knitr::opts_knit$get()
      oopts_template <- knitr::opts_template$get()
      oopts_hooks <- knitr::opts_hooks$get()
      oopts_chunk <- knitr::opts_chunk$get()
      oopts_current <- knitr::opts_current$get()

      knitr::knit_patterns$restore()
      on.exit(knitr::knit_patterns$set(opat), add = TRUE)
      knitr::opts_knit$restore()
      on.exit(knitr::opts_knit$set(oopts_knit), add = TRUE)
      knitr::opts_template$restore()
      on.exit(knitr::opts_template$set(oopts_template), add = TRUE)
      knitr::opts_hooks$restore()
      on.exit(knitr::opts_hooks$set(oopts_hooks), add = TRUE)
      knitr::opts_chunk$restore()
      on.exit(knitr::opts_chunk$set(oopts_chunk), add = TRUE)
      knitr::opts_current$restore()
      on.exit(knitr::opts_current$set(oopts_current), add = TRUE)

      knitr::opts_knit$set("unnamed.chunk.label" =
                           paste("modulr", url, sep = "-"))

      script <-
        knitr::knit(text = script, tangle = TRUE, quiet = TRUE)

    }

    tryCatch({
      ev <- local(eval(parse(text = script, keep.source = TRUE)))
      },
      error = function(e) {
        rollback()
        e$message <- sprintf("%s. Rolling back.", e$message)
        stop(e)
      })

    if (.is_undefined(name)) {
      rollback()
      stop(sprintf("module '%s' cannot be found. Rolling back.", name),
           call. = FALSE)
    }

    if (!is.null(digest) && isTRUE(get_digest(name) != digest)) {
      rollback()
      stop(sprintf("digests do not match. Rolling back.", name),
           call. = FALSE)
    }

    .modulr_env$injector$registry[[c(name, "url")]] <- url

    return(invisible(ev))

  }

  invisible()

}

#' @export
`%imports%` <- function(lhs, rhs) {

  if (.is_called_from_within_module()) {
    warning("`%imports%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.string(rhs),
    msg = "right-hand side of `%imports%` is not an URL."
    )

  assert_that(
    assertthat::is.string(lhs) || (
      is.list(lhs) &
        setequal(names(lhs), c("name", "digest"))),
    msg = "left-hand side of `%imports%` is not a module name or a digest.")

  if (is.list(lhs)) {
    name <- lhs[["name"]]
    digest <- lhs[["digest"]]
  } else {
    name <- lhs
    digest <- NULL
  }

  import_module(name = name, digest = digest, url = rhs, force = FALSE)

}

#' @export
`%digests%` <- function(lhs, rhs) {

  assert_that(
    assertthat::is.string(lhs),
    msg = "left-hand side of `%digests%` is not a module name.")
  assert_that(
    assertthat::is.string(rhs),
    msg = "right-hand side of `%digests%` is not a digest.")

  list(name = lhs, digest = rhs)

}
