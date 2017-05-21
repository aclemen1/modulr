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
#'   "https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3"
#' list_modules()
#' reset()
#' # equivalently
#' "modulr/vault" %imports% "3fcc508cb40ddac6c1e3"
#' make("modulr/vault/example")
#' make_tests()}
#'
#' \dontrun{
#' reset()
#' # https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3
#' "modulr/vault_with_a_typo" %imports% "3fcc508cb40ddac6c1e3"
#' list_modules()}
#'
#' \dontrun{
#' reset()
#' # https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3
#' "modulr/vault" %digests%
#'   "with a wrong digest" %imports%
#'   "3fcc508cb40ddac6c1e3"
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

  name <- .get_name(name, load = FALSE)

  if (force && .is_defined(name))
    undefine(name)

  if (.is_undefined(name)) {

    parsed_version <- .parse_version(name)

    gist_url_match <- "((^https://)|^)gist.github.com/([^/]+/)?([0-9a-f]+)$"

    gist_id <- NULL

    if (grepl(gist_url_match, url, ignore.case = FALSE)) {
      gist_id <- regmatches(url, regexec(gist_url_match, url))[[1]][5]
    } else if (grepl("^[0-9a-f]+$", url)) {
      gist_id <- as.character(url)
    }

    .message_meta(
      if (!is.null(gist_id)) {
        sprintf(
          "Importing '%s' %sfrom gist ID '%s' ...",
          name,
          ifelse(!is.null(digest),
                 sprintf("with digest '%s' ", digest),
                 ""),
          gist_id)
      } else {
        sprintf(
          "Importing '%s' %sfrom '%s' ...",
          name,
          ifelse(!is.null(digest),
                 sprintf("with digest '%s' ", digest),
                 ""),
          url)
      }, {
        if (!is.null(gist_id)) {

          gist_req <-
            httr::GET(sprintf("https://api.github.com/gists/%s", gist_id))

          if (gist_req[["status_code"]] >= 400) {
            stop(sprintf("gist ID '%s' not found.", gist_id), call. = FALSE)
          }

          gist_content <-
            httr::content(gist_req, as = "text", encoding = "UTF-8")

          gist_result <-
            jsonlite::fromJSON(gist_content, simplifyVector = FALSE)

          gist_files <- gist_result[["files"]]

          gist_R_files <- gist_files[
            grepl("\\.(?:R)|\\.(?:Rmd)|\\.(?:Rnw)$",
                  names(gist_files), ignore.case = TRUE)]

          .message_meta(
            sprintf(
              "Found %d file(s) with R flavour at %s.",
              length(gist_R_files),
              gist_result[["html_url"]]
            ), verbosity = 2L)

          if (length(gist_R_files) == 0L) {
            stop(
              sprintf("no file with R flavour found in gist ID '%s'.", gist_id),
              call. = FALSE)
          }

          scripts <- lapply(gist_R_files, `[[`, "content")

        } else {

          parsed_url <- httr::parse_url(url)

          if (isTRUE(parsed_url[["scheme"]] %in% c("http", "https"))) {

            tryCatch({
              result <- httr::GET(url, ...)
            }, error = function(e) {
              stop(e$message, call. = FALSE)
            })

            if (isTRUE(result[["status_code"]] >= 400)) {
              stop(sprintf("URL '%s' not found.", url), call. = FALSE)
            }

            scripts <- list(httr::content(result, as = "text"))

          } else {

            stop("only HTTP(S) protocol is supported.", call. = FALSE)

          }

        }

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

        # Tangle scripts.
        for (idx in seq_along(scripts)) {

          script <- scripts[[idx]]

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

            scripts[[idx]] <-
              knitr::knit(text = script, tangle = TRUE, quiet = TRUE)

          }

        }

        nonames <- sprintf(".__no_filename_%d__", seq_along(scripts))
        if (is.null(names(scripts))) {
          scripts <- stats::setNames(scripts, nonames)
        } else {
          names(scripts[names(scripts) == ""]) <-
            nonames[names(scripts) == ""]
        }

        # Identify candidates among scripts.
        versions <- Map(function(script) {
          module_names <- .extract_name(
            text = script,
            namespace = .parse_name(name)[["namespace"]])
          versions <-
            Map(function(name) .parse_name(name)[["version"]], module_names)
          utils::tail(.filter_versions(
            versions,
            parsed_version[["version"]],
            parsed_version[["symbol"]]), 1L)
        }, scripts)

        script_name <-
          names(utils::tail(.filter_versions(
            unlist(lapply(versions, unname), recursive = FALSE),
            parsed_version[["version"]],
            parsed_version[["symbol"]]
          ), 1L))

        if (is.null(script_name)) {
          stop(sprintf("module '%s' not found.", name), call. = FALSE)
        }

        local_name <- names(versions[[script_name]])

        script <- scripts[[script_name]]
        path <-
          if (grepl("^\\.__no_filename_\\d+__$", script_name))
            NA_character_ else script_name

        registry_names <- names(.modulr_env$injector$registry)

        tryCatch({
          ev <- local(eval(parse(text = script, keep.source = TRUE)))
        },
        error = function(e) {
          rollback()
          stop(sprintf("%s Rolling back.", e$message), call. = FALSE)
        })

        loaded_names <-
          setdiff(names(.modulr_env$injector$registry),
                  registry_names)

        Map(function(name_) {
          if (is.null(.modulr_env$injector$registry[[c(name_, "url")]]) ||
              is.na(.modulr_env$injector$registry[[c(name_, "url")]])) {
            .modulr_env$injector$registry[[c(name_, "along")]] <-
              if (isTRUE(name_ != local_name)) local_name else NA_character_
            .modulr_env$injector$registry[[c(name_, "url")]] <- url
            .modulr_env$injector$registry[[c(name_, "filepath")]] <- path
          }
        },
        loaded_names)

        name <-
          .get_name(name, load = FALSE, all = FALSE)

        if (.is_undefined(name)) {
          rollback()
          stop(sprintf("module '%s' cannot be found. Rolling back.", name),
               call. = FALSE)
        }

        import_digest <- get_digest(name)

        .message_meta(
          sprintf(
            "Digest of '%s' is '%s'.",
            name,
            import_digest
          ), verbosity = 2L)

        if (!is.null(digest) && isTRUE(import_digest != digest)) {
          rollback()
          stop(sprintf("digests do not match. Rolling back."),
               call. = FALSE)
        }

      },
      verbosity = 2L)

    .message_meta(sprintf("DONE ('%s')", name), {
      return(invisible(ev))
    }, verbosity = 2L)

  }

  invisible(NULL)

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
