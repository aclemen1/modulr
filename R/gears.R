.function_to_string <- function(fun) {

  assert_that(is.function(fun))

  deparsed <- deparse(fun, control = "useSource")
  deparsed[1L] <- sub("function() ", "", deparsed[1L], fixed = TRUE)
  paste(deparsed, collapse = "\n")

}

.module_to_string <- function(name, base = NULL) {

  assert_that(
    .is_defined(name),
    is.null(base) || .is_defined(base))

  provider <- get_provider(name, load = FALSE)
  if (!is.null(base) &&
       identical(provider, get_provider(base), ignore.environment = TRUE)) {
    provider_sugar <- "%provides%"
    provider_string <- sprintf("get_provider(\"%s\")", base)
  } else if (identical(body(provider), body(options_provider_))) {
    provider_sugar <- "%provides_options%"
    provider_string <-
      paste(deparse(as.list(get("options", environment(provider))),
                  width.cutoff = 78L, control = "useSource"),
            collapse = "\n")
  } else {
    provider_sugar <- "%provides%"
    provider_string <- .function_to_string(provider)
  }

  dependencies <- .modulr_env$injector$registry[[name]]$dependencies
  if (isTRUE(length(dependencies) > 0L)) {
    if (length(dependencies) == 1L) {
      deps <-
        sprintf("list(%s = \"%s\")",
                names(dependencies),
                unlist(dependencies))
    } else {
      deps <- paste0(
        "list(\n    ",
        paste(
          sprintf("%s = \"%s\"",
                  names(dependencies),
                  unlist(dependencies)),
          collapse = ",\n    "),
        "\n)")
    }
    module <- sprintf(
      "\"%s\" %%requires%% %s %s %s\n",
      name, deps, provider_sugar, provider_string)
  } else {
    module <- sprintf(
      "\"%s\" %s %s\n",
      name, provider_sugar, provider_string)
  }
  module
}

.import_to_string <- function(name) {

  assert_that(.is_defined(name))

  url <- .modulr_env$injector$registry[[c(name, "url")]]

  if (!is.null(url)) {
    sprintf(
      paste(
        "\"%1$s\" %%imports%% \"%3$s\"",
        "",
        "# \"%1$s\" %%digests%% \"%2$s\" %%imports%% \"%3$s\"",
        "", sep = "\n"),
      name,
      .modulr_env$injector$registry[[c(name, "digest")]],
      url)
  }

}

#' Prepare Modulr Gear.
#'
#' Prepare a module (including mocks, tests and examples) for publication as a
#' modulr gear.
#'
#' @inheritParams define
#' @inheritParams import_module
#' @inheritParams get_digest
#'
#' @details
#'
#' A modulr gear is an R Markdown script containing a module, its
#' \link{docstring} info, and its special modules definitions, like mocks,
#' tests, and examples, for instance. \code{prepare_gear} prepares such a Modulr
#' Gear. If an \code{url} is given, the script will be completed accordingly
#' (see example).
#'
#' @return A string (character vector of length one) containing a modulr gear R
#'   Markdown script.
#'
#' @seealso \code{\link{define}}, \code{\link{release_gear_as_gist}}, \code{\link{info}},
#'   and \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() {
#'  #' # A `foo` module
#'  #'
#'  #' Description goes here.
#'  NULL
#' })
#' cat(prepare_gear("foo", url = "https://example.org/gears/foo"), sep = "\n")
#'
#' @export
prepare_gear <- function(name = .Last.name, url = NULL, load = TRUE) {

  .message_meta("Entering prepare_gear() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("prepare_gear is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    .is_regular(name),
    is.null(url) || assertthat::is.string(url),
    assertthat::is.flag(load))

  if (load) load_module(name)

  name <- find_module(name)[["name"]]

  assert_that(.is_defined_regular(name))

  imports <- Filter(function(x) !is.null(x), lapply(
    setdiff(.define_all_dependent_modules(name), name),
    .import_to_string))

  module <- .module_to_string(name)

  utils::capture.output(docstring <- info(name))

  mocks <-
    vapply(list_modules(regexp = sprintf("^%s/.*mocks?$", name), wide = F),
           .module_to_string, base = name, FUN.VALUE = "")
  tests <-
    vapply(list_modules(regexp = sprintf("^%s/.*tests?$", name), wide = F),
           .module_to_string, FUN.VALUE = "")
  examples <-
    vapply(list_modules(regexp = sprintf("^%s/.*examples?$", name), wide = F),
           .module_to_string, FUN.VALUE = "")

  gear <- paste(
    if (isTRUE(nchar(docstring) > 0L)) {
      format(docstring)
    } else {
      sprintf("# `%s`", name)
    },
    paste(
      sprintf("## Installation"),
      sprintf("```{r}"),
      sprintf("library(modulr)"),
      sprintf("```"),
      sprintf("```r"),
      sprintf("# Not run"),
      sprintf(
        paste(
          "\"%1$s\" %%imports%% \"%3$s\"",
          "",
          "# \"%1$s\" %%digests%% \"%2$s\" %%imports%% \"%3$s\"",
          sep = "\n"),
        name,
        .modulr_env$injector$registry[[c(name, "digest")]],
        ifelse(is.null(url), "<URL>", url)),
      sprintf("```"), sep = "\n"),
    paste(
      sprintf("## Definition"),
      if (length(imports) > 0L) paste(
        sprintf("```{r imports}"),
        sprintf("%s", paste(imports, collapse = "\n")),
        sprintf("```"), sep = "\n"),
      sprintf("```{r definition}"),
      sprintf("%s", module),
      sprintf("```"), sep = "\n"),
    if (length(tests) + length(mocks) > 0L) paste(
      sprintf("## Testing"),
      if (length(mocks) > 0L) paste(
        sprintf("### Mocks"),
        sprintf("```{r mocks}"),
        sprintf("%s", paste(mocks, collapse = "\n")),
        sprintf("```"), sep = "\n"),
      if (length(tests) > 0L) paste(
        sprintf("### Tests"),
        sprintf("```{r tests}"),
        sprintf("%s", paste(tests, collapse = "\n")),
        sprintf("```"),
        sprintf("```r"), sep = "\n"),
      sprintf("# Not run"),
      sprintf("make_all(regexp = \"%s/test\")", name),
      sprintf("```"), sep = "\n"),
    if (length(examples) > 0L) paste(
      sprintf("## Examples"),
      sprintf("```{r examples}"),
      sprintf("%s", paste(examples, collapse = "\n")),
      sprintf("```"),
      sprintf("```r"),
      sprintf("# Not run"),
      sprintf("make_all(regexp = \"%s/example\")", name),
      sprintf("```"), sep = "\n"),
    sprintf("---"),
    sprintf(paste0("Gear prepared with the R package ",
                   "[modulr](https://github.com/aclemen1/modulr) (v%s)."),
            utils::packageVersion("modulr")),
    sep = "\n")

  structure(
    gear,
    class = "modulr_gear")

}

print.modulr_gear <- function(x, ...) {
  cat(x)
  invisible(x)
}

#' Prepare and Release a Modulr Gear as a Gist on GitHub.
#'
#' Prepare and release a modulr gear as a gist on GitHub.
#'
#' @inheritParams define
#' @inheritParams get_digest
#' @param update A flag or a string (character vector of length one). Should an
#'   existing gist be updated (flag)? Or gist ID to update (string).
#' @param update_first_found A flag. Should the first corresponding module
#'   name be updated?
#' @param per_page A scalar (integer vector of length one). Number of gists
#'   per page requested.
#' @param max_pages A scalar (integer vector of length one). Upper bound for
#'   the number of pages requested.
#' @param endpoint A string (character vector of length one). GitHub API
#'   Endpoint URL.
#' @param browse A flag. Should the created or updated gist be opened in the
#'   default browser?
#'
#' @seealso \code{\link{define}}, \code{\link{prepare_gear}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' \dontrun{
#' library(gistr)
#' reset()
#' define("foo", NULL, function() NULL)
#' Sys.setenv("GITHUB_PAT" = "your Personal Access Token here")
#' gist_auth(reauth = TRUE)
#' release_gear_as_gist("foo")}
#'
#' @export
release_gear_as_gist <- function(name = .Last.name, load = TRUE,
                                 update = TRUE, update_first_found = TRUE,
                                 per_page = 100L, max_pages = 10L,
                                 endpoint = "https://api.github.com",
                                 browse = TRUE) {

  .message_meta("Entering release_gear_as_gist() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("release_gear_as_gist is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  # nocov start
  if (!requireNamespace("gistr", quietly = TRUE) ||
      !requireNamespace("testthat", quietly = TRUE)) {
    stop(
      "packages 'gistr' and 'testthat' are needed for this function to work. ",
      "Please install them.",
      call. = FALSE)
  }
  # nocov end

  assert_that(
    .is_regular(name),
    assertthat::is.flag(load),
    assertthat::is.flag(update) || assertthat::is.string(update),
    assertthat::is.flag(update_first_found),
    assertthat::is.count(per_page),
    assertthat::is.count(max_pages),
    assertthat::is.string(endpoint),
    assertthat::is.flag(browse))

  if (load) load_module(name)

  name <- find_module(name)[["name"]]

  assert_that(.is_defined_regular(name))

  g <- testthat::with_mock(
    `gistr:::ghbase` = function() endpoint, {

      gistr::gist_auth()

      file <- gsub("/", "-", name)

      filename <- sprintf("%s.Rmd", file)

      # nocov start
      if (isTRUE(update)) {

        candidates <- list()
        page <- 1L
        repeat {

          if (page > max_pages) break

          gs <-
            gistr::gists(what = "minepublic", per_page = per_page, page = page)

          if (length(gs) == 0L) break

          candidates <- c(
            candidates,
            Filter(
              function(g)
                any(filename == lapply(g[["files"]], `[[`, "filename")),
              gs))

          if (update_first_found && length(candidates) > 0L) break

          page <- page + 1L

        }

        if (length(candidates) == 1L) {

          g <- candidates[[1L]]

          if (length(gs) == 0L || page == 1L && length(gs) < per_page) {

            .message_info(
              sprintf("Updating '%s' in gist ID '%s'.",
                      filename, g[["id"]]))

          } else {

            .message_info(
              sprintf("Updating '%s' in gist ID '%s' (first candidate found).",
                      filename, g[["id"]]))

          }

          g <- gistr::gist(g[["id"]])

        } else if (length(candidates) > 1L) {

          .message_stop(
            sprintf("Found '%s' in too many (%d) gists.",
                    filename, length(candidates))
          )

        } else {

          g <- gistr::gist_create(
            code = {
              "# First commit."
            },
            description = sprintf("'%s' (modulr gear)", name),
            filename = filename, browse = FALSE)

          Sys.sleep(3L)

        }

      } else if (identical(update, FALSE)) {

        g <- gistr::gist_create(
          code = {
            "# First commit."
          },
          description = sprintf("'%s' (modulr gear)", name),
          filename = filename, browse = FALSE)

        Sys.sleep(3L)

      } else if (is.character(update)) {

        g <- gistr::gist(update)

        if (any(filename == lapply(g[["files"]], `[[`, "filename"))) {
          .message_info(
            sprintf("Updating '%s' in gist ID '%s'.",
                    filename, g[["id"]]))
        } else {
          .message_info(
            sprintf("Creating '%s' in gist ID '%s'.",
                    filename, g[["id"]]))
        }
      }
      # nocov end

      gear_id <- g[["id"]]

      gear_string <- prepare_gear(name, url = gear_id)

      if (identical(g[[c("files", filename, "content")]],
                    unclass(gear_string))) {

        .message_info(sprintf("No changes found for '%s' in gist ID '%s'.",
                              filename, g[["id"]]))

      } else {

        tmp_dir <- tempfile("modulr_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        tmp_filename <- file.path(tmp_dir, filename)

        cat(gear_string, file = tmp_filename)

        g <- gistr::update(gistr::update_files(g, tmp_filename))

      }

      if (browse) gistr::browse(g)

      return(g)

    })

  invisible(g)

}
