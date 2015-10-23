.function_to_string <- function(fun) {

  assert_that(is.function(fun))

  s <- capture.output(fun)
  if (grepl("<[^<]*>", s[length(s)]))
    s <- s[-length(s)]
  paste(s, collapse = "\n")
}

.module_to_string <- function(name, base = NULL) {

  assert_that(
    .is_defined_regular(name),
    is.null(base) || .is_defined_regular(base))

  factory <- get_factory(name, load = FALSE)
  if (!is.null(base) &&
       identical(factory, get_factory(base))) {
    factory_string <- sprintf("get_factory(\"%s\")", base)
  } else {
    factory_string <- .function_to_string(factory)
  }

  dependencies <- modulr_env$register[[name]]$dependencies
  if (isTRUE(length(dependencies) > 0)) {
    if (length(dependencies) == 1) {
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
        ")")
    }
    module <- sprintf(paste0(
      "\"%s\" %%requires%%\n",
      "  %s %%provides%%\n",
      "  %s\n"), name, deps, factory_string)
  } else {
    module <- sprintf(paste0(
      "\"%s\" %%provides%%\n",
      "  %s\n"), name, factory_string)
  }
  module
}

.import_to_string <- function(name) {

  assert_that(.is_defined(name))

  url <- modulr_env$register[[c(name, "url")]]

  if (!is.null(url)) {
    sprintf(
      paste(
        "\"%1$s\" %%imports%%",
        "  \"%3$s\"",
        "",
        "# \"%1$s\" %%digests%%",
        "#   \"%2$s\" %%imports%%",
        "#   \"%3$s\"",
        "", sep = "\n"),
      name,
      modulr_env$register[[c(name, "digest")]],
      url)
  }

}

#' Prepare Modulr Gear.
#'
#' Prepare a module (including mocks, tests and examples) for publication as a
#' Modulr Gear.
#'
#' @inheritParams define
#' @inheritParams import_module
#' @inheritParams get_digest
#'
#' @details
#'
#' A Modulr Gear is an R Markdown script containing a module and its special
#' modules definitions, like mocks, tests, and examples, for instance.
#' \code{prepare_gear} prepares such a Modulr Gear. If an \code{url} is given,
#' the script will be completed accordingly (see example).
#'
#' @return A string (character vector of length one) containing a Modulr Gear R
#'   Markdown script.
#'
#' @seealso \code{\link{define}}, \code{\link{gist_gear}}, and
#'   \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() NULL)
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

  assert_that(.is_defined_regular(name))

  imports <- Filter(function(x) !is.null(x), lapply(
    setdiff(.define_all_dependent_modules(name), name),
    .import_to_string))

  module <- .module_to_string(name)

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
    sprintf("# `%s` (Modulr Gear)", name),
    sprintf(""),
    paste(
      sprintf("## Installation"),
      sprintf("```{r}"),
      sprintf("library(modulr)"),
      sprintf("```"),
      sprintf("```r"),
      sprintf("# Not run"),
      sprintf(
        paste(
          "\"%1$s\" %%imports%%",
          "  \"%3$s\"",
          "",
          "# \"%1$s\" %%digests%%",
          "#   \"%2$s\" %%imports%%",
          "#   \"%3$s\"",
          sep = "\n"),
        name,
        modulr_env$register[[c(name, "digest")]],
        ifelse(is.null(url), "<URL>", url)),
      sprintf("```"), sep = "\n"),
    paste(
      sprintf("## Definition"),
      if (length(imports) > 0) paste(
        sprintf("```{r imports}"),
        sprintf("%s", paste(imports, collapse = "\n")),
        sprintf("```"), sep = "\n"),
      sprintf("```{r definition}"),
      sprintf("%s", module),
      sprintf("```"), sep = "\n"),
    if (length(tests) + length(mocks) > 0) paste(
      sprintf("## Testing"),
      if (length(mocks) > 0) paste(
        sprintf("### Mocks"),
        sprintf("```{r mocks}"),
        sprintf("%s", paste(mocks, collapse = "\n")),
        sprintf("```"), sep = "\n"),
      if (length(tests) > 0) paste(
        sprintf("### Tests"),
        sprintf("```{r tests}"),
        sprintf("%s", paste(tests, collapse = "\n")),
        sprintf("```"),
        sprintf("```r"), sep = "\n"),
      sprintf("# Not run"),
      sprintf("make_all(regexp = \"%s/test\")", name),
      sprintf("```"), sep = "\n"),
    if (length(examples) > 0) paste(
      sprintf("## Examples"),
      sprintf("```{r examples}"),
      sprintf("%s", paste(examples, collapse = "\n")),
      sprintf("```"),
      sprintf("```r"),
      sprintf("# Not run"),
      sprintf("make_all(regexp = \"%s/example\")", name),
      sprintf("```"), sep = "\n"),
    sprintf("---"),
    sprintf(paste0("_Gear prepared with the R package ",
                   "[_modulr_](https://github.com/aclemen1/modulr) (v%s) ",
                   "on %s._"), packageVersion("modulr"), Sys.time()),
    sep = "\n")

  gear

}

#' Prepare and Publish a Modulr Gear as a Gist on Github.
#'
#' Prepare and publish a Modulr Gear as a Gist on Github.
#'
#' @inheritParams define
#' @inheritParams get_digest
#' @param browse A flag. Should the created gist be opened in the default
#'   browser?
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
#' gist_gear("foo")}
#'
#' @export
gist_gear <- function(name = .Last.name, load = TRUE, browse = TRUE) {

  .message_meta("Entering publish_gear() ...",
                verbosity = +Inf)

  # nocov start
  if (!requireNamespace("gistr", quietly = TRUE)) {
    stop("gistr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # nocov end

  if (.is_called_from_within_module()) {
    warning("publish_gear is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    .is_regular(name),
    assertthat::is.flag(load),
    assertthat::is.flag(browse))

  if (load) load_module(name)

  assert_that(.is_defined_regular(name))

  auth <- gistr::gist_auth()

  rates <- gistr::rate_limit()
  assert_that(
    rates[[c("rate", "remaining")]] >= 2,
    msg = sprintf(
      "the resource limit is exceeded on Github. Wait until %s and try again.",
      format(as.POSIXct(rates[[c("rate", "reset")]] / 1e6,
                        origin = Sys.time()), format = "%c")))

  file <- gsub("/", "-", name)

  filename <- sprintf("%s.Rmd", file)

  # nocov start
  g <- gistr::gist_create(
    code = {
      '"First commit."'
      },
    description = sprintf("'%s' (modulr gear)", name),
    filename = filename, browse = FALSE)
  # nocov end

  # nocov start
  if (!is.null(auth)) Sys.sleep(3)
  # nocov end

  rates <- gistr::rate_limit()
  assert_that(
    rates[[c("rate", "remaining")]] >= 1,
    msg = sprintf(
      paste0(
        "the resource limit is exceeded on Github and ",
        "there is a dangling Gist to clean up here: %s. ",
        " Wait until %s and try again."),
      g[["html_url"]],
      format(as.POSIXct(rates[[c("rate", "reset")]] / 1e6,
                        origin = Sys.time()), format = "%c")))

  gear_url <- gsub("(?:raw/)[^/]*", "raw", g[["files"]][[1]][["raw_url"]])

  gear_string <- prepare_gear(name, url = gear_url)

  tmp_dir <- tempfile("modulr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  tmp_filename <- file.path(tmp_dir, filename)

  cat(gear_string, file = tmp_filename)

  g <- gistr::update(gistr::update_files(g, tmp_filename))

  if (browse) gistr::browse(g)

  return(g)

}
