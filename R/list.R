#' List Defined Modules.
#'
#' List defined modules.
#'
#' @inheritParams make
#' @param wide A flag. Should the function return a data frame instead of a
#'   characted vector?
#' @param full A flag. Should all columns be included in the data frame?
#' @param formatted A flag. Should columns with units be string formatted?
#' @param cols A character vector. Details returned in the columns of the data
#'   frame.
#' @param absolute A flag. Should the file paths be absolute?
#'
#' @return A character vector or a data frame containing module informations.
#'
#' @details
#'
#' For each module, the following details can be returned in the columns of the
#' data frame:
#' \describe{
#' \item{\code{name}}{name.}
#' \item{\code{version}}{version of the module.}
#' \item{\code{storage}}{storage of the module, `in-memory` or `on-disk`.}
#' \item{\code{along}}{name of the on-disk module along which the definition
#' takes place.}
#' \item{\code{filepath}}{file path of the `on-disk` module.}
#' \item{\code{type}}{type of the object returned.}
#' \item{\code{weight}}{memory size of the object.}
#' \item{\code{calls}}{number of explicit make calls.}
#' \item{\code{dependencies}}{number of direct dependencies.}
#' \item{\code{uses}}{number of modules requiring the module.}
#' \item{\code{size}}{memory size occupied by the definition.}
#' \item{\code{lines}}{number of lines of the provider.}
#' \item{\code{chars}}{number of characters of the provider.}
#' \item{\code{duration}}{duration of the evaluation.}
#' \item{\code{modified}}{timestamp of last modification.}
#' \item{\code{created}}{timestamp of creation.}
#' \item{\code{digest}}{digest (cf. \code{\link{get_digest}}).}
#' }
#'
#' @seealso \code{\link{define}}, \code{\link{make}},
#'   \code{\link{reset}}, and \code{\link{touch}}.
#'
#' @examples
#' reset()
#' list_modules()
#' define("foo", NULL, function() Sys.sleep(1L))
#' list_modules()
#' list_modules(reserved = TRUE)
#' list_modules(reserved = TRUE, wide = FALSE)
#' invisible(make("foo"))
#' list_modules(reserved = TRUE, full = TRUE)
#' list_modules(
#'   reserved = TRUE,
#'   formatted = FALSE,
#'   cols = c("weight", "size", "modified", "created"))
#' define("bar", NULL, function() Sys.sleep(1L))
#' define("foobar", list(f = "foo", b = "bar"), function(f, b) NULL)
#' invisible(make("foobar"))
#' Sys.sleep(1L)
#' touch("foo")
#' list_modules(".oo.*", cols = c("weight", "size", "modified", "created"))
#'
#' @aliases lsmod
#' @export
list_modules <-
  function(regexp,
           reserved = FALSE, wide = TRUE, full = FALSE,
           formatted = TRUE, absolute = FALSE,
           cols = c(
             "name",
             "version",
             "storage",
             "along",
             "type",
             "weight",
             "calls",
             "dependencies",
             "uses",
             "size",
             "lines",
             "modified")) {

  .message_meta("Entering list_modules() ...",
                verbosity = +Inf)

  assert_that(
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(reserved),
    assertthat::is.flag(wide),
    assertthat::is.flag(full),
    assertthat::is.flag(formatted))

  assert_that(
    is.character(cols) &&
      all(cols %in% c(
        "name",
        "version",
        "storage",
        "filepath",
        "url",
        "along",
        "type",
        "weight",
        "calls",
        "dependencies",
        "uses",
        "size",
        "lines",
        "chars",
        "created",
        "modified",
        "duration",
        "digest")),
    msg = "an invalid column name is specified.")

  flat <- names(.modulr_env$injector$registry)

  if (!reserved)
    flat <- Filter(.is_regular, flat)

  if (!missing(regexp))
    flat <- grep(paste0(regexp), flat, value = TRUE)

  if (assertthat::not_empty(flat)) {

    flat <- flat[ordered(flat)]

    if (wide) {

      digests <- vapply(flat, get_digest, FUN.VALUE = "")

      modified <-
        (if (formatted) function(x) format(x, format = "%c") else identity)(
          do.call(c, Map(function(name)
            .modulr_env$injector$registry[[c(name, "timestamp")]], flat)))

      created <-
        (if (formatted) function(x) format(x, format = "%c") else identity)(
          do.call(c, Map(function(name)
            .modulr_env$injector$registry[[c(name, "created")]], flat)))

      types <-
        do.call(c, Map(function(name)
          ifelse(
            .modulr_env$injector$registry[[c(name, "instanciated")]],
            typeof(
              .modulr_env$injector$registry[[c(name, "instance", "value")]]),
            NA_character_), flat))

      sizes <-
        do.call(c, Map(function(name)
          (if (formatted) function(x) format(x, units = "auto") else identity)(
            utils::object.size(
              .modulr_env$injector$registry[[c(name, "provider")]])), flat))

      weights <-
        do.call(c, Map(function(name)
          ifelse(
            .modulr_env$injector$registry[[c(name, "instanciated")]],
            (if (formatted) function(x) format(x, units = "auto") else identity)
            (utils::object.size(.modulr_env$injector$registry[[
              c(name, "instance", "value")]])),
            NA_character_), flat))

      deparsed_factories <-
        Map(function(name)
          deparse(.modulr_env$injector$registry[[c(name, "provider")]]), flat)

      lines <- vapply(deparsed_factories, length, FUN.VALUE = 0L)

      chars <- vapply(deparsed_factories,
                      function(provider) sum(nchar(provider)),
                      FUN.VALUE = 0L)

      durations <-
        do.call(c, Map(function(name)
          .modulr_env$injector$registry[[c(name, "duration")]], flat))

      storages <-
        do.call(c, Map(function(name)
          .modulr_env$injector$registry[[c(name, "storage")]], flat))

      filepaths <-
        do.call(c, Map(function(name) {
          if (!is.null(.modulr_env$injector$registry[[c(name, "filepath")]])) {
            ifelse(absolute, normalizePath, identity)(
              .modulr_env$injector$registry[[c(name, "filepath")]])
          } else {
            NA_character_
          }
        },
        flat))

      urls <-
        do.call(c, Map(function(name) {
          url <- .modulr_env$injector$registry[[c(name, "url")]]
          if (is.null(url)) NA_character_ else url
        },
        flat))

      versions <-
        do.call(c, Map(function(name)
          .parse_version(name)[["version"]], flat))

      alongs <-
        do.call(c, Map(function(name)
          .modulr_env$injector$registry[[c(name, "along")]], flat))

      adj <- .compute_adjacency_matrix(flat)
      deps <- diag(adj %*% t(adj))
      uses <- diag(t(adj) %*% adj)

      calls <- do.call(c, Map(function(name)
        .modulr_env$injector$registry[[c(name, "calls")]], flat))

      data <- data.frame(
        name = flat,
        version = versions,
        storage = storages,
        filepath = filepaths,
        url = urls,
        along = alongs,
        type = types,
        weight = weights,
        calls = calls,
        dependencies = deps,
        uses = uses,
        size = sizes,
        lines = lines,
        chars = chars,
        created = created,
        modified = modified,
        duration = durations,
        digest = digests,
        stringsAsFactors = FALSE,
        row.names = NULL)

      data <- data[order(data$name), ]

      row.names(data) <- seq_len(nrow(data))

      if (full) return(data)

      return(data[unique(c("name", cols))])

    }

    flat

  }

}

#' @rdname list_modules
#' @export
lsmod <- list_modules
