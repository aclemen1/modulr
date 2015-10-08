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
#'
#' @return A character vector or a data frame containing module informations.
#'
#' @details
#'
#' For each module, the following details can be returned in the columns of the
#' data frame:
#' \describe{
#' \item{\code{name}}{name.}
#' \item{\code{type}}{type of the object returned.}
#' \item{\code{weight}}{memory size of the object.}
#' \item{\code{calls}}{number of explicit make calls.}
#' \item{\code{dependencies}}{number of direct dependencies (parents).}
#' \item{\code{childs}}{number of modules requiring the module (childs).}
#' \item{\code{size}}{memory size occupied by the definition.}
#' \item{\code{lines}}{number of lines of the factory.}
#' \item{\code{chars}}{number of characters of the factory.}
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
#' define("foo", NULL, function() Sys.sleep(1))
#' list_modules()
#' list_modules(reserved = TRUE)
#' list_modules(reserved = TRUE, wide = FALSE)
#' invisible(make("foo"))
#' list_modules(reserved = TRUE, full = TRUE)
#' list_modules(
#'   reserved = TRUE,
#'   formatted = FALSE,
#'   cols = c("weight", "size", "modified", "created"))
#' define("bar", NULL, function() Sys.sleep(1))
#' define("foobar", list(f = "foo", b = "bar"), function(f, b) NULL)
#' invisible(make("foobar"))
#' Sys.sleep(1)
#' touch("foo")
#' list_modules(".oo.*", cols = c("weight", "size", "modified", "created"))
#'
#' @aliases lsmod
#' @export
list_modules <-
  function(regexp,
           reserved = FALSE, wide = TRUE, full = FALSE, formatted = TRUE,
           cols = c(
             "name",
             "type",
             "weight",
             "calls",
             "dependencies",
             "childs",
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
        "type",
        "weight",
        "calls",
        "dependencies",
        "childs",
        "size",
        "lines",
        "chars",
        "created",
        "modified",
        "duration",
        "digest")),
    msg = "an invalid column name is specified.")

  flat <- names(modulr_env$register)

  if (!reserved)
    flat <- setdiff(flat, RESERVED_NAMES)

  if (!missing(regexp))
    flat <- grep(paste0(regexp), flat, value = TRUE)

  if (assertthat::not_empty(flat)) {

    flat <- flat[ordered(flat)]

    if (wide) {

      digests <- vapply(flat, get_digest, FUN.VALUE = "")

      modified <-
        (if (formatted) function(x) format(x, format = "%c") else identity)(
          do.call(c, Map(function(name)
            modulr_env$register[[c(name, "timestamp")]], flat)))

      created <-
        (if (formatted) function(x) format(x, format = "%c") else identity)(
          do.call(c, Map(function(name)
            modulr_env$register[[c(name, "created")]], flat)))

      types <-
        do.call(c, Map(function(name)
          ifelse(modulr_env$register[[c(name, "instanciated")]],
                 typeof(modulr_env$register[[c(name, "instance")]]),
                 NA_character_), flat))

      sizes <-
        do.call(c, Map(function(name)
          (if (formatted) function(x) format(x, units = "auto") else identity)(
            utils::object.size(
              modulr_env$register[[c(name, "factory")]])), flat))

      weights <-
        do.call(c, Map(function(name)
          ifelse(
            modulr_env$register[[c(name, "instanciated")]],
            (if (formatted) function(x) format(x, units = "auto") else identity)
            (utils::object.size(modulr_env$register[[c(name, "instance")]])),
            NA_character_), flat))

      deparsed_factories <-
        Map(function(name)
          deparse(modulr_env$register[[c(name, "factory")]]), flat)

      lines <- vapply(deparsed_factories, length, FUN.VALUE = 0)

      chars <- vapply(deparsed_factories, function(factory) sum(nchar(factory)),
                      FUN.VALUE = 0)

      durations <-
        do.call(c, Map(function(name)
          modulr_env$register[[c(name, "duration")]], flat))

      adj <- .compute_adjacency_matrix(flat)
      deps <- diag(adj %*% t(adj))
      childs <- diag(t(adj) %*% adj)

      calls <- do.call(c, Map(function(name)
        modulr_env$register[[c(name, "calls")]], flat))

      data <- data.frame(
        name = flat,
        type = types,
        weight = weights,
        calls = calls,
        dependencies = deps,
        childs = childs,
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
