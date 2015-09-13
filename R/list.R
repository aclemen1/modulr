#' @export
# TODO: write documentation
list_modules <-
  function(regexp, reserved = TRUE, wide = TRUE, full = FALSE, formatted = TRUE,
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

  assertthat::assert_that(
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(reserved),
    assertthat::is.flag(wide),
    assertthat::is.flag(full),
    assertthat::is.flag(formatted))

  assertthat::assert_that(
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

  if(!reserved)
    flat <- setdiff(flat, RESERVED_NAMES)

  if(!missing(regexp))
    flat <- grep(paste0(regexp), flat, value = TRUE)

  if(assertthat::not_empty(flat)) {

    flat <- flat[ordered(flat)]

    if(wide) {

      digests <- vapply(flat, get_digest, FUN.VALUE = "")

      modified <-
        (if(formatted) function(x) format(x, format = "%c") else identity)(
          do.call(c, Map(function(name)
            modulr_env$register[[c(name, "timestamp")]], flat)))

      created <-
        (if(formatted) function(x) format(x, format = "%c") else identity)(
          do.call(c, Map(function(name)
            modulr_env$register[[c(name, "created")]], flat)))

      types <-
        do.call(c, Map(function(name)
          ifelse(modulr_env$register[[c(name, "instanciated")]],
                 typeof(modulr_env$register[[c(name, "instance")]]),
                 NA_character_), flat))

      sizes <-
        do.call(c, Map(function(name)
          (if(formatted) function(x) format(x, units = "auto") else identity)(
            utils::object.size(
              modulr_env$register[[c(name, "factory")]])), flat))

      weights <-
        do.call(c, Map(function(name)
          ifelse(
            modulr_env$register[[c(name, "instanciated")]],
            (if(formatted) function(x) format(x, units = "auto") else identity)(
              utils::object.size(modulr_env$register[[c(name, "instance")]])),
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

      if(full) return(data)

      return(data[unique(c("name", cols))])

    }

    flat

  }

}

#' @export
# TODO: write documentation
lsmod <- list_modules
