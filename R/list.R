#' @export
# TODO: write documentation
list_modules <- function(regexp, all = T, wide = T, full = F,
                         cols = c(
                           "name",
                           "type",
                           "calls",
                           "dependencies",
                           "requisitions",
                           "lines",
                           "modified")) {

  assertthat::assert_that(
    missing(regexp) || assertthat::is.string(regexp),
    assertthat::is.flag(all),
    assertthat::is.flag(wide),
    assertthat::is.flag(full),
    is.character(cols) &&
      all(cols %in% c(
        "name",
        "type",
        "calls",
        "dependencies",
        "requisitions",
        "lines",
        "chars",
        "created",
        "modified",
        "duration",
        "digest")))

  register <- .internals()$register

  flat <- names(register)

  if(!all)
    flat <- setdiff(flat, RESERVED_NAMES)

  if(!missing(regexp))
    flat <- grep(paste0(regexp), flat, value = T)

  if(assertthat::not_empty(flat)) {

    flat <- flat[ordered(flat)]

    if(wide) {

      digests <- vapply(flat, get_digest, FUN.VALUE = "")

      modified <-
        format(
          do.call(c, Map(function(name) register[[name]]$timestamp, flat)),
          "%c")

      created <-
        format(
          do.call(c, Map(function(name) register[[name]]$created, flat)), "%c")

      types <-
        do.call(c, Map(function(name)
          ifelse(register[[name]]$instanciated,
                 typeof(register[[name]]$instance), NA_character_), flat))

      deparsed_factories <-
        Map(function(name) deparse(register[[name]]$factory), flat)

      lines <- vapply(deparsed_factories, length, FUN.VALUE = 0)

      chars <- vapply(deparsed_factories, function(factory) sum(nchar(factory)),
                      FUN.VALUE = 0)

      durations <-
        do.call(c, Map(function(name) register[[name]]$duration, flat))

      adj <- .compute_adjacency_matrix(flat)
      deps <- diag(adj %*% t(adj))
      reqs <- diag(t(adj) %*% adj)

      calls <- do.call(c, Map(function(name) register[[name]]$calls, flat))

      data <- data.frame(
        name = flat,
        type = types,
        calls = calls,
        dependencies = deps,
        requisitions = reqs,
        lines = lines,
        chars = chars,
        created = created,
        modified = modified,
        duration = durations,
        digest = digests,
        stringsAsFactors = F,
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
