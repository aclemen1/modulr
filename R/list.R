#' @export
# TODO: write documentation
list_modules <- function(regexp, all = T, wide = T, full = F,
                         cols = c(
                           "name",
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
        "calls",
        "dependencies",
        "requisitions",
        "lines",
        "chars",
        "created",
        "modified",
        "duration",
        "signature")))

  register <- .internals()$register

  flat <- names(register)

  if(!all)
    flat <- setdiff(flat, RESERVED_NAMES)

  if(!missing(regexp))
    flat <- grep(paste0(regexp), flat, value = T)

  if(assertthat::not_empty(flat)) {

    flat <- flat[ordered(flat)]

    if(wide) {

      signatures <- vapply(flat, get_signature, FUN.VALUE = "")

      modified <-
        do.call(c, Map(function(name) register[[name]]$timestamp, flat))

      created <-
        do.call(c, Map(function(name) register[[name]]$created, flat))

      deparsed_factories <- Map(function(name) deparse(register[[name]]), flat)

      lines <- vapply(deparsed_factories, length, FUN.VALUE = 0)

      chars <- vapply(deparsed_factories, function(factory) sum(nchar(factory)),
                      FUN.VALUE = 0)

      durations <- do.call(c, Map(function(name) register[[name]]$duration, flat))

      inc <- table(Reduce(rbind, Map(function(name) {
        deps <- factor(unlist(register[[name]]$dependencies), levels = flat)
        data.frame(from=factor(rep(name, length(deps)), levels = flat), to=deps)
      }, flat)))

      deps <- diag(inc %*% t(inc))

      reqs <- diag(t(inc) %*% inc)

      calls <- do.call(c, Map(function(name) register[[name]]$calls, flat))

      data <- data.frame(
        name = flat,
        calls = calls,
        dependencies = deps,
        requisitions = reqs,
        lines = lines,
        chars = chars,
        created = created,
        modified = modified,
        duration = durations,
        signature = signatures,
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
