#' @export
import_module <- function(name, url, digest = NULL, force = F, ...) {

  .message_meta(sprintf("Entering import_module() for '%s' ...", name),
                verbosity = +Inf)

  if(.is_called_from_within_module()) {
    stop("import_module is called from within a module.", call. = F)
  }

  assertthat::assert_that(
    .is_regular(name),
    assertthat::is.string(url),
    is.null(digest) || assertthat::is.string(digest),
    assertthat::is.flag(force)
  )

  try(load_module(name), silent = T)

  if(force && .is_defined(name))
    undefine(name)

  if(.is_undefined(name)) {

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

    register <- .internals()$register

    if(grepl("```\\s*\\{\\s*[rR]", script)) {
      # Rmd import
      unnamed_chunk_label_opts <- knitr::opts_knit$get("unnamed.chunk.label")
      knitr::opts_knit$set("unnamed.chunk.label" =
                             paste("modulr", name, sep="/"))
      script <- knitr::knit(text = script,
                            tangle = T, quiet = T)
      knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)
    }

    tryCatch({
      ev <- eval(parse(text = script),
                 envir = parent.frame())
      },
      error = function(e) {
        assign("register", register, pos = modulr_env)
        e$message <- sprintf("%s. Rolling back.", e$message)
        stop(e)
      })

    if(.is_undefined(name)) {
      assign("register", register, pos = modulr_env)
      stop(sprintf("module '%s' cannot be found. Rolling back.", name),
           call. = F)
    }

    if(!is.null(digest) && isTRUE(get_digest(name) != digest)) {
      assign("register", register, pos = modulr_env)
      stop(sprintf("digest is not matching. Rolling back.", name),
           call. = F)
    }

    return(invisible(ev))

  }

  invisible()

}

#' @export
`%imports%` <- function(lhs, rhs) {

  if(.is_called_from_within_module()) {
    warning("`%imports%` is called from within a module.",
            call. = F, immediate. = T)
  }

  assertthat::assert_that(
    assertthat::is.string(rhs),
    msg = "right-hand side of `%imports%` is not an URL."
    )

  assertthat::assert_that(
    assertthat::is.string(lhs) || (
      is.list(lhs) &
        setequal(names(lhs), c("name", "digest"))),
    msg = "left-hand side of `%imports%` is not a module name or a digest.")

  if(is.list(lhs)) {
    name <- lhs$name
    digest <- lhs$digest
  } else {
    name <- lhs
    digest <- NULL
  }

  import_module(name = name, digest = digest, url = rhs, force = F)

}

#' @export
`%digests%` <- function(lhs, rhs) {

  assertthat::assert_that(
    assertthat::is.string(lhs),
    msg = "left-hand side of `%digests%` is not a module name.")
  assertthat::assert_that(
    assertthat::is.string(rhs),
    msg = "right-hand side of `%digests%` is not a digest.")

  list(name = lhs, digest = rhs)

}
