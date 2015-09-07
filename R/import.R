#' @export
import_module <- function(name, url, digest = NULL, force = F, ...) {

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
        "importing [%s] %sfrom %s ...",
        name,
        ifelse(!is.null(digest),
               sprintf("with digest %s ", digest),
               ""),
        url))

    result <- httr::GET(url, ...)

    script <- httr::content(result, as = "text")

    register <- .internals()$register

    if(grepl("```\\s*\\{\\s*[rR]", script)) {
      # Rmd import
      unnamed_chunk_label_opts = knitr::opts_knit$get("unnamed.chunk.label")
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
      stop(sprintf("No module named [%s] found. Rolling back.", name),
           call. = F)
    }

    if(!is.null(digest) && isTRUE(get_digest(name) != digest)) {
      assign("register", register, pos = modulr_env)
      stop(sprintf("Digest mismatch. Rolling back.", name),
           call. = F)
    }

    return(invisible(ev))

  }

  invisible()

}

#' @export
`%imports%` <- function(lhs, rhs) {

  assertthat::assert_that(
    assertthat::is.string(rhs),
    assertthat::is.string(lhs) || (
      is.list(lhs) &
        setequal(names(lhs), c("name", "digest"))))

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

  assertthat::assert_that(assertthat::is.string(lhs),
                          assertthat::is.string(rhs))

  list(name = lhs, digest = rhs)

}
