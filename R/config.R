#' @include modulr.R

# A helper function to set, get and unset values in configuration scopes.
.config <- function(scope) {

  assert_that(missing(scope) || is.character(scope))

  if (missing(scope)) return(NULL)

  unset <- function() {

    modulr_env$config[[scope]] <- NULL

  }

  set <- function(..., drop = TRUE) {

    .message_meta("Entering .config$set() ...",
                  verbosity = +Inf)

    assert_that(assertthat::is.flag(drop))

    options_list <- list(...)

    if (is.null(names(options_list))
       & length(options_list) == 1)
      if (is.list(options_list[[1]]))
        options_list <- options_list[[1]]

    if (length(options_list) == 0) return(invisible())

    if (is.null(modulr_env$config[[scope]])) {

      modulr_env$config[[scope]] <- options_list

    } else {

      if (is.null(names(options_list))) {

        if (drop)
          modulr_env$config[[scope]] <- options_list

      } else {

        for (key in names(options_list))
          if (is.null(modulr_env$config[[c(scope, key)]]) | isTRUE(drop))
            modulr_env$config[[c(scope, key)]] <- options_list[[key]]

      }

    }

  }

  get_all <- function() {

    .message_meta("Entering .config$get_all() ...",
                  verbosity = +Inf)

    if (is.na(scope[2])) {

      modulr_env$config[[scope[1]]]

    } else {

      modulr_env$config[[scope[1:2]]]

    }

  }

  get <- function(key) {

    .message_meta("Entering .config$get() ...",
                  verbosity = +Inf)

    assert_that(is.null(key) || assertthat::is.string(key))

    if (!is.null(key))
      get_all()[[key]]

  }

  list(
    unset = unset,
    set = set,
    get = get,
    get_all = get_all
  )

}

#' All configurations.
#'
#' @export
# TODO: write documentation
get_configs <- function() get("config", pos = modulr_env)

#' Root configuration.
#'
#' @export
# TODO: write documentation
root_config <-
  .config(".__root__")

#' Paths configuration.
#'
#' @export
# TODO: write documentation
paths_config <-
  .config(".__paths__")


#' Maps configuration.
#'
#' @export
# TODO: write documentation
maps_config <-
  .config(".__maps__")

#' Module options.
#'
#' @export
# TODO: write documentation
module_option <- function(name = .Last.name) {

  .message_meta("Entering module_option() ...",
                verbosity = +Inf)

  assert_that(.is_regular(name))

  .config(c("modules", name))

}

#' Syntactic sugar for setting default module options.
#'
#' @export
`%has_default_option%` <- function(lhs, rhs) {

  assert_that(
    .is_regular(lhs),
    msg =
      "left-hand side of `%has_default_option%` is not a regular module name."
    )

  assert_that(
    is.list(rhs),
    msg = "right-hand side of `%has_default_option%` is not a list."
  )

  module_option(lhs)$set(rhs, drop = FALSE)

}

#' Syntactic sugar for setting default module options.
#'
#' @export
`%has_default_options%` <-
  function(lhs, rhs) eval(substitute(`%has_default_option%`(lhs, rhs)),
                          envir = parent.frame())

#' Syntactic sugar for setting module options.
#'
#' @export
`%has_option%` <- function(lhs, rhs) {

  assert_that(
    .is_regular(lhs),
    msg = "left-hand side of `%has_option%` is not a regular module name."
  )

  assert_that(
    is.list(rhs),
    msg = "right-hand side of `%has_option%` is not a list."
  )

  module_option(lhs)$set(rhs, drop = TRUE)

}

#' Syntactic sugar for setting module options.
#'
#' @export
`%has_options%` <- function(lhs, rhs) eval(substitute(`%has_option%`(lhs, rhs)),
                                  envir = parent.frame())
