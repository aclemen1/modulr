# A helper function to set, get and unset values in configuration scopes.
.config <- function(scope) {

  assertthat::assert_that(missing(scope) || is.character(scope))

  if(missing(scope)) return(NULL)

  unset <- function() {

    config <- base::get("config", pos = modulr_env)

    config[[scope]] <- NULL

    assign("config", config, pos = modulr_env)

  }

  set <- function(..., drop = T) {

    .message_meta("Entering .config$set() ...",
                  verbosity = +Inf)

    assertthat::assert_that(assertthat::is.flag(drop))

    options_list = list(...)

    if(is.null(names(options_list))
       & length(options_list) == 1)
      if(is.list(options_list[[1]]))
        options_list <- options_list[[1]]

    if(length(options_list) == 0) return(invisible())

    config <- base::get("config", pos = modulr_env)

    if(is.null(config[[scope]])) {

      config[[scope]] <- options_list

    } else {

      if(is.null(names(options_list))) {

        if(drop)
          config[[scope]] <- options_list

      } else {

        for(key in names(options_list))
          if(is.null(config[[scope]][[key]]) | isTRUE(drop))
            config[[scope]][[key]] <- options_list[[key]]

      }

    }

    assign("config", config, pos = modulr_env)

  }

  get_all <- function() {

    .message_meta("Entering .config$get_all() ...",
                  verbosity = +Inf)

    config <- base::get("config", pos = modulr_env)

    if(is.na(scope[2])) {

      config[[scope[1]]]

    } else {

      config[[scope[1]]][[scope[2]]]

    }

  }

  get <- function(key) {

    .message_meta("Entering .config$get() ...",
                  verbosity = +Inf)

    assertthat::assert_that(is.null(key) || assertthat::is.string(key))

    if(!is.null(key))
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
module_option <- function(name) {

  .message_meta("Entering module_option() ...",
                verbosity = +Inf)

  assertthat::assert_that(.is_regular(name))

  .config(c("modules", name))

}

#' Syntactic sugar for setting default module options.
#'
#' @export
`%has_default_option%` <- function(lhs, rhs) {

  assertthat::assert_that(
    .is_regular(lhs),
    msg =
      "left-hand side of `%has_default_option%` is not a regular module name."
    )

  assertthat::assert_that(
    is.list(rhs),
    msg = "right-hand side of `%has_default_option%` is not a list."
  )

  module_option(lhs)$set(rhs, drop = F)

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

  assertthat::assert_that(
    .is_regular(lhs),
    msg = "left-hand side of `%has_option%` is not a regular module name."
  )

  assertthat::assert_that(
    is.list(rhs),
    msg = "right-hand side of `%has_option%` is not a list."
  )

  module_option(lhs)$set(rhs, drop = T)

}

#' Syntactic sugar for setting module options.
#'
#' @export
`%has_options%` <- function(lhs, rhs) eval(substitute(`%has_option%`(lhs, rhs)),
                                  envir = parent.frame())
