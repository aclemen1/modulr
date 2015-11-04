#' @include modulr.R

.config <- function(scope) {

  assert_that(missing(scope) || is.character(scope))

  if (missing(scope)) return(NULL)

  unset <- function() {

    modulr_env$config[[scope]] <- NULL

  }

  set <- function(..., drop = TRUE) {

#     .message_meta("Entering .config$set() ...",
#                   verbosity = +Inf)

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

#     .message_meta("Entering .config$get_all() ...",
#                   verbosity = +Inf)

    if (is.na(scope[2])) {

      modulr_env$config[[scope[1]]]

    } else {

      modulr_env$config[[scope[1:2]]]

    }

  }

  get <- function(key) {

#     .message_meta("Entering .config$get() ...",
#                   verbosity = +Inf)

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

#' @title Configurations.
#' @description Getters and setters for various configurations.
#' @format A list of getters and setters for a configuration.
#'  \preformatted{
#'  unset()
#'  set(..., drop = TRUE)
#'  get(key)
#'  get_all()
#'  }
#' @section Arguments:
#' \describe{
#' \item{\code{...}}{A (named) list of options.}
#' \item{\code{key}}{A string (character vector of length one).}
#' \item{\code{drop}}{A flag. Should previous options be dropped and replaced?}
#' }
#' @seealso \code{\link{define}}, \code{\link{get_configs}}, and \code{\link{reset}}.
#' @examples
#' reset()
#' root_config$get_all()
#' root_config$unset()
#' root_config$get_all()
#' root_config$set(c("my_modules"))
#' root_config$get_all()
#' root_config$set(c("my_great_modules"), drop = FALSE)
#' root_config$get_all()
#' @name config
#' @aliases get get_all unset set
NULL

#' @rdname config
#' @usage root_config
#' @export
root_config <-
  .config(".__root__")

#' @rdname config
#' @usage paths_config
#' @export
paths_config <-
  .config(".__paths__")

#' @rdname config
#' @usage maps_config
#' @export
maps_config <-
  .config(".__maps__")

#' Get Configurations.
#'
#' Get all configurations.
#'
#' @examples
#' reset()
#' get_configs()
#'
#' @seealso \code{\link{config}} and \code{\link{reset}}.
#' @export
get_configs <- function() get("config", pos = modulr_env)

#' Module Options.
#'
#' Getters and setters for module options.
#' \bold{Deprecated and kept for backward compatibility.}
#'
#' @inheritParams define
#'
#' @return A list of getters and setters. See \code{\link{config}}.
#'
#' @section Syntactic Sugars:
#'  \code{name \%has_default_option\% options} and
#'  \code{name \%has_default_options\% options} for
#'    \code{module_options(name)$set(options, drop = FALSE)}.
#'
#'  \code{name \%has_option\% options} and
#'  \code{name \%has_options\% options} for
#'    \code{module_options(name)$set(options, drop = TRUE)}.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @section Warning:
#' Deprecated and kept for backward compatibility.
#'
#' @aliases %has_default_option% %has_default_options% %has_option%
#'   %has_options%
#' @export
module_options <- function(name = .Last.name) {

  .Deprecated(msg = paste0(
    "Module options are deprecated. As a replacement, you can add a ",
    "dependency with a dedicated module containing an appropriate mechanism ",
    "for your options and configurations settings. "
  ))

  assert_that(.is_regular(name))

  .config(c("modules", name))

}

#' @rdname module_options
#' @inheritParams define
#' @param options A (named) list of options.
#' @export
`%has_default_option%` <- function(name, options) {

  assert_that(
    .is_regular(name),
    msg =
      "left-hand side of `%has_default_option%` is not a regular module name."
  )

  assert_that(
    is.list(options),
    msg = "right-hand side of `%has_default_option%` is not a list."
  )

  module_options(name)$set(options, drop = FALSE)

}

#' @rdname module_options
#' @export
`%has_default_options%` <-
  function(name, options)
    eval(substitute(`%has_default_option%`(name, options)),
         envir = parent.frame())

#' @rdname module_options
#' @export
`%has_option%` <- function(name, options) {

  assert_that(
    .is_regular(name),
    msg = "left-hand side of `%has_option%` is not a regular module name."
  )

  assert_that(
    is.list(options),
    msg = "right-hand side of `%has_option%` is not a list."
  )

  module_options(name)$set(options, drop = TRUE)

}

#' @rdname module_options
#' @export
`%has_options%` <- function(name, options)
  eval(substitute(`%has_option%`(name, options)),
       envir = parent.frame())
