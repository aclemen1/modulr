MODULR_NAMESPACE <- "modulr"
MODULR_NAME <-
  paste(MODULR_NAMESPACE, utils::packageVersion(pkg = "modulr"), sep = "#")
RESERVED_NAMES <- c(MODULR_NAMESPACE)

#' @title Special Module \code{'modulr'}
#' @description Access module helper functions.
#' @details The purpose of the special module \code{'modulr'} is to give access
#' to useful helper functions related to the module into which it is injected.
#' @format A list of functions.
#' \preformatted{
#' post_evaluation_hook(expr, add = FALSE)
#' message_info(...)
#' message_warn(...)
#' message_stop(...)
#'
#' ## Deprecated ##
#' get_module_name()
#' get_module_options()
#' get_filename()
#' get_dirname()}
#' @section \code{post_evaluation_hook(expr, add = FALSE)}:
#' Records the expression given as its argument as needing to be executed when
#' the current module evaluation exits (either naturally or as the result of an
#' error). If no expression is provided, i.e., the call is
#' `post_evaluation_hook()`, then the current expression is removed. If `add` is
#' `TRUE`, `expr` is added after any previously set expressions; otherwise (the
#' default) `expr` will overwrite any previously set expressions.
#' @section \code{message_info(...), message_warn(...), and message_stop(...)}:
#' Outputs an informative, warning, or critical and stopping message, prefixed
#' with a timestamp and the module name. Such messages are particularily useful
#' in modules involved in long chains of dependencies and workflows.
#' @section \code{get_module_name()}:
#' Returns a string (character vector of lenght one) containing the module name.
#' See \code{\link{define}}. \bold{Deprecated and kept for backward
#' compatibility.}
#' @section \code{get_module_version()}:
#' Returns numeric version of the module. \bold{Deprecated and kept for backward
#' compatibility.}
#' @section \code{get_module_options()}:
#' Returns a list containing the module options. See
#' \code{\link{module_options}}. \bold{Deprecated and kept for backward
#' compatibility.}
#' @section \code{get_filename(absolute = TRUE)}:
#' Returns a string (character vector of lenght one) containing the module
#' (absolute) filename. \bold{Deprecated and kept for backward
#' compatibility.}
#' @section \code{get_dirname(absolute = TRUE)}:
#' Returns a string (character vector of lenght one) containing the module
#' (absolute) dirname. \bold{Deprecated and kept for backward
#' compatibility.}
#' @seealso \code{\link{define}}, \code{\link{module_options}}, and
#'   \code{\link{reset}}.
#' @examples
#' reset()
#' define("foo", list(modulr = "modulr"), function(modulr) {
#'   list(
#'     info = function() modulr$message_info("Module name: ", .__name__),
#'     warn = function() modulr$message_warn("Module name: ", .__name__),
#'     stop = function() modulr$message_stop("Module name: ", .__name__)
#'   )
#' })
#' foo <- make()
#' foo$info()
#' foo$warn()
#'
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat(
#' 'define("foo", list(modulr = "modulr"), function(modulr) {
#'   modulr$message_info("Module file name: ", .__file__)
#'   modulr$message_info("Module file path: ", .__path__)
#' })', sep = "\n", file = tmp_file)
#' root_config$set(tmp_dir)
#' make("foo")
#' unlink(tmp_dir, recursive = TRUE)
#'
#' \dontrun{foo$stop()}
#'
#' reset()
#' "foo" %requires% list(modulr = "modulr") %provides% {
#'   modulr$post_evaluation_hook(touch("foo"))
#'   message("Hello, I am a ", sQuote("no-scoped"), " module.")
#' }
#' make("foo")
#' make("foo")
#'
#' reset()
#' "foo" %requires% list(modulr = "modulr") %provides% {
#'   modulr$post_evaluation_hook(undefine("foo"))
#'   message("Hello, I am an ", sQuote("ephemeral"), " module.")
#' }
#' make("foo")
#' \dontrun{make("foo")}
#'
#' @name modulr-module
#' @aliases post_evaluation_hook message_info message_warn message_stop
#'  get_module_name get_module_options get_filename get_dirname
NULL

define_modulr <- function() {

  define(MODULR_NAME, list(), function() {

    # Begin Exclude Linting

    #' # `modulr`
    #'
    #' Access module helper functions.
    #'
    #' The purpose of this special module is to give access to useful helper
    #' functions related to the module into which it is injected.
    #'
    #' ## Methods
    #'
    #' ### `post_evaluation_hook(expr, add = FALSE)`
    #'
    #' Records the expression given as its argument as needing to be executed
    #' when the current module evaluation exits (either naturally or as the
    #' result of an error). If no expression is provided, i.e., the call is
    #' `post_evaluation_hook()`, then the current expression is removed. If
    #' `add` is `TRUE`, `expr` is added after any previously set expressions;
    #' otherwise (the default) `expr` will overwrite any previously set
    #' expressions.
    #'
    #' ### `message_info(...)`, `message_warn(...)`, `message_stop(...)`
    #'
    #' Outputs an informative, warning, or critical and stopping message,
    #' prefixed with a timestamp and the module name. Such messages are
    #' particularily useful in modules involved in long chains of dependencies
    #' and workflows.
    #'
    #' ## Deprecated methods
    #'
    #' ### `get_module_name()`
    #'
    #' Returns a string (character vector of lenght one) containing the module
    #' name. Deprecated and kept for backward compatibility. Use `.__name__`
    #' instead.
    #'
    #' ### `get_module_version()`
    #'
    #' Returns the numeric version of the module. Deprecated and kept for
    #' backward compatibility. Use `.__version__` instead.
    #'
    #' ### `get_module_options()`
    #'
    #' Returns a list containing the module options. Deprecated and kept for
    #' backward compatibility.
    #'
    #' ### `get_filename()`
    #'
    #' Returns a string (character vector of lenght one) containing the module
    #' filename. Deprecated and kept for backward compatibility. Use `.__path__`
    #' and/or `.__file__` instead.
    #'
    #' ### `get_dirname()`
    #'
    #' Returns a string (character vector of lenght one) containing the module
    #' dirname. Deprecated and kept for backward compatibility. Use `.__path__`
    #' and/or `.__file__` instead.
    #'
    #' @noRd

    # End Exclude Linting

    .get_special_env <- function(which = c("wrapper", "instanciator")) {
      which <- match.arg(which)
      for (frame in sys.frames()) {
        if (exists(paste0(".__", which, "__"), where = frame)) return(frame)
      }
    }

    list(

      # returns module name
      # Deprecated and kept for backward compatibility.
      # nocov start
      get_module_name = function() {
        .deprecated(".__name__", old = "$get_module_name")
        get(".__name__", pos = parent.frame())
      },
      # nocov end

      # returns module version
      # Deprecated and kept for backward compatibility.
      # nocov start
      get_module_version = function() {
        .deprecated(".__version__", old = "$get_module_version")
        .parse_name(get(".__name__", pos = parent.frame()))$version
      },
      # nocov end

      # returns module options
      # Deprecated and kept for backward compatibility.
      # nocov start
      get_module_options = function() {

        suppressWarnings(.deprecated(msg = paste0(
          "Module options are deprecated. As a replacement, you can add a ",
          "dependency with a dedicated module containing an appropriate ",
          "mechanism for your options and configurations settings. "
        )))

        name <- get(".__name__", pos = parent.frame())
        module_options(name)$get_all()
      },
      # nocov end

      # returns module filename
      # Deprecated and kept for backward compatibility.
      # nocov start
      get_filename = function(absolute = TRUE) {
        .deprecated(".__file__", old = "$get_filename")
        name <- get(".__name__", pos = parent.frame())
        file <-
          if (!is.null(.modulr_env$injector$registry[[c(name, "filepath")]])) {
            .modulr_env$injector$registry[[c(name, "filepath")]]
          } else {
            trace <- stats::na.omit(names(.source_trace()))
            if (length(trace) > 0L)
              utils::tail(trace, 1L)
          }
        if (!is.null(file)) stats::setNames(
          ifelse(absolute, normalizePath, identity)(file), name)
      },
      # nocov end

      # returns module directory
      # Deprecated and kept for backward compatibility.
      # nocov start
      get_dirname = function(absolute = TRUE) {
        .deprecated(".__path__", old = "$get_dirname")
        name <- get(".__name__", pos = parent.frame())
        file <-
          if (!is.null(.modulr_env$injector$registry[[c(name, "filepath")]])) {
            .modulr_env$injector$registry[[c(name, "filepath")]]
          } else {
            trace <- stats::na.omit(names(.source_trace()))
            if (length(trace) > 0L)
              utils::tail(trace, 1L)
          }
        if (!is.null(file)) dirname(
          ifelse(absolute, normalizePath, identity)(file))
      },
      # nocov end

      # post-evaluation hook
      post_evaluation_hook = function(expr = NULL, add = FALSE) {
        do.call(
          "on.exit",
          args = list(
            substitute(expr),
            add = add
          ),
          envir = .get_special_env(which = "instanciator"))
      },

      # returns find_path function
      # Deprecated and kept for backward compatibility.
      # nocov start
      resolve_path = function(...) {
        .deprecated("$get_dirname', 'find_module' or 'find_path",
                            old = "$resolve_path")
        eval(.deprecated_resolve_path(...), envir = parent.frame(1L))
      },
      # nocov end

      # returns .resolve_mapping function
      # Deprecated and kept for backward compatibility.
      # nocov start
      resolve_mapping = function(...) {
        .deprecated(old = "$resolve_mapping")
        eval(.deprecated_resolve_mapping(...), envir = parent.frame(1L))
      },
      # nocov end

      # returns .message_info function
      message_info = function(...) {
        .message_info(
          ...,
          module_name = tryCatch(get(".__name__", pos = parent.frame()),
                                 error = function(e) NULL))
      },

      # returns .message_warn function
      message_warn = function(...) {
        .message_warn(
          ...,
          module_name = tryCatch(get(".__name__", pos = parent.frame()),
                                 error = function(e) NULL))
      },

      # returns .message_stop function
      message_stop = function(...) {
        .message_stop(
          ...,
          module_name = tryCatch(get(".__name__", pos = parent.frame()),
                                 error = function(e) NULL))
      }

    )

  })

}

#' Module Options Provider
#'
#' Construct a module provider intended for defining modules that can be used as
#' general purpose options and configuration containers.
#'
#' @param ... Named arguments used as default options. If void, no default
#'   option is set.
#'
#' @return A module provider which exposes an R environment. The default options
#'   arguments, if any, are used to assign the values to their corresponding
#'   names in the returned environment.
#'
#' @section Syntactic Sugars:
#'  \preformatted{name \%provides_options\% options}
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{\%<=\%}}, \code{\link{define}}, \code{\link{make}},
#'   \code{\link{\%provides\%}}, \code{\link{\%requires\%}},
#'   \code{\link{reset}}, and \code{\link{touch}}.
#'
#' @examples
#' reset()
#'
#' "foo/config" %provides% options_provider(upper = FALSE)
#'
#' "foo" %requires% list(config = "foo/config") %provides% {
#'   function() casefold("foo", upper = config$upper)
#' }
#'
#' foo %<=% "foo"
#' foo()
#'
#' config %<=% "foo/config"
#' config$upper <- TRUE
#' foo()
#'
#' "foo/config" %provides_options% list(upper = FALSE)
#' foo %<=% "foo"
#' foo()
#'
#' touch("foo/config")
#' "foo/config" %provides_options% list(upper = FALSE)
#' foo %<=% "foo"
#' foo()
#'
#' @aliases %provides_options%
#' @export
options_provider <- function(...) {

  enclos <- force(parent.frame())
  env <- new.env(parent = enclos)
  env$options <- substitute(as.list(c(...)))
  environment(options_provider_) <- env
  options_provider_

}

options_provider_ <- function() {
  #' Options module which exposes an R environment. See ?options_provider.
  as.environment(eval(get("options", inherits = TRUE)))
}

#' @export
`%provides_options%` <- function(lhs, options) {

  if (.is_called_from_within_module()) {
    warning("`%provides_options%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  eval(substitute(
    `%provides%`(lhs, options_provider(options))),
    envir = parent.frame(1L))
}
