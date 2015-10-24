RESERVED_NAMES <- c("modulr")

#' @title Special Module \code{'modulr'}
#' @description Access module metadata and helper functions.
#' @details The purpose of the special module \code{'modulr'} is to give access
#' to metadata and useful helper functions related to the module into which it
#' is injected.
#' @format A list of functions.
#' \preformatted{
#' get_module_name()
#' get_module_options()
#' get_filename()
#' get_dirname()
#' message_info(...)
#' message_warn(...)
#' message_stop(...)}
#' @section \code{get_module_name()}:
#' Returns a string (character vector of lenght one) containing the module name.
#' See \code{\link{define}}.
#' @section \code{get_module_options()}:
#' Returns a list containing the module options. See
#' \code{\link{module_options}}. \bold{Deprecated and kept for backward
#' compatibility.}
#' @section \code{get_filename()}:
#' Returns a string (character vector of lenght one) containing the module
#' filename.
#' @section \code{get_dirname()}:
#' Returns a string (character vector of lenght one) containing the module
#' dirname.
#' @section \code{message_info(...), message_warn(...), and message_stop(...)}:
#' Outputs an informative, warning, or critical and stopping message, prefixed
#' with a timestamp and the module name. Such messages are particularily useful
#' in modules involved in long chains of dependencies and workflows.
#' @seealso \code{\link{define}}, \code{\link{module_options}}, and
#'   \code{\link{reset}}.
#' @examples
#' reset()
#' define("foo", list(modulr = "modulr"), function(modulr) {
#'   module_name <- modulr$get_module_name()
#'   list(
#'     info = function() modulr$message_info("Module name: ", module_name),
#'     warn = function() modulr$message_warn("Module name: ", module_name),
#'     stop = function() modulr$message_stop("Module name: ", module_name)
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
#'   modulr$message_info("Module filename: ", modulr$get_filename())
#'   modulr$message_info("Module dirname: ", modulr$get_dirname())
#' })', sep = "\n", file = tmp_file)
#' root_config$set(tmp_dir)
#' make("foo")
#' unlink(tmp_dir, recursive = TRUE)
#'
#' \dontrun{foo$stop()}
#' @name modulr-module
#' @aliases get_module_name get_module_options get_filename get_dirname
#'   message_info message_warn message_stop
NULL

.define_modulr <- function() {

  define("modulr", list(), function() {

    # Begin Exclude Linting

    #' # `modulr`
    #'
    #' Access module metadata and helper functions.
    #'
    #' The purpose of this special module is to give access to metadata and
    #' useful helper functions related to the module into which it is injected.
    #'
    #' ## Methods
    #'
    #' ### `get_module_name()`
    #'
    #' Returns a string (character vector of lenght one) containing the module
    #' name.
    #'
    #' ### `get_module_options()`
    #'
    #' Returns a list containing the module options. Deprecated and kept for
    #' backward compatibility.
    #'
    #' ### `get_filename()`
    #'
    #' Returns a string (character vector of lenght one) containing the module
    #' filename.
    #'
    #' ### `get_dirname()`
    #'
    #' Returns a string (character vector of lenght one) containing the module
    #' dirname.
    #'
    #' ### `message_info(...)`, `message_warn(...)`, `message_stop(...)`
    #'
    #' Outputs an informative, warning, or critical and stopping message,
    #' prefixed with a timestamp and the module name. Such messages are
    #' particularily useful in modules involved in long chains of dependencies
    #' and workflows.
    #'

    # End Exclude Linting

    list(

      # returns module name
      get_module_name = function() {
        get(".__name__", pos = parent.frame())
      },

      # returns module options
      # Deprecated and kept for backward compatibility.
      get_module_options = function() {
        name <- get(".__name__", pos = parent.frame())
        module_options(name)$get_all()
      },

      # returns module filename
      get_filename = function() {
        name <- get(".__name__", pos = parent.frame())
        .resolve_path(name)
      },

      # returns module directory
      get_dirname = function() {
        name <- get(".__name__", pos = parent.frame())
        file <- .resolve_path(name)
        if (is.null(file)) return(NULL)
        dirname(file)
      },

      # returns .resolve_path function
      # Deprecated and kept for backward compatibility.
      resolve_path = .resolve_path,

      # returns .resolve_mapping function
      # Deprecated and kept for backward compatibility.
      resolve_mapping = .resolve_mapping,

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
