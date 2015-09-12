RESERVED_NAMES <- c("modulr")

# TODO: write documentation

.define_modulr <- function() {

  define("modulr", list(), function() {

    list(

      # returns module name
      get_module_name = function() {
        get(".__name__", pos = parent.frame())
      },

      # returns module options
      get_module_options = function() {
        name <- get(".__name__", pos = parent.frame())
        module_option(name)$get_all()
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
        if(is.null(file)) return(NULL)
        dirname(file)
      },

      # returns .resolve_path function
      resolve_path = .resolve_path,

      # returns .resolve_mapping function
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
