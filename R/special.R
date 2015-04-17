define_modulr <- function() {
  define("modulr", list(), function() {
    list(
      get_module_options = function()
        module_option(get(".__name__", pos = parent.frame()))$get_all(),
      get_module_name = function()
        get(".__name__", pos = parent.frame()),
      get_filename = function() {
        name <- get(".__name__", pos = parent.frame())
        resolve_path(name)
      },
      get_dirname = function() {
        name <- get(".__name__", pos = parent.frame())
        dirname(resolve_path(name))
      },
      resolve_path = resolve_path,
      message_info = function(...)
        message_info(
          ...,
          module_name = tryCatch(get(".__name__", pos = parent.frame()),
                                 error = function(e) NULL)),
      message_warn = function(...)
        message_warn(
          ...,
          module_name = tryCatch(get(".__name__", pos = parent.frame()),
                                 error = function(e) NULL)),
      message_stop = function(...)
        message_stop(
          ...,
          module_name = tryCatch(get(".__name__", pos = parent.frame()),
                                 error = function(e) NULL))
    )
  })
}
