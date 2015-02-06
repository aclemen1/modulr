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
      message_info = message_info,
      message_warn = message_warn,
      message_stop = message_stop
    )
  })
}
