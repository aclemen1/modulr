.onLoad <- function(libname, pkgname) {
  assign("register", list(), pos = modulr_env)
  assign("config", list(modules=list()), pos = modulr_env)
  assign("verbosity", +Inf, pos = modulr_env)

  .reset_message_level()
  root_config$set(c("module", "modules", "lib", "libs", "."))
  define_modulr()
  activate_breadcrumbs()

  invisible()

}

.onAttach <- function(libname, pkgname) {
}
