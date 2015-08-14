.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "Let's modules address complication, and\n",
    "Dependency Injection manage complexity.\n",
    "\n",
    "             -- The modulr Team\n")
}

.onLoad <- function(libname, pkgname) {
  assign("register", list(), pos = modulr_env)
  assign("configuration", list(modules=list()), pos = modulr_env)
  assign("message_handler", NULL, pos = modulr_env)
  assign("message_closed", "", pos = modulr_env)

  define_modulr()
  activate_breadcrumbs()
}
