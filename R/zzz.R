.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n",
    "Enjoy Dependency Injection and Module Pattern in R with modulr!", "\n")
}

.onLoad <- function(libname, pkgname) {
  RESERVED_NAMES <<- c("modulr")

  modulr_env <<- new.env()

  assign("register", list(), pos = modulr_env)
  assign("configuration", list(modules=list()), pos = modulr_env)
  assign("message_handler", NULL, pos = modulr_env)
  assign("message_closed", "", pos = modulr_env)

  define_modulr()
}
