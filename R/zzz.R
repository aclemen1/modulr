.onLoad <- function(libname, pkgname) {

  assign("register", list(), pos = modulr_env)
  assign("config", list(modules = list()), pos = modulr_env)
  assign("verbosity", +Inf, pos = modulr_env)
  assign(".Last.name", NULL, pos = modulr_env)

  .define_modulr()

  root_config$set(c("module", "modules", "lib", "libs", "."))

  activate_breadcrumbs()

  invisible()

}

.onAttach <- function(libname, pkgname) {

  makeActiveBinding(
    as.symbol(".Last.name"),
    function() {
      .get_0(".Last.name", envir = modulr_env, ifnotfound = NULL)
    },
    env = as.environment("package:modulr"))

}

# TODO: write documentation for .Last.name
