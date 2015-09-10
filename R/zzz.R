.onLoad <- function(libname, pkgname) {

  reset(all = T, verbose = F)

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
