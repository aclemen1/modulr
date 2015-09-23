# nocov start
.onLoad <- function(libname, pkgname) {

  reset(all = TRUE, .verbose = FALSE)

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

  if(utils::packageVersion("assertthat") < package_version("0.1.0.99")) {
    packageStartupMessage(
      paste0("Please update package 'assertthat' from Github at ",
             "https://github.com/hadley/assertthat ",
             "for a better user experience with error messages."))
  }

  for(profile in c(".modulr_profile", ".modulr_profile.R", ".modulr_profile.r"))
    if(file.exists(profile)) {
      source(profile)
      break
    }

}

# TODO: write documentation for .Last.name
# nocov end
