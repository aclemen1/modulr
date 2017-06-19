# nocov start
.onLoad <- function(libname, pkgname) {  # Exclude Linting

  .modulr_env$old_modulr_options <- options(
    modulr.digits = getOption(
      "modulr.digits",
      DEFAULT_DIGITS),
    modulr.ignore_packages = getOption(
      "modulr.ignore_packages",
      DEFAULT_IGNORE_PACKAGES),
    modulr.deparse.max.lines.in.pipes = getOption(
      "modulr.deparse.max.lines.in.pipes",
      DEFAULT_DEPARSE_MAX_LINES_IN_PIPES),
    modulr.hit_suffix = getOption(
      "modulr.hit_suffix",
      default = DEFAULT_HIT_SUFFIX),
    modulr.gears_path = getOption(
      "modulr.gears_path",
      default = DEFAULT_GEARS_PATH)
  )

  reset(all = TRUE, .verbose = FALSE)

  invisible(NULL)

}

.onAttach <- function(libname, pkgname) {  # Exclude Linting

  .modulr_env$error_option <- reactivate_breadcrumbs()

  makeActiveBinding(
    as.symbol(".Last.name"),
    function() {
      .get_0(".Last.name", envir = .modulr_env$injector, ifnotfound = NULL)
    },
    env = as.environment("package:modulr"))

  makeActiveBinding(
    as.symbol(".Last.packages_manifest"),
    function() {
      .get_0(
        ".Last.packages_manifest", envir = .modulr_env, ifnotfound = NULL)
    },
    env = as.environment("package:modulr"))

  makeActiveBinding(
    as.symbol(".SharedEnv"),
    sharedenv,
    env = as.environment("package:modulr"))

  makeActiveBinding(
    as.symbol(".__name__"),
    function() {
      .get_0(".__name__", envir = .modulr_env$injector,
             ifnotfound = "__main__")
    },
    env = as.environment("package:modulr"))

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (utils::packageVersion("rstudioapi") < package_version("0.5")) {
      packageStartupMessage(
        paste0("Please update RStudio or package 'rstudioapi' ",
               "for a better user experience regarding add-ins."))
    }
  }

  if (utils::packageVersion("assertthat") < package_version("0.1.0.99")) {
    packageStartupMessage(
      paste0("Please update package 'assertthat' from GitHub at ",
             "https://github.com/hadley/assertthat ",
             "for a better user experience regarding error messages."))
  }

  for (profile in
       c(".modulr_profile", ".modulr_profile.R", ".modulr_profile.r"))
    if (file.exists(profile)) {
      source(profile)
      break
    }

  packageStartupMessage(sprintf(
    "This is modulr version %s", utils::packageVersion("modulr")))

  invisible(NULL)

}

.onDetach <- function(libpath) {  # Exclude Linting

  if (exists("error_option", where = .modulr_env)) {
    options(error = .modulr_env$error_option)
  }

  invisible(NULL)

}

.onUnload <- function(libpath) {  # Exclude Linting

  options(.modulr_env$old_modulr_options)

  invisible(NULL)

}

# nocov end
