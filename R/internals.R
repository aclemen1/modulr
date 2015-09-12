#' Get internals
#'
#' @export
get_internals <- function() {

  list(
    env = modulr_env,
    register = get("register", pos = modulr_env),
    config = get("config", pos = modulr_env),
    verbosity = get("verbosity", pos = modulr_env)
  )

}
