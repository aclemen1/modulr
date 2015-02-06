#' Get internals
#'
#' @export
.internals <- function() {
  list(
    env = modulr_env,
    register = get("register", pos = modulr_env),
    configuration = get("configuration", pos = modulr_env)
  )
}
