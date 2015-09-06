# Test if a module is already defined.
.is_defined <- function(name) {
  assertthat::assert_that(assertthat::is.string(name))
  name %in% names(.internals()$register)
}

assertthat::on_failure(.is_defined) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is undefined.")
}

# Test if a module is undefined.
.is_undefined <- function(name) {
  !.is_defined(name)
}

assertthat::on_failure(.is_undefined) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is defined.")
}

# Test if a module has a regular name.
.is_regular <- function(name) {
  assertthat::assert_that(assertthat::is.string(name))
  !(name %in% RESERVED_NAMES)
}

assertthat::on_failure(.is_regular) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is special.")
}

# Test if a module name is special.
.is_special <- function(name) {
  !.is_regular(name)
}

assertthat::on_failure(.is_special) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is regular.")
}

# Test if a module is defined and has a regular name.
.is_defined_regular <- function(name) {
  .is_defined(name) && .is_regular(name)
}

assertthat::on_failure(.is_defined_regular) <- function(call, env) {
  paste0(
    deparse(eval(call$name, envir = env)),
    " is undefined and/or special.")
}
