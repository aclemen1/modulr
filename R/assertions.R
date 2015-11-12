# Test if a module is already defined.
.is_defined <- function(name) {
  assert_that(assertthat::is.string(name))
  name %in% names(modulr_env$register)
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

.is_conform <- function(name) {
  assert_that(assertthat::is.string(name))
  !grepl("[^a-zA-Z0-9_/-]", name)
}

assertthat::on_failure(.is_conform) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " contains reserved characters.")
}

# Test if a module has a regular name.
.is_regular <- function(name) {
  assert_that(assertthat::is.string(name))
  !(name %in% RESERVED_NAMES) & .is_conform(name)
}

assertthat::on_failure(.is_regular) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is reserved.")
}

# Test if a module has a regular name which is not intended for testing
# or special purposes
.is_regular_core <- function(name) {
  .is_regular(name) &&
    !grepl(paste0("\\/tests?$|\\/tests?\\/|",
                  "\\/mocks?$|\\/mocks?\\/|",
                  "\\/examples?$|\\/examples?\\/"),
           name, ignore.case = TRUE)
}

assertthat::on_failure(.is_regular_core) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " is reserved or intended for testing or special purposes.")
}

# Test if a module name is reserved.
.is_reserved <- function(name) {
  name %in% RESERVED_NAMES
}

assertthat::on_failure(.is_reserved) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is not reserved.")
}

# Test if a module is defined and has a regular name.
.is_defined_regular <- function(name) {
  .is_defined(name) && .is_regular(name)
}

assertthat::on_failure(.is_defined_regular) <- function(call, env) {
  paste0(
    deparse(eval(call$name, envir = env)),
    " is undefined and/or reserved.")
}

# Test if a call is made from within a module
.is_called_from_within_module <- function() {
  isTRUE(exists(".__name__", inherits = TRUE,
         mode = "character", envir = parent.frame(2L)))
}

# Test if object is a braced expression
.is_braced_expression <- function(object) {
  is.call(object) && object[[1]] == as.name("{")
}

# Test if object is a constant
.is_constant <- function(object) {
  is.null(object) || (
    assertthat::is.scalar(object) &&
      !is.list(object) &&
      !is.function(object) &&
      !is.expression(object) &&
      !is.language(object))
}
