# Test if a module is already defined.
.is_defined <- function(name) {
  assertthat::see_if(assertthat::is.string(name)) &&
    name %in% names(.modulr_env$injector$registry)
}

assertthat::on_failure(.is_defined) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is not defined.")
}

# Test if a module is undefined.
.is_undefined <- function(name) {
  !.is_defined(name)
}

assertthat::on_failure(.is_undefined) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)), " is defined.")
}

.is_version <- function(version) {
  inherits(version, "numeric_version")
}

assertthat::on_failure(.is_version) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " is not an element of the numeric version class.")
}

.version_symbol_regex <- "(?:[~^]|(?:>=))"

.is_version_symbol <- function(symbol) {
  assertthat::see_if(is.na(symbol) || assertthat::is.string(symbol)) &&
    (is.na(symbol) || grepl(sprintf("^%s$", .version_symbol_regex), symbol))
}

assertthat::on_failure(.is_version_symbol) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " is an invalid version symbol.")
}

.version_string_regex <-
  sprintf("(%s)?(\\d+(?:\\.\\d+){0,3})", .version_symbol_regex)
.version_hash_string_regex <- sprintf("(?:(?:#%s))", .version_string_regex)

.is_version_string <- function(string) {
  assertthat::see_if(assertthat::is.string(string) || is.na(string)) &&
    (is.na(string) || grepl(sprintf("^%s$", .version_string_regex), string))
}

assertthat::on_failure(.is_version_string) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " is an invalid (semantic) version.")
}

.conform_regex <- paste0(
  "^$|",
  "^((?:[a-zA-Z0-9_-]+)(?:/[a-zA-Z0-9_-]+)*)",
  .version_hash_string_regex, "?",
  "((?:/[a-zA-Z0-9_-]+)*)$")

.is_conform <- function(name) {
  assertthat::see_if(assertthat::is.string(name)) &&
    grepl(.conform_regex, name)
}

assertthat::on_failure(.is_conform) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " contains reserved characters or is malformed.")
}

# Test if a name does not contain a semantic versioning symbol.
.is_exact <- function(name) {
  assertthat::see_if(.is_conform(name)) &&
    is.na(.parse_name(name)$symbol)
}

assertthat::on_failure(.is_exact) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " contains a prefixed version number.")
}

# Test if a name does not contain a semantic version.
.is_namespace <- function(namespace) {
  assertthat::see_if(.is_exact(namespace)) &&
    is.na(.parse_name(namespace)[["version"]])
}

assertthat::on_failure(.is_namespace) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " contains a version.")
}

# Test if a module has a regular name.
.is_regular <- function(name) {
  assertthat::see_if(assertthat::is.string(name)) &&
    !(.parse_name(name)[["namespace"]] %in% RESERVED_NAMES) && .is_conform(name)
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
  .parse_name(name)[["namespace"]] %in% RESERVED_NAMES
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
    " is not defined and/or reserved.")
}

# Test if a call is made from within a module
.is_called_from_within_module <- function() {
  isTRUE(exists(".__wrapper__", inherits = TRUE,
         mode = "logical", envir = parent.frame(2L)))
}

# Test if a load is nested
.is_nested_load <- function() {
  any(
    vapply(
      seq_along(sys.frames())[-length(sys.frames())],
      function(x) identical(sys.function(x), .load_module),
      FUN.VALUE = T))
}

# Test if object is a braced expression
.is_braced_expression <- function(object) {
  is.call(object) && object[[1L]] == as.name("{")
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

# Test if object is a packages manifest
.is_packages_manifest <- function(manifest) {
  inherits(manifest, "packages_manifest")
}

assertthat::on_failure(.is_packages_manifest) <- function(call, env) {
  paste0(deparse(eval(call$name, envir = env)),
         " is not a packages manifest.")
}
