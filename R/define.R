# Compute a SHA-1 digest of an R object
.hash <- function(object) {

  digest::digest(object, "sha1")

}

#' Get the Digest of a Module.
#'
#' Get the digest (a SHA-256 hash of the dependencies and factory) of a module.
#'
#' @inheritParams define
#' @param load A flag. Should an undefined module be implicitely loaded?
#'
#' @return A string containing the digest of the module.
#'
#' @details
#'
#'  A digest is useful for comparing two modules. For instance, in order to know
#'  if a module definition has changed, a digest of the newly presented module
#'  is computed and compared to the existing one. When a module is imported from
#'  an URL, it is also possible to specify a digest. The imported module is then
#'  rejected if its digest differs from the expected value.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{define}} and \code{\link{root_config}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() NULL)
#' get_digest("foo")
#'
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() NULL)', file = tmp_file)
#' root_config$set(tmp_dir)
#' \dontrun{get_digest("foo", load = FALSE)}
#' get_digest("foo", load = TRUE)
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @export
get_digest <- function(name = .Last.name, load = FALSE) {

  .message_meta(sprintf("Entering get_digest() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(assertthat::is.flag(load))

  if (.is_undefined(name) & load) {

    if (.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)
  }

  assert_that(.is_defined(name))

  .hash(c(
    deparse(modulr_env$register[[c(name, "dependencies")]]),
    deparse(modulr_env$register[[c(name, "factory")]])))

}

#' Define a Module.
#'
#' Define or redefine a module by name, dependencies, and factory.
#'
#' @param name A string (character vector of lenght one).
#'
#'   A module name can contain letters, figures and some special characters,
#'   namely \code{_}, \code{-}, and \code{/}. The latter is a namespace
#'   separator.
#'
#'   Names containing \code{/mock/}, \code{/mocks/}, \code{/test/},
#'   \code{/tests/}, \code{/example/}, or \code{/examples/} have a special
#'   meaning related to code testing and examples.
#'
#'   The name "modulr" corresponds to a special module and is therefore
#'   reserved.
#'
#' @param dependencies A (preferably named) list of strings.
#'
#'   Elements of the list of dependencies designate modules by their name.
#'
#' @param factory A function.
#'
#'   The formals of the factory must coincide with the list of dependencies.
#'
#' @return A wrapper function around a make call for the defined module.
#'
#' @details
#'
#' The definition of a module can be done explicitly in the console, implicitly
#' from a file on a disk, or remotely at a given URL via the HTTP(S) protocol.
#' These three ways of defining modules have their specificities.
#'
#' \describe{
#' \item{Explicit Definition}{
#' This is the most direct method to define or redefine a module. This is also
#' the most volatile since the lifespan of the module is limited to the R
#' session. When a new module is defined, the internal state of the package
#' is modified to record its name, dependencies and factory. Some other useful
#' metadata are also recorded, like timestamps, various flags and counters, and
#' a digest. When an existing module is redefined, the internal state is updated
#' accordingly, unless no change is detected by digests comparison. No other
#' side-effect occurs during the definition process, notably the evaluation of
#' the factory which is postponed to a subsequent \code{\link{make}} call.
#' }
#'
#' \item{Implicit Definition}{ This is the natural
#' method to choose when a module is intended to be reused. In such a case, the
#' definition takes place in a dedicated file, which name is closely related to
#' the module's name.
#'
#' As a file \code{/home/user/readme.txt} is composed of a path
#' \code{/home/user} and a file name \code{readme.txt}, a module name
#' \code{vendor/tool/swissknife} is similarily composed of a namespace
#' \code{vendor/tool} and a local name \code{swissknife}. For modulr to find
#' this module, it is sufficient to store its definition in an R, R Markdown or
#' R Sweave file named \code{swissknife.R[md|nw]} (R files have precedence over
#' Rmd's and Rnw's), laid out on disk in the \code{vendor/tool} path, relative
#' to the modulr root directory (see \code{\link{root_config}}).
#'
#' \itemize{
#' \item \code{vendor/}
#' \itemize{
#' \item \code{tool/}
#' \itemize{
#' \item \code{swissknife.R},
#'
#' contains the "vendor/tool/swissknife" definition.}}}
#'
#' Each time the definition of a module is needed, modulr resolves its name into
#' a file location by applying the following configurable rules.
#'
#' \enumerate{
#' \item
#' The \code{\link{root_config}} accessor acts at the \emph{filesystem level},
#' by specifying the root directory, relative to which all paths are expressed.
#' For instance, \code{root_config$set("./lib")} tells modulr that all modules
#' are to be found in \code{lib} (in the R working directory). The directory
#' path can be relative (e.g. \code{./lib}) or absolute (e.g.
#' \code{/home/user/lib}). By default, modulr looks in turn into the following
#' directories \code{"./module"}, \code{"./modules"}, \code{"./lib"},
#' \code{"./libs"}, and \code{"."}.
#'
#' \item The \code{\link{paths_config}} accessor acts at the \emph{namespace
#' level}, by mapping a specific namespace to a dedicated path, relative to the
#' root directory. For instance, \code{paths_config$set("vendor" =
#' "third_parties/vendor")} will map the \code{vendor/great_module} to the
#' \code{third_parties/vendor/great_module.R} path, relative to the root
#' directory.
#' \itemize{
#' \item \code{third_parties}
#'
#' is intended to be a dedicated container for third-parties modules.
#' \itemize{
#' \item \code{vendor}
#' \itemize{
#' \item \code{great_module.R}
#'
#' contains the "vendor/great_module" definition.
#' }
#' }
#' }
#' \item The \code{\link{maps_config}} accessor acts at the \emph{module level},
#' by substituting specific dependencies within the scope of a given module.
#' This is especially useful in a situation where a dependency has been
#' replaced by a newer version, but a module still needs to rely on the previous
#' one. For instance, \code{maps_config$set("foo/bar" =
#' list("vendor/great_module" = "vendor/old_great_module"))} tells modulr that
#' for the module \code{foo/bar} only, the dependency \code{vendor/great_module}
#' must be replaced by \code{vendor/old_great_module}. }
#'
#' \itemize{
#' \item \code{foo}
#' \itemize{
#' \item \code{bar.R}
#'
#' depends on \code{vendor/great_module} by definition, but will be replaced by
#' \code{vendor/old_great_module} when needed.
#' }
#' \item \code{vendor}
#' \itemize{
#' \item \code{great_module.R}
#'
#' serves all modules that depend on it, except \code{foo/bar}.
#' \item \code{old_great_module.R}
#'
#' serves \code{foo/bar} only.
#' }
#' }
#'
#' These rules are applied in reverse order, substituting dependencies first,
#' then mapping namespaces and finally expressing the absolute path, relative to
#' the modulr root directory.
#'
#' It is possible to store several definitions into one main file. By doing so,
#' the implicit definition of the main module (i.e. the module for which the
#' name is resolved into the file location) triggers the simultaneous definition
#' of a bunch of related modules. It is notably desirable for test purposes,
#' when a module and its dependencies have to be mocked and injected into a new,
#' testing module.
#'
#' }
#' \item{Remote Definition}{
#' This is the method used to share a module via the HTTP(S) protocol. The
#' module is thus served at a given URL and has to be imported (see
#' \code{\link{import_module}}) in order to be defined and used. Like files, it
#' is possible to store several related definitions at one URL. Public and
#' private Gists, files on Github, and any HTTP server can be used to share so
#' called \emph{modulr gears}.
#' }
#' }
#'
#' @section Syntactic Sugars:
#'  \preformatted{name \%provides\% factory}
#'  \preformatted{name \%requires\% dependencies \%provides\% factory}
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{.Last.name}}, \code{\link{graph_dependencies}},
#'   \code{\link{import_module}}, \code{\link{make}},
#'   \code{\link{maps_config}}, \code{\link{paths_config}}, \code{\link{reset}},
#'   \code{\link{touch}}, and \code{\link{undefine}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() "Hello")
#' bar <- define("bar", list(foo = "foo"), function(foo) paste(foo, "World!"))
#' bar()
#' define("foo", NULL, function() "Again, Hello")
#' bar()
#'
#' reset()
#' "foo" %provides% function() "Hello"
#' "bar" %requires%
#'   list(foo = "foo") %provides%
#'   function(foo) paste(foo, "World!")
#' make()
#' "foo" %provides% function() "Again, Hello"
#' make("bar")
#'
#' reset()
#' define("A", list(b = "B"), function(b) NULL)
#' define("B", list(a = "A"), function(a) NULL)
#' \dontrun{make()}
#'
#' reset()
#' define("A", NULL, function() NULL)
#' define("B", NULL, function() NULL)
#' define("C", list(a = "A"), function(a) NULL)
#' define("D", list(a = "A", b = "B"), function(a, b) NULL)
#' define("E", list(d = "D"), function(d) NULL)
#' define("F", list(c = "C", d = "D", e = "E"), function(c, d, e) NULL)
#' graph_dependencies()
#' make()
#'
#' @aliases %requires% %provides%
#' @export
define <- function(name, dependencies, factory) {

  .message_meta(sprintf("Entering define() for '%s' ...", name),
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("define is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.string(name),
    is.function(factory))

  assert_that(
    .is_regular(name) | .is_reserved(name),
    msg = "module name is not regular nor reserved.")

  assert_that(
    is.null(dependencies) || is.list(dependencies),
    is.function(factory)
    )

  assert_that(
    is.null(dependencies) || (
      setequal(names(dependencies), names(formals(factory))) || (
        assertthat::are_equal(length(dependencies),
                              length(formals(factory))) &&
          is.null(names(dependencies)))),
    msg = "dependencies and formals are not matching.")

  if (is.null(dependencies)) dependencies <- list()

  timestamp <- Sys.time()

  if (.is_undefined(name)) {

    .message_meta(
      sprintf("Defining '%s'", name), {

        modulr_env$register[[name]] <- list()
        modulr_env$register[[c(name, "name")]] <- name
        modulr_env$register[[c(name, "dependencies")]] <- dependencies
        modulr_env$register[[c(name, "factory")]] <- factory
        modulr_env$register[[c(name, "digest")]] <- .hash(c(
          deparse(dependencies),
          deparse(factory)))
        modulr_env$register[[c(name, "instance")]] <- NULL
        modulr_env$register[[c(name, "instanciated")]] <- F
        modulr_env$register[[c(name, "calls")]] <- 0
        modulr_env$register[[c(name, "duration")]] <- NA_integer_
        modulr_env$register[[c(name, "first_instance")]] <- T
        modulr_env$register[[c(name, "url")]] <- NULL
        modulr_env$register[[c(name, "timestamp")]] <- timestamp
        modulr_env$register[[c(name, "created")]] <- timestamp

    },
    ok = TRUE, verbosity = ifelse(.is_regular(name), 2, 3))



  } else if (.is_regular(name)) {

    previous_digest <- modulr_env$register[[c(name, "digest")]]
    digest <- .hash(c(
      deparse(dependencies),
      deparse(factory)))
    if (digest != previous_digest) {
      .message_meta(sprintf("Re-defining '%s'", name), {

        modulr_env$register[[c(name, "dependencies")]] <- dependencies
        modulr_env$register[[c(name, "factory")]] <- factory
        modulr_env$register[[c(name, "digest")]] <- digest
        modulr_env$register[[c(name, "instance")]] <- NULL
        modulr_env$register[[c(name, "instanciated")]] <- F
        modulr_env$register[[c(name, "calls")]] <- 0
        modulr_env$register[[c(name, "duration")]] <- NA_integer_
        modulr_env$register[[c(name, "first_instance")]] <- F
        modulr_env$register[[c(name, "url")]] <- NULL
        modulr_env$register[[c(name, "timestamp")]] <- timestamp

      },
      ok = TRUE, verbosity = 1)

    }
  } else {
    assert_that(.is_regular(name))
  }

  if (.is_regular_core(name))
    modulr_env$.Last.name <- name

  invisible(function(...) make(name, ...))

}

#' Get the Factory of a Module.
#'
#' Get the factory function of a module.
#'
#' @inheritParams define
#' @param load A flag. Should an undefined module be implicitely loaded?
#'
#' @return A function identical to the factory function of the module.
#'
#' @details
#'
#'  For testing purposes, it is often useful for mocks to be able to refer to
#'  the factory of a module.
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{define}}, \code{\link{make}}, \code{\link{reset}},
#'   and \code{link{root_config}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() "foo")
#' define("bar", list(foo = "foo"), function(foo) paste0(foo, "bar"))
#' define("foo/mock", NULL, function() "foooooo")
#' define("bar/mock", list(foo = "foo/mock"), get_factory("bar"))
#' make("bar/mock")
#'
#' reset()
#' tmp_dir <- tempfile("modulr_")
#' dir.create(tmp_dir)
#' tmp_file <- file.path(tmp_dir, "foo.R")
#' cat('define("foo", NULL, function() "Hello World!")', file = tmp_file)
#' root_config$set(tmp_dir)
#' \dontrun{get_factory("foo", load = FALSE)}
#' get_factory("foo", load = TRUE)
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @export
get_factory <- function(name = .Last.name, load = FALSE) {

  .message_meta(sprintf("Entering get_factory() for '%s' ...", name),
                verbosity = +Inf)

  assert_that(assertthat::is.flag(load))

  if (.is_undefined(name) & load) {

    if (.is_called_from_within_module()) {
      warning("get_factory is called from within a module.",
              call. = FALSE, immediate. = TRUE)
    }

    load_module(name)
  }

  assert_that(.is_defined(name))

  modulr_env$register[[c(name, "factory")]]

}

#' Reset the Modulr Internal State.
#'
#' Reset the modulr internal state.
#'
#' @param all A flag. Should stashes be also dropped?
#' @param .verbose A flag. For internal use only. Should be verbose?
#'
#' @details
#'
#'  Reset the modulr internal state: definitions and configurations are dropped,
#'  verbosity is set to default (see \code{\link{set_verbosity}}), .Last.name is
#'  set to NULL (see \code{\link{.Last.name}}), and root directory is set to
#'  default (see \code{\link{root_config}}). After a reset, the special module
#'  \code{modulr} is automatically defined. If \code{all} is set to \code{TRUE},
#'  stashes are also dropped (see \code{\link{stash}}).
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{.Last.name}}, \code{\link{define}},
#'   \code{\link{list_modules}}, \code{\link{list_stashes}},
#'   \code{\link{reset}}, \code{\link{root_config}},
#'   \code{\link{set_verbosity}}, and \code{\link{stash}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() NULL)
#' root_config$set("./foobar")
#' stash(comment = "foo stash")
#' set_verbosity(+Inf)
#' reset()
#' list_modules()
#' list_stashes()
#' root_config$get_all()
#' .Last.name
#' reset(all = T)
#' list_stashes()
#'
#' @export
reset <- function(all = FALSE, .verbose = TRUE) {

  .message_meta("Entering reset() ...",
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("reset is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    assertthat::is.flag(all),
    assertthat::is.flag(.verbose))

  .message_meta(
    if (.verbose) "Resetting modulr state", {

      modulr_env$register <- list()
      modulr_env$config <- list(modules = list())
      modulr_env$verbosity <- 2
      modulr_env$.Last.name <- NULL
      if (all)
        modulr_env$stash <- list()

      .define_modulr()

      root_config$set(c("module", "modules", "lib", "libs", "."))

    },
    ok = TRUE, verbosity = 2)

  invisible()

}

#' Undefine a Module.
#'
#' Undefine a module by dropping it definition from the modulr internal state.
#'
#' @inheritParams define
#'
#' @section Warning:
#'  It is considered a very bad practice to define, touch, undefine, load, make,
#'  reset, or perform any other operation from within a module definition that
#'  may alterate the internal state of modulr.
#'
#' @seealso \code{\link{define}}, \code{\link{list_modules}},
#'   and \code{\link{reset}}.
#'
#' @examples
#' reset()
#' define("foo", NULL, function() "foo")
#' list_modules()
#' undefine("foo")
#' list_modules()
#'
#' @export
undefine <- function(name = .Last.name) {

  .message_meta(sprintf("Entering undefine() for '%s' ...", name),
                verbosity = +Inf)

  if (.is_called_from_within_module()) {
    warning("undefine is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(.is_defined_regular(name))

  .message_meta(sprintf("Undefining '%s'", name), {

    modulr_env$register[[name]] <- NULL

  },
  ok = TRUE, verbosity = 2)

  invisible()

}

#' @export
`%requires%` <- function(name, dependencies) {

  assert_that(.is_regular(name))

  assert_that(
    is.list(dependencies) || is.null(dependencies),
    msg = "right-hand side of `%requires%` is not a list of dependencies."
  )

  list(name = name, dependencies = dependencies)

}

#' @export
`%provides%` <- function(lhs, factory) {

  if (.is_called_from_within_module()) {
    warning("`%provides%` is called from within a module.",
            call. = FALSE, immediate. = TRUE)
  }

  assert_that(
    is.function(factory),
    msg = "right-hand side of `%provides%` is not a factory."
    )

  assert_that(
    assertthat::is.string(lhs) || (
      is.list(lhs) &&
        setequal(names(lhs), c("name", "dependencies"))),
    msg = paste0("left-hand side of `%provides%` ",
                 "is not a module name or a list of dependencies.")
  )

  if (is.list(lhs)) {
    name <- lhs[["name"]]
    dependencies <- lhs[["dependencies"]]
  } else {
    name <- as.character(lhs)
    dependencies <- list()
  }

  do.call(
    define,
    args =
      list(name = name,
           dependencies = dependencies,
           factory = factory),
    envir = parent.frame())

}
