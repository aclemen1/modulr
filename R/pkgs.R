.is_base_pkg <- function(pkgs) {
  vapply(
    pkgs,
    function(pkg) {
      desc <- utils::packageDescription(pkg)
      !is.null(desc[["Priority"]]) && desc[["Priority"]] == "base"
    },
    FUN.VALUE = T)
}

.empty_manifest_pkgs <- data.frame(
  pkg = character(0L),
  loc = character(0L),
  base = logical(0L),
  attached = logical(0L),
  stringsAsFactors = FALSE)

.get_pkgs_manifest <- function(ignore, ignore_base = TRUE) {

  search_locs <- stats::setNames(dirname(searchpaths()), search())
  pkgs_locs <- search_locs[grepl("^package:", names(search_locs))]
  names(pkgs_locs) <- sub("^package:", "", names(pkgs_locs))
  base_pkgs_locs <- pkgs_locs[.is_base_pkg(names(pkgs_locs))]
  other_pkgs_locs <- pkgs_locs[!.is_base_pkg(names(pkgs_locs))]

  loaded_ns <- setdiff(loadedNamespaces(), names(pkgs_locs))
  loaded_ns_locs <-
    vapply(
      loaded_ns,
      FUN = function(x) dirname(getNamespaceInfo(x, which = "path")),
      FUN.VALUE = "string")
  other_ns_locs <- loaded_ns_locs[!.is_base_pkg(names(loaded_ns_locs))]
  base_ns_locs <- loaded_ns_locs[.is_base_pkg(names(loaded_ns_locs))]

  pkgs <- rbind(
    if (length(base_pkgs_locs) == 0) .empty_manifest_pkgs else data.frame(
      pkg = names(base_pkgs_locs),
      loc = unname(base_pkgs_locs),
      base = TRUE,
      attached = TRUE,
      stringsAsFactors = FALSE
    ),
    if (length(other_pkgs_locs) == 0) .empty_manifest_pkgs else data.frame(
      pkg = names(other_pkgs_locs),
      loc = unname(other_pkgs_locs),
      base = FALSE,
      attached = TRUE,
      stringsAsFactors = FALSE
    ),
    if (length(base_ns_locs) == 0) .empty_manifest_pkgs else data.frame(
      pkg = names(base_ns_locs),
      loc = unname(base_ns_locs),
      base = TRUE,
      attached = FALSE,
      stringsAsFactors = FALSE
    ),
    if (length(other_ns_locs) == 0) .empty_manifest_pkgs else data.frame(
      pkg = names(other_ns_locs),
      loc = unname(other_ns_locs),
      base = FALSE,
      attached = FALSE,
      stringsAsFactors = FALSE
    )
  )

  is_ignored <- (ignore_base & pkgs[["base"]]) | pkgs[["pkg"]] %in% ignore

  list(
    pkgs = pkgs[!is_ignored, ],
    ignored = pkgs[is_ignored, ],
    domain = names(search_locs)
  )

}

.compute_ops <- function(to, from) {

  from[["ids"]] <- paste(from[["pkg"]], from[["loc"]], sep = ";")
  to[["ids"]] <- paste(to[["pkg"]], to[["loc"]], sep = ";")

  pkg_ids <- unique(c(from[["ids"]], to[["ids"]]))
  ops <- c()
  for (id in pkg_ids) {
    from_info <- from[id == from[["ids"]], ]
    to_info <- to[id == to[["ids"]], ]
    if (nrow(from_info) > 0 & nrow(to_info) > 0) {
      if (from_info[["attached"]] & to_info[["attached"]]) {
        op <- "sort"
      } else if (from_info[["attached"]] & !to_info[["attached"]]) {
        op <- "detach"
      } else if (!from_info[["attached"]] & to_info[["attached"]]) {
        op <- "attach"
      } else {
        op <- "noop"
      }
    } else if (nrow(from_info) > 0 & nrow(to_info) == 0) {
      if (from_info[["attached"]]) {
        op <- "forget"
      } else {
        op <- "unload"
      }
    } else if (nrow(from_info) == 0 & nrow(to_info) > 0) {
      if (to_info[["attached"]]) {
        op <- "require"
      } else {
        op <- "load"
      }
    }
    ops <- c(ops, op)
  }

  data.frame(
    stats::setNames(rbind(
      data.frame(pkg = character(0L), loc = character(0L),
                 stringsAsFactors = FALSE),
      as.data.frame(
        do.call(rbind, strsplit(pkg_ids, ";", fixed = TRUE)),
        stringsAsFactors = FALSE)),
      c("pkg", "loc")),
    op = ops,
    stringsAsFactors = FALSE
  )

}

.catch_error_wrapper <- function(f, ok_cb = function(...) message("OK"),
                                 err_cb = function(...) message("FAILED")) {
  function(...) {
    tryCatch({
      result <- suppressWarnings(suppressMessages(f(...)))
      ok_cb(result)
    },
    error = function(e) {
      err_cb(e)
    })
  }
}

.load <- function(pkg, loc) {
  message("Loading ", pkg, " ... ", appendLF = FALSE)
  .catch_error_wrapper(loadNamespace)(pkg, lib.loc = loc)
}

.detach <- function(pkg) {
  message("Detaching ", sQuote(pkg), " ... ", appendLF = FALSE)
  .catch_error_wrapper(detach)(
    paste("package", pkg, sep = ":"), character.only = TRUE,
    unload = FALSE, force = TRUE)
}

.unload <- function(pkg, loc) {
  message("Unloading ", sQuote(pkg), " ... ", appendLF = FALSE)
  .catch_error_wrapper(devtools::unload)(file.path(loc, pkg))
}

.forget <- function(pkg, loc) {
  message("Forgetting ", sQuote(pkg), " ... ", appendLF = FALSE)
  .catch_error_wrapper(
    function(pkg, loc) {
      .detach(pkg)
      .unload(pkg, loc)
    }
  )(pkg, loc)
}

.sort <- function(ops, domain) {

  if (nrow(ops) > 0) {
    search_ <- search()

    pkgs <- paste("package", ops[["pkg"]], sep = ":")

    assert_that(
      all(pkgs %in% intersect(domain, search_)),
      msg = "Unattached package(s).")

    idxs <- match(intersect(domain, pkgs), intersect(search_, pkgs))
    # we split the indexes in increasing sequences and keep the longest one
    intervals <- split(idxs, c(0L, cumsum(sign(diff(idxs)) == -1L)))
    lengths <-
      unlist(lapply(intervals, length), use.names = FALSE, recursive = FALSE)
    to_detach <-
      unlist(intervals[-which.max(lengths)],
             use.names = FALSE, recursive = FALSE)

    for (idx in to_detach) {
      .detach(ops[["pkg"]][idx])
    }

    return(ops[to_detach, ])

  }

  ops

}

.attach_require <- function(pkg, loc, domain) {
  pkg_ <- paste("package", pkg, sep = ":")
  pos <- match(pkg_, intersect(domain, unique(c(pkg_, search()))))
  message("Attaching ", sQuote(pkg), " at position ", pos, " ... ",
          appendLF = FALSE)
  .catch_error_wrapper(library)(pkg, lib.loc = loc, pos = pos,
                                character.only = TRUE)
}

.do_ops <- function(ops, domain) {

  to_forget <- ops[ops[["op"]] == "forget", c("pkg", "loc"), drop = FALSE]
  by(to_forget, seq_len(nrow(to_forget)), function(row) {
    .forget(row[["pkg"]], row[["loc"]])
  })

  to_unload <- ops[ops[["op"]] == "unload", c("pkg", "loc"), drop = FALSE]
  by(to_unload, seq_len(nrow(to_unload)), function(row) {
    .unload(row[["pkg"]], row[["loc"]])
  })

  to_detach <- ops[ops[["op"]] == "detach", c("pkg"), drop = FALSE]
  by(to_detach, seq_len(nrow(to_detach)), function(row) {
    .detach(row[["pkg"]])
  })

  to_load <- ops[ops[["op"]] == "load", c("pkg", "loc"), drop = FALSE]
  by(to_load[rev(rownames(to_load)), ], seq_len(nrow(to_load)), function(row) {
    .load(row[["pkg"]], row[["loc"]])
  })

  to_sort <- ops[ops[["op"]] == "sort", c("pkg", "loc"), drop = FALSE]
  to_reattach <- .sort(to_sort, domain)

  to_attach_require <- rbind(
    to_reattach,
    ops[ops[["op"]] %in% c("attach", "require"),
        c("pkg", "loc"), drop = FALSE])

  by(to_attach_require, seq_len(nrow(to_attach_require)), function(row) {
    .attach_require(row[["pkg"]], row[["loc"]], domain)
  })

  invisible(NULL)

}

.minimal_pkgs_manifest <- function(ignore) {
  manifest <- .get_pkgs_manifest(ignore = ignore, ignore_base = TRUE)
  manifest[["pkgs"]] <- .empty_manifest_pkgs
  manifest
}

.set_pkgs <- function(from, to) {

  ops <- .compute_ops(to = to[["pkgs"]], from = from[["pkgs"]])
  .do_ops(ops, domain = to[["domain"]])

  invisible(from)

}

#' With (...) Packages.
#'
#' Temporarily unload and detach packages, and optionally use a specific
#' packages library for the module.
#'
#' @param code Any object. Code to execute in the temporary environment.
#' @param ignore A vector of character strings. Packages (in addition to base
#'   packages) to ignore in the process.
#'
#' @return The result of the evaluation of the \code{code} argument.
#'
#' @details
#'
#' Packages are temporarily unloaded and detached from the search path. When
#' restored, the ordering of the packages on the search path is preserved. Base
#' packages and packages passed to the \code{ignore} parameter are ignored in
#' the process. By default, the vector of names of ignored packages is taken
#' from the \code{modulr.ignore_packages} R option.
#'
#' \code{with_packages} uses the library given by \code{lib_path}.
#'
#' \code{with_module_packages} uses a dedicated library for the module, using
#' the module name as path.
#'
#' \code{with_namespace_packages} uses a shared library for all the modules
#' under the given \code{namespace} parameter.
#'
#' @section Warning: This is an experimental feature subject to changes.
#'
#' @seealso \code{\link[withr]{withr}} for examples of 'with_' methods,
#'   \code{\link{getOption}}, \code{\link{library}}, \code{\link{options}}, and
#'   \code{\link{search}}.
#'
#' @examples
#' print(sessionInfo())
#' \dontrun{
#' with_no_packages({
#'   message("Look mum, no packages!")
#'   print(sessionInfo())
#' })}
#' print(sessionInfo())
#'
#' print(sessionInfo())
#' \dontrun{
#' with_packages("~/my_packages", {
#'  if (!"pooh" %in% rownames(installed.packages()))
#'    utils::install.packages("pooh")
#'  library(pooh)
#'  print(sessionInfo())
#' })}
#' print(sessionInfo())
#'
#' ## in file "foos/foobar.R"
#' "foos/foobar" %provides% {
#'  print(sessionInfo())
#'  with_module_packages({
#'    if (!"devtools" %in% rownames(installed.packages()))
#'      utils::install.packages("devtools")
#'    library(devtools)
#'    if (!"pooh" %in% rownames(installed.packages()))
#'      devtools::install_version("pooh", "0.2")
#'    library(pooh)
#'    print(sessionInfo())
#'  })
#' print(sessionInfo())
#' }
#' ## EOF
#' \dontrun{make()}
#'
#' ## in file "foos/foobaz.R"
#' "foos/foobaz" %provides% {
#'  print(sessionInfo())
#'  with_namespace_packages("foos", {
#'    if (!"devtools" %in% rownames(installed.packages()))
#'      utils::install.packages("devtools")
#'    library(devtools)
#'    if (!"pooh" %in% rownames(installed.packages()))
#'      devtools::install_version("pooh", "0.3")
#'    library(pooh)
#'    print(sessionInfo())
#'  })
#' print(sessionInfo())
#' }
#' ## EOF
#' \dontrun{make()}
#'
#' @aliases without_packages
#' @export
with_no_packages <- function(code,
                             ignore = getOption("modulr.ignore_packages")) {

  # nocov start
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # nocov end

  old_pkgs <- .set_pkgs(
    from = .get_pkgs_manifest(ignore),
    to = .minimal_pkgs_manifest(ignore))
  on.exit(.set_pkgs(from = .get_pkgs_manifest(ignore), to = old_pkgs))
  force(code)
}

#' @rdname with_no_packages
#' @export
without_packages <- with_no_packages

.with_libpaths <- function (new, code)
{
    old <- .set_libpaths(paths = new)
    on.exit(.libPaths(old))
    force(code)
}

.set_libpaths <- function (paths)
{
  old <- .libPaths()
  if (!is.null(paths)) {
    paths <- normalizePath(paths, mustWork = TRUE)
    .libPaths(paths)
  }
  invisible(old)
}

# Thanks to Henrik Bengtsson's R.utils::findSourceTraceback() method.
.source_trace <- function () {

  srcfile_list <- list()

  args_to_find <- names(formals(source))
  for (frame in sys.nframe():0) {
    env <- sys.frame(frame)
    exist <- vapply(args_to_find, exists, envir = env, inherits = FALSE,
                    FUN.VALUE = TRUE)
    if (!all(exist)) next
    srcfile <- get("srcfile", envir = env, inherits = FALSE)
    if (!is.null(srcfile)) {
      if (!is.function(srcfile)) {
        srcfile_list <- c(srcfile_list, list(srcfile))
      }
    }
  }

  paths <- vapply(srcfile_list, FUN = function(srcfile) {
    if (inherits(srcfile, "srcfile")) {
      pathname <- srcfile$filename
    }
    else if (is.environment(srcfile)) {
      pathname <- srcfile$filename
    }
    else if (is.character(srcfile)) {
      pathname <- srcfile
    }
    else {
      pathname <- NA_character_
      warning("Unknown class of 'srcfile': ", class(srcfile)[1L])
    }
    pathname
  }, FUN.VALUE = "character")

  names(srcfile_list) <- paths

  srcfile_list

}

#' @rdname with_no_packages
#' @param lib_path A string (character vector of length one) containing the path
#'   of the packages library.
#' @param ... Further arguments to be passed to \code{\link{with_no_packages}}.
#' @export
with_packages <- function(lib_path, code, ...) {

  assert_that(assertthat::is.string(lib_path))

  if (!.dir_exists(lib_path)) {
    dir.create(lib_path, recursive = TRUE)
    cat(character(0), file = file.path(lib_path, "__IGNORE__"))
  }
  with_no_packages(code = .with_libpaths(new = lib_path, code = code), ...)
}

#' @rdname with_no_packages
#' @export
with_module_packages <- function(code, ...) {
  lib_path <- NULL
  name <- .get_0(".__name__", envir = parent.frame())
  file <-
    if (!is.null(.modulr_env$injector$registry[[c(name, "filepath")]])) {
      normalizePath(.modulr_env$injector$registry[[c(name, "filepath")]])
    } else {
      trace <- stats::na.omit(names(.source_trace()))
      if (length(trace) > 0L)
        normalizePath(utils::tail(trace, 1L))
    }
  if (!is.null(file)) {
    parsed_name <- .parse_name(name)
    file_name <- parsed_name[["final"]]
    if (!is.na(parsed_name[["version"]]))
      file_name <- paste(file_name, as.character(parsed_name[["version"]]),
                         sep = "#")
    lib_path <-
      file.path(dirname(file), file_name, "lib")
    with_packages(new = lib_path, code = code, ...)
  } else {
    stop(paste("Module packages are not available for in-memory modules",
               "which are not sourced from a file."))
  }
}

# TODO test that
.namespace_to_lib_path <- function(name, path, namespace) {
  name_initials <- .parse_name(name)[["initials"]]
  path_initials <- .parse_filepath(path)[["path"]]
  root <- sub(sprintf("(?:%s%s)", .Platform$file.sep, name_initials), "",
              path_initials)
  namespace_parsed <- .parse_name(namespace)
  if (is.na(namespace_parsed[["version"]])) {
    file.path(root, namespace_parsed[["namespace"]], "lib")
  } else {
    file.path(root, namespace_parsed[["namespace"]],
              paste("lib", as.character(namespace_parsed[["version"]]),
                    sep = "#"))
  }
}

#' @rdname with_no_packages
#' @param namespace A string (character vector of length one) containing the
#'   namespace of the module to be used as path for the packages library.
#' @export
with_namespace_packages <- function(namespace, code, ...) {
  lib_path <- NULL
  name <- .get_0(".__name__", envir = parent.frame())
  path <-
    if (!is.null(.modulr_env$injector$registry[[c(name, "filepath")]])) {
      normalizePath(.modulr_env$injector$registry[[c(name, "filepath")]])
    } else {
      trace <- stats::na.omit(names(.source_trace()))
      if (length(trace) > 0L)
        normalizePath(utils::tail(trace, 1L))
    }
  if (!is.null(file)) {
    lib_path <- .namespace_to_lib_path(name, path, namespace)
    with_packages(new = lib_path, code = code, ...)
  } else {
    stop(paste("Namespace packages are not available for in-memory modules",
               "which are not sourced from a file."))
  }
}

DEFAULT_IGNORE_PACKAGES <- c("modulr", "devtools", "shiny", "knitr")
