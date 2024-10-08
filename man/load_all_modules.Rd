% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_all_modules}
\alias{load_all_modules}
\title{Load All Modules.}
\usage{
load_all_modules(
  path = root_config$get_all()[[1L]],
  pattern = "[^_]\\\\.[Rr](?:md|nw)?$",
  full.names = TRUE,
  recursive = TRUE,
  strict = TRUE,
  ...
)
}
\arguments{
\item{path}{a character vector of full path names; the default
    corresponds to the working directory, \code{\link[base]{getwd}()}.  Tilde
    expansion (see \code{\link[base]{path.expand}}) is performed.  Missing
    values will be ignored.  Elements with a marked encoding will
    be converted to the native encoding.}

\item{pattern}{an optional \link[base]{regular expression}.  Only file names
    which match the regular expression will be returned.}

\item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names to give a relative file path.
    If \code{FALSE}, the file names (rather than paths) are returned.}

\item{recursive}{logical.  Should the listing recurse into directories?}

\item{strict}{A flag. Should stop at errors?}

\item{...}{Further arguments to be passed to \code{base::\link[base]{list.files}}.}
}
\description{
Load or reload all modules which are defined in the named directory.
}
\section{Warning}{

 It is considered a very bad practice to define, touch, undefine, load, make,
 reset, or perform any other operation from within a module definition that
 may alterate the internal state of modulr.
}

\examples{
reset()
tmp_dir <- tempfile("modulr_")
dir.create(tmp_dir)
tmp_file <- file.path(tmp_dir, "foo.R")
cat('define("foo", NULL, function() print("Hello World!"))', file = tmp_file)
tmp_file <- file.path(tmp_dir, "bar.R")
cat('define("bar", NULL, function() print("hELLO wORLD?"))', file = tmp_file)
load_all_modules(tmp_dir)
make_all()
unlink(tmp_dir, recursive = TRUE)

}
\seealso{
\code{\link{load_module}}, \code{\link{make_all}}, and
  \code{\link{reset}}.
}
