% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define.R
\name{define}
\alias{define}
\alias{\%requires\%}
\alias{\%provides\%}
\title{Define a Module.}
\usage{
define(name, dependencies = NULL, provider = function() NULL)
}
\arguments{
\item{name}{A string (character vector of lenght one).

  A module name can contain letters, figures and some special characters,
  namely \code{_}, \code{-}, and \code{/}. The latter is a namespace
  separator.

  Names containing \code{/mock/}, \code{/mocks/}, \code{/test/},
  \code{/tests/}, \code{/example/}, or \code{/examples/} have a special
  meaning related to code testing and examples.

  The name "modulr" corresponds to a special module and is therefore
  reserved.}

\item{dependencies}{A (preferably named) list of strings.

  Elements of the list of dependencies designate modules by their name.}

\item{provider}{A string, a braced expression, or a function.

  If any, function's formals must coincide with the list of dependencies.}
}
\value{
A wrapper function around a make call for the defined module.
}
\description{
Define or redefine a module by name, dependencies, and provider.
}
\details{
The definition of a module can be done explicitly in the console, implicitly
from a file on a disk, or remotely at a given URL via the HTTP(S) protocol.
These three ways of defining modules have their specificities.

\describe{ \item{Explicit Definition}{ This is the most direct method to
define or redefine a module. This is also the most volatile since the
lifespan of the module is limited to the R session. When a new module is
defined, the internal state of the package is modified to record its name,
dependencies and provider. Some other useful metadata are also recorded, like
timestamps, various flags and counters, and a digest. When an existing module
is redefined, the internal state is updated accordingly, unless no change is
detected by digests comparison. No other side-effect occurs during the
definition process, notably the evaluation of the provider which is postponed
to a subsequent \code{\link{make}} call. }

\item{Implicit Definition}{ This is the natural method to choose when a
module is intended to be reused. In such a case, the definition takes place
in a dedicated file, which name is closely related to the module's name.

As a file \code{/home/user/readme.txt} is composed of a path
\code{/home/user} and a file name \code{readme.txt}, a module name
\code{vendor/tool/swissknife} is similarily composed of a namespace
\code{vendor/tool} and a local name \code{swissknife}. For modulr to find
this module, it is sufficient to store its definition in an R, R Markdown or
R Sweave file named \code{swissknife.R[md|nw]} (R files have precedence over
Rmd's and Rnw's), laid out on disk in the \code{vendor/tool} path, relative
to the modulr root directory (see \code{\link{root_config}}).

\itemize{ \item \code{vendor/} \itemize{ \item \code{tool/} \itemize{ \item
\code{swissknife.R},

contains the "vendor/tool/swissknife" definition.}}}

Each time the definition of a module is needed, modulr resolves its name into
a file location by applying the following configurable rules.

\enumerate{ \item The \code{\link{root_config}} accessor acts at the
\emph{filesystem level}, by specifying the root directory, relative to which
all paths are expressed. For instance, \code{root_config$set("./lib")} tells
modulr that all modules are to be found in \code{lib} (in the R working
directory). The directory path can be relative (e.g. \code{./lib}) or
absolute (e.g. \code{/home/user/lib}). By default, modulr looks in turn into
the following directories \code{"./module"}, \code{"./modules"},
\code{"./lib"}, \code{"./libs"}, and \code{"."}.

\item The \code{\link{paths_config}} accessor acts at the \emph{namespace
level}, by mapping a specific namespace to a dedicated path, relative to the
root directory. For instance, \code{paths_config$set("vendor" =
"third_parties/vendor")} will map the \code{vendor/great_module} to the
\code{third_parties/vendor/great_module.R} path, relative to the root
directory. \itemize{ \item \code{third_parties}

is intended to be a dedicated container for third-parties modules. \itemize{
\item \code{vendor} \itemize{ \item \code{great_module.R}

contains the "vendor/great_module" definition. } } } \item The
\code{\link{maps_config}} accessor acts at the \emph{module level}, by
substituting specific dependencies within the scope of a given module. This
is especially useful in a situation where a dependency has been replaced by a
newer version, but a module still needs to rely on the previous one. For
instance, \code{maps_config$set("foo/bar" = list("vendor/great_module" =
"vendor/old_great_module"))} tells modulr that for the module \code{foo/bar}
only, the dependency \code{vendor/great_module} must be replaced by
\code{vendor/old_great_module}. }

\itemize{ \item \code{foo} \itemize{ \item \code{bar.R}

depends on \code{vendor/great_module} by definition, but will be replaced by
\code{vendor/old_great_module} when needed. } \item \code{vendor} \itemize{
\item \code{great_module.R}

serves all modules that depend on it, except \code{foo/bar}. \item
\code{old_great_module.R}

serves \code{foo/bar} only. } }

These rules are applied in reverse order, substituting dependencies first,
then mapping namespaces and finally expressing the absolute path, relative to
the modulr root directory.

It is possible to store several definitions into one main file. By doing so,
the implicit definition of the main module (i.e. the module for which the
name is resolved into the file location) triggers the simultaneous definition
of a bunch of related modules. It is notably desirable for test purposes,
when a module and its dependencies have to be mocked and injected into a new,
testing module.

} \item{Remote Definition}{ This is the method used to share a module via the
HTTP(S) protocol. The module is thus served at a given URL and has to be
imported (see \code{\link{import_module}}) in order to be defined and used.
Like files, it is possible to store several related definitions at one URL.
Public and private gists, files on GitHub, and any HTTP server can be used to
share so called \emph{modulr gears}. } }
}
\section{Syntactic Sugars}{
 \preformatted{name \%provides\% provider}
  \preformatted{name \%requires\% dependencies \%provides\% provider}
}

\section{Warning}{
 It is considered a very bad practice to define, touch,
  undefine, load, make, reset, or perform any other operation from within a
  module definition that may alterate the internal state of modulr.
}

\examples{
reset()
define("foo", NULL, "Hello")
bar <- define("bar", list(foo = "foo"), { paste(foo, "World!") })
bar()
define("foo", NULL, "Again, Hello")
bar()

reset()
"foo" \%provides\% "Hello"
"bar" \%requires\% list(
  foo = "foo"
) \%provides\% {
  paste(foo, "World!")
}
make()
"foo" \%provides\% "Again, Hello"
make("bar")

reset()
define("A", list(b = "B"), function(b) NULL)
define("B", list(a = "A"), function(a) NULL)
\dontrun{make()}

reset()
define("A", NULL, function() NULL)
define("B", NULL, function() NULL)
define("C", list(a = "A"), function(a) NULL)
define("D", list(a = "A", b = "B"), function(a, b) NULL)
define("E", list(d = "D"), function(d) NULL)
define("F", list(c = "C", d = "D", e = "E"), function(c, d, e) NULL)
plot_dependencies()
make()

}
\seealso{
\code{\link{.Last.name}}, \code{\link{plot_dependencies}},
  \code{\link{import_module}}, \code{\link{make}}, \code{\link{maps_config}},
  \code{\link{paths_config}}, \code{\link{reset}}, \code{\link{touch}}, and
  \code{\link{undefine}}.
}
