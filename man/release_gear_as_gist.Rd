% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gears.R
\name{release_gear_as_gist}
\alias{release_gear_as_gist}
\title{Prepare and Release a Modulr Gear as a Gist on GitHub.}
\usage{
release_gear_as_gist(
  name = .Last.name,
  load = TRUE,
  update = TRUE,
  update_first_found = TRUE,
  per_page = 100L,
  max_pages = 10L,
  endpoint = "https://api.github.com",
  browse = TRUE
)
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

\item{load}{A flag. Should an undefined module be implicitely loaded?}

\item{update}{A flag or a string (character vector of length one). Should an
existing gist be updated (flag)? Or gist ID to update (string).}

\item{update_first_found}{A flag. Should the first corresponding module
name be updated?}

\item{per_page}{A scalar (integer vector of length one). Number of gists
per page requested.}

\item{max_pages}{A scalar (integer vector of length one). Upper bound for
the number of pages requested.}

\item{endpoint}{A string (character vector of length one). GitHub API
Endpoint URL.}

\item{browse}{A flag. Should the created or updated gist be opened in the
default browser?}
}
\description{
Prepare and release a modulr gear as a gist on GitHub.
}
\examples{
\dontrun{
library(gistr)
reset()
define("foo", NULL, function() NULL)
Sys.setenv("GITHUB_PAT" = "your Personal Access Token here")
gist_auth(reauth = TRUE)
release_gear_as_gist("foo")}

}
\seealso{
\code{\link{define}}, \code{\link{prepare_gear}}, and
  \code{\link{reset}}.
}
