% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/future.R
\name{futurize}
\alias{futurize}
\title{Tweak a module to a future.}
\usage{
futurize(
  original_name,
  name = paste(original_name, "future", sep = "/"),
  dependencies = get_dependencies(original_name),
  strategy = NULL,
  lazy = FALSE,
  ...
)
}
\arguments{
\item{original_name}{A module name.}

\item{name}{A new module name.}

\item{dependencies}{A list of dependencies.}

\item{strategy}{The evaluation function (or name of it) to use for resolving
a future. If NULL, then the current strategy is returned.}

\item{lazy}{Is the strategy lazy?}

\item{...}{Further arguments passed to \code{\link{get_provider}}.}
}
\description{
Tweak a module to a future.
}
\section{Warning}{

This is an experimental feature subject to changes.
}

