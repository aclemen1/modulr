[![Build Status](https://travis-ci.org/aclemen1/modulr.svg?branch=master)](https://travis-ci.org/aclemen1/modulr)
[![Coverage status](https://codecov.io/github/aclemen1/modulr/coverage.svg?branch=master)](http://codecov.io/github/aclemen1/modulr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/modulr)](https://cran.r-project.org/package=modulr)
[![CRAN](http://cranlogs.r-pkg.org/badges/grand-total/modulr)](https://cran.r-project.org/package=modulr)
[![License](https://img.shields.io/:license-mit-blue.svg)](http://aclemen1.mit-license.org/license.html)
[![Twitter](https://img.shields.io/twitter/url/http/github.com/aclemen1/modulr.svg?style=social)](https://twitter.com/intent/tweet.html?text=Modulr:%20A%20Dependency%20Injection%20Framework%20for%20R&url=http://github.com/aclemen1/modulr&hashtags=r,dependency-injection)

# Modulr: A Dependency Injection Framework for R

Modulr is a Dependency Injection (DI) framework for R which allows to break down sequential programs into discrete, modular units that are loosely coupled, simple to develop, test, debug, maintain, reuse, and share. 

* Modulr is designed to ease production of mocks for testing purpose and is therefore well-suited for [test-driven development](https://en.wikipedia.org/wiki/Test-driven_development).
* Modulr offers [docstrings](https://en.wikipedia.org/wiki/Docstring)-like comments for documenting the internals of a module.
* Modulr understands [R Markdown](http://rmarkdown.rstudio.com/) and [Sweave](https://leisch.userweb.mwn.de/Sweave/), thanks to Yihui Xie's [knitr](http://yihui.name/knitr/) package.
* Modulr takes advantage of [semantic versioning](http://semver.org/) to avoid _version lock_ and _version promiscuity_.
* Modulr can use [GitHub](http://github.com) (repositories and gists) to publish and share modules (so-called _gears_). See [`modulr/vault`](https://gist.github.com/aclemen1/3fcc508cb40ddac6c1e3) for an example.
* Modulr helps to circumvent errors occurring in a chain of nested dependent modules, showing [breadcrumbs](https://en.wikipedia.org/wiki/Breadcrumb_(navigation)).
* Modulr allows parallelization using [futures](https://en.wikipedia.org/wiki/Futures_and_promises), thanks to Henrik Bengtsson's [future](https://github.com/HenrikBengtsson/future) package.
* Modulr implements __package isolation__ as an experimental feature: following the philosophy of Jim Hester's [withr](https://github.com/jimhester/withr) package, the 'with_' method `with_no_packages()` can be used to run code with temporarily no loaded or attached R packages.

Please read the [documentation](https://aclemen1.github.io/modulr) and vignettes (`browseVignettes(package = "modulr")`) to see extended examples in action.

## "Hello, world!"





```r
library(modulr)

"foo" %provides% "Hello"
#> [2018-12-02T16:13:51 UTC] Defining 'foo' ... OK

"bar" %provides% "World"
#> [2018-12-02T16:13:51 UTC] Defining 'bar' ... OK

"foobar" %requires% list(
  f = "foo", 
  b = "bar"
) %provides% {
  paste0(f, ", ", tolower(b), "!")
}
#> [2018-12-02T16:13:51 UTC] Defining 'foobar' ... OK

make("foobar")
#> [2018-12-02T16:13:51 UTC] Making 'foobar' ...
#> [2018-12-02T16:13:51 UTC] * Visiting and defining dependencies ...
#> [2018-12-02T16:13:52 UTC] * Constructing dependency graph ... OK
#> [2018-12-02T16:13:52 UTC] * Sorting 2 dependencies with 2 relations ... on 1 layer, OK
#> [2018-12-02T16:13:52 UTC] * Evaluating new and outdated dependencies ...
#> [2018-12-02T16:13:52 UTC] ** Evaluating #1/2 (layer #1/1): 'bar' ...
#> [2018-12-02T16:13:52 UTC] ** Evaluating #2/2 (layer #1/1): 'foo' ...
#> [2018-12-02T16:13:52 UTC] DONE ('foobar' in 0.066 secs)
#> [1] "Hello, world!"
```

## Further readings

* [Why a DI framework for R?](https://aclemen1.github.io/modulr/articles/motivation.html)
* [Introduction to modulr](https://aclemen1.github.io/modulr/articles/modulr.html)
* [Unit testing](https://aclemen1.github.io/modulr/articles/testing.html)
* [Using modulr in packages](https://aclemen1.github.io/modulr/articles/packages.html)
* [Experimental features](https://aclemen1.github.io/modulr/articles/experimental.html)

## Installation

``` r
install.packages("devtools")
devtools::install_github("hadley/devtools")
devtools::install_github("aclemen1/modulr")
```

If you encounter a clear bug, please [file a minimal reproducible example](https://github.com/aclemen1/modulr/issues).
For questions and other discussion, please use the [modulr mailing list](https://groups.google.com/forum/#!forum/modulr-talk).

## Real-world applications

![](https://github.com/aclemen1/modulr/raw/master/README-fig1.png)
   
This is the dependency graph of a module (on the rightmost side) which exposes tidy, exhaustive, statistics-ready, and daily HR data for the [University of Lausanne](http://www.unil.ch), Switzerland. Although not visible here, many of these modules are reused in other projects. The University of Lausanne has been running modulr in [mission critical applications](http://www.unil.ch/statistiques) since March 2015.

## Related approaches

_In alphabetical order._

* Zebulun Arendsee's [rmonad](https://github.com/arendsee/rmonad) package.
* Rich FitzJohn's [remake](http://github.com/richfitz/remake) package.
* Robert Krzyzanowski's [director](https://github.com/syberia/director) package.
* Lev Kuznetsov's [injectoR](http://dfci-cccb.github.io/injectoR) package.
* Will Landau's [drake](https://github.com/wlandau-lilly/drake) package.
* Konrad Rudolph's [modules](http://github.com/klmr/modules) package.
* Sebastian Warnholz's [modules](https://github.com/wahani/modules) package.

## Code of conduct

In order to have a more open and welcoming community, modulr adheres to a [Contributor Code of Conduct](CONDUCT.md). Please adhere to this code of conduct in any interactions you have in the modulr community. It is strictly enforced on all official modulr repositories, websites, and resources. If you encounter someone violating these terms, please let the maintainer know and he will address it as soon as possible.
