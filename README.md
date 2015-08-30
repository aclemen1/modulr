<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/aclemen1/modulr.svg)](https://travis-ci.org/aclemen1/modulr)

modulr — A Dependency Injection (DI) Framework for R
====================================================

*Until the release of the forthcoming stable version (0.2.0), the documentation of the package is a work in progress. Thank you for your understanding.*

Description
-----------

The `modulr` package is a Dependency Injection (DI) Framework for R. By design, `modulr` allows to break down sequential programs into discrete, modular units that are loosely coupled, simple to develop, test, reuse and share in a wide range of situations. As every DI framework, it aims for a clear separation between code complication and complexity, highlighting the core purpose and behaviour of objects (application code), and hiding their construction and wiring (infrastructure code).

Advantages
----------

-   modules are easy (and fun) to develop,
-   modules are easy to debug,
-   modules are easy to test,
-   modules are easy to read,
-   modules are easy to reuse,
-   modules are easy to share,
-   modules are easy to maintain, and
-   modules force (a bit) to keep up with good practices.

`modulr` is deeply inspired from [AngularJS](https://angularjs.org/) and [RequireJS](http://requirejs.org) for Javascript, as well as [guice](https://github.com/google/guice) for Java.

Installation
------------

<!---
You can install:

* the latest released version from CRAN with

``` r
install.packages("modulr")
```
* the latest released version from Github with
``` r
if (packageVersion("devtools") < 1.8) {
  install.packages("devtools")
}
devtools::install_github("aclemen1/modulr")
```

* the latest _bleeding edge_ development version from Github with
-->
``` r
if (packageVersion("devtools") < 1.8) {
  install.packages("devtools")
}
devtools::install_github("aclemen1/modulr@devel")
```

If you encounter a clear bug, please [file a minimal reproducible example](https://github.com/aclemen1/modulr/issues).

A short example
---------------

To get started with `modulr`, let us consider the following situation. Suppose that a university needs to compute its student-teacher ratio. This requires to gather at least a dataset about students and a dataset about teachers. Due to the organization of the university, suppose furthermore that these datasets are accessible, kept and/or maintained by different people. Alice, say, knows everything about students, when teachers have no secret for Bob. To start with our calculation of a student-teacher ratio, let's ask Alice to provide us with a usable dataset.

``` r
library(modulr)

# This module provides a dataset relating students and their inscriptions to courses.
# Alice is the maintainer of this module.
"data/students" %provides%
  function() {
    students <- data.frame(
      id = c(1, 2, 2, 3, 3, 3),
      course = c("maths", "maths", "physics", "maths", "physics", "chemistry"),
      stringsAsFactors = F)
    return(students)
  }
#> [2015-08-30 00:30:20.233390] defining [data/students] ...
```

The anatomy of this module is very simple: "data/student" is its name and the body of the function following the `%provides%` operator (which is part of a *syntactic sugar* for the more verbose `define` function) contains its core functionality, namely returning the required data frame.

It is important to note that no intrinsic computation takes place in this **definition** process. The DI framework simply **registers** the module, thus relaying the actual evaluation of its body to another **making** stage, as we'll see below.

In parallel, let's ask Bob to provide us with a similar module.

``` r
# This module provides a dataset relating teachers and their courses.
# Bob is the maintainer of this module.
"data/teachers" %provides%
  function() {
    teachers <- data.frame(
      id = c(1, 2, 3),
      course = c("maths", "physics", "chemistry"),
      stringsAsFactors = F)
    return(teachers)
  }
#> [2015-08-30 00:30:20.239814] defining [data/teachers] ...
```

Now that we have these two modules at our disposal, let's combine them into another module that returns a (bad) student-teacher ratio.

``` r
"bad_stat/student_teacher_ratio" %requires%
  list(
    students = "data/students",
    teachers = "data/teachers"
  ) %provides%
  function(students, teachers) {
    ratio <- length(unique(students$id)) / length(unique(teachers$id))
    return(ratio)
  }
#> [2015-08-30 00:30:20.245210] defining [bad_stat/student_teacher_ratio] ...
```

The `%requires%` operator allows us to specify the modules we rely on for the calculation we provide. This list of **dependencies** assigns some arbitrary and ephemeral names to the required modules. These are those names that are then used to call objects into which the results of the required modules are **injected**, and available for use in the body of the module's definition.

It is now time to see the DI framework in action.

``` r
bad_ratio %<=% "bad_stat/student_teacher_ratio"
#> [2015-08-30 00:30:20.250350] making [bad_stat/student_teacher_ratio] ...
#> [2015-08-30 00:30:20.250820] * checking definitions ...
#> [2015-08-30 00:30:20.263113] * found 2 dependencies(s) with 3 modules(s) on 2 layer(s)
#> [2015-08-30 00:30:20.264255] ** making [data/students] ...
#> [2015-08-30 00:30:20.265665] ** making [data/teachers] ...
#> [2015-08-30 00:30:20.267332] ** making [bad_stat/student_teacher_ratio] ...
```

We say that the `%<=%` operator **makes** the module given on its right-hand side. Obviously, there are three modules involved in this process, namely `[data/student]` and `[data/teachers]` which are independent on a first *layer*, and `[bad_stat/student_teacher_ratio]` which depends on them on a second layer. Under the hood, the framework figures out the directed acyclic graph (DAG) of the dependencies and computes a topological sort, grouped by independent modules into layers.

``` r
dependency_graph("bad_stat/student_teacher_ratio")
```

![](README-fig1.png)

All the modules are then evaluated in order and the final result is assigned to the variable on the left-hand side of the `%<=%` operator.

``` r
print(bad_ratio)
#> [1] 1
```

A Real-World Usage
------------------

This is the dependency graph of a module which exposes daily HR data for the University of Lausanne, Switzerland. All modules have been developed by four data scientists in three months. Although not shown here, many of these modules are reused for other purposes.

![](README-fig3.png)

Code of Conduct
---------------

This project adheres to the [Open Code of Conduct](http://todogroup.org/opencodeofconduct/#modulr/alain.clement-pavon@unil.ch). By participating, you are expected to honor this code.
