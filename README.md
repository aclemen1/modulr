# modulr — A Dependency Injection (DI) Framework for R

## Raison d'être

The `modulr` package is a Dependency Injection (DI) Framework for R. By design, `modulr` allows to break down sequential programs into discrete, modular units that are loosely coupled, simple to develop, test, reuse and share in a wide range of situations. As every DI framework, it aims for a clear separation between code complication and complexity, highlighting the core purpose and behaviour of objects (application code), and hiding their construction and wiring (infrastructure code). 

## Pros

  * modules are easy (and fun) to develop,
  * modules are easy to debug,
  * modules are easy to test,
  * modules are easy to read,
  * modules are easy to reuse,
  * modules are easy to share,
  * modules are easy to maintain, and
  * modules force (a bit) to keep up with good practices.

## Genesis

`modulr` has been developed by the [University of Lausanne](http://www.unil.ch) in Switzerland. The main goal of this package was to support the production of the institutional statistics and sets of indicators. Streamlined industrialization of data-related processes, agility, reusability and coding with fun in a distributed development environment were the first requirements. 

`modulr` is in production for several months as by August 2015, with unprecedented results and great adoption among various teams. Therefore, we are thrilled to open the code and share it with the vibrant community of R users, teachers, researchers, and developers.

`modulr` is deeply inspired from [AngularJS](https://angularjs.org/) and [RequireJS](http://requirejs.org) for Javascript, as well as [guice](https://github.com/google/guice) for Java.

## Installation

You can install the latest development version of `modulr` from github with
``` r
install.packages("devtools") # if not already installed
library(devtools)
devtools::install_github("aclemen1/modulr")
```

If you encounter a clear bug, please file a minimal reproducible example on github.

## A short example

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
```

The anatomy of this module is very simple: "data/student" is its name and the body of the function following the `%provides%` operator (which is part of a _syntactic sugar_ for the more verbose function `define`) contains its core functionality, namely returning the required data frame.

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
```

Now that we have these two modules at our disposal, let's combine them into another module that will return a student-teacher ratio.

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
```

The `%requires%` operator allows us to specify on which modules we rely.

  1. The first `define()` call registers a new module named `greeter`. This module has no dependency and provides a one parameter function which returns a greeting string. Once defined, this module can be injected into other modules. It will behave like a __singleton__, i.e. only one instance of the module `greeter` is produced and reused.
  2. The second `define()` call registers a new module named `who`. This module has no dependency and provides a single string.
  3. The third `define()` call is intended to produce the main module. Its name is `__runtime__` and is a reserved word. This module depends on `greeter` and `who`, two modules which singleton instances are __injected__ as `g` and `w`, respectively. The module then uses these components to print a greeting message `g(w)`.
  4. The module instance is evaluated with `$eval()`.

## Code of Conduct

This project adheres to the [Open Code of Conduct][code-of-conduct]. By participating, you are expected to honor this code.
[code-of-conduct]: http://todogroup.org/opencodeofconduct/#modulr/alain.clement-pavon@unil.ch
