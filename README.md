# modulr — A Dependency Injection (DI) Framework for R

## Raison d'être

The `modulr` package is a Dependency Injection (DI) Framework for R. By design, `modulr` allows to break down sequential programs into discrete, modular units that are loosely coupled, simple to develop, test, reuse and share in a wide range of situations. As every DI framework, it aims for a clear separation between code complication and complexity, highlighting the core purpose and behaviour of objects (application code), and hiding their construction and wiring (infrastructure code). 

## Pros

  * modules are easy (and fun) to develop,
  * modules are easy to test,
  * modules are easy to read,
  * modules are easy to reuse,
  * modules are easy to share,
  * modules are easy to maintain, and
  * modules force (a bit) to keep up with good practices.

## Acknowledgments

`modulr` has been developed by the University of Lausanne in Switzerland. The main goal of this package was to support the production of the institutional statistics and sets of indicators. Streamlined industrialization of data-related processes, agility, reusability and coding with fun in a distributed development environment were the first requirements. 

`modulr` is in production for several months as by July 2015, with unprecedented results and great adoption among various teams. Therefore, it is an honour to open the code and share it with the vibrant community of R users, teachers, researchers, and developers.

`modulr` is deeply inspired from `AngularJS` and `RequireJS` for Javascript, as well as `guice` for Java.

## Installation

You can install the latest development version of `modulr` from github with
``` r
install.packages("devtools") # if not already installed
library(devtools)
devtools::install_github("aclemen1/modulr")
```

If you encounter a clear bug, please file a minimal reproducible example on github.

## A minimal example

To get started with `modulr`, let us consider the following minimal example:

``` r
require("modulr")

# define a module named "greeter" which provides 
# a one parameter function returning a greeting string.
define(id="greeter", function() {
    function(who) {
        sprintf("Hello %s", who)
    }
})

# define a module named "who" which provides a string
define(id="who", function() {
    "World"
})

# define a module into which both "greeter" and "who" are injected
# and used to print a greeting message.
main = define("greeter", "who", function(g, w) {
    print(g(w))
})

# evaluate the main module instance
main$eval()
```

  1. The first `define()` call registers a new module named `greeter`. This module has no dependency and provides a one parameter function which returns a greeting string. Once defined, this module can be injected into other modules. It will behave like a __singleton__, i.e. only one instance of the module `greeter` is produced and reused.
  2. The second `define()` call registers a new module named `who`. This module has no dependency and provides a single string.
  3. The third `define()` call is intended to produce the main module. Its name is `__runtime__` and is a reserved word. This module depends on `greeter` and `who`, two modules which singleton instances are __injected__ as `g` and `w`, respectively. The module then uses these components to print a greeting message `g(w)`.
  4. The module instance is evaluated with `$eval()`.

