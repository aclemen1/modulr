# modulr -- A Dependency Injection (DI) Framework for R

## Raison d'être

The `modulr` package aims for a clear separation between code complication and complexity in R, highlighting the core purpose and behaviour of objects (application code), and hiding their construction and wiring (infrastructure code). It allows to break down sequential programs into discrete, modular units that are loosely coupled (minimizing impact of significant changes to the internals of a module on other ones), simple to develop, test, re-use and share in a wide range of situations.

# Pros

  * modules are more reusable
  * modules are more testable
  * modules are more readable
  * modules are easier to maintain
  * modules allow for more SOLID (Single responsibility, Open-closed, Liskov substitution, Interface segregation and Dependency inversion) code

# Acknowledgment

`modulr` is widely inspired from `RequireJS` and `AngularJS` for Javascript.

# Installation

You can install the latest development version of `modulr` from github with
``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
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

