#' modulr -- A Module Pattern for R
#'
#' modulr is an R implementation of the Module Pattern that allows you to encapsulate pieces of code into useful singleton units, namely modules that register their capabilities, export values and rely on other modules dependencies. modulr is widely inspired from RequireJS for Javascript and the work of James Burke and other contributors.
#'
#' @docType package
#' @name modulr
#' @author Alain Clément-Pavon <\email{alain.clement-pavon@@unil.ch}>

.modulr = (function() {

  .register = list()

  .config = list()

  .internals = function() {
    list(
      register = .register,
      config = .config
      )
  }

  configure = function(config) {
    .config <<- config
  }

  .is_normal = function(s) {
    !grepl(".R$", s, ignore.case = T)
  }

  .normalize = function(path) {
    .withSlash = function(s) {
      if(substr(s, nchar(s), nchar(s)) != "/") return(paste0(s, "/"))
      s
    }
    id = path
    idx = sapply(names(.config$paths), function(name) startsWith(path, .withSlash(.config$paths[[name]])))
    if(any(idx)) {
      lens = sapply(.config$paths[idx], nchar)
      name = names(.config$paths)[idx][which(lens == max(lens))]
      id = sub(.withSlash(.config$paths[[name]]), .withSlash(name), path)
    }
    sub("\\.[[:alnum:]]*$", "", id)
  }

  .denormalize = function(id) {
    if(!.is_normal(id)) return(id)
    .withSlash = function(s) {
      if(substr(s, nchar(s), nchar(s)) != "/") return(paste0(s, "/"))
      s
    }
    path = id
    idx = sapply(names(.config$paths), function(name) startsWith(id, .withSlash(name)))
    if(any(idx)) {
      lens = sapply(names(.config$paths)[idx], nchar)
      name = names(.config$paths)[idx][which(lens == max(lens))]
      path = paste0(sub(.withSlash(name), .withSlash(.config$paths[[name]]), id), ".R")
    }
    path
  }

  define = function(..., id, force = F) {
    if(missing(id)) {
      frame_files = Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
      if(length(frame_files) > 0) {
        frame_file = frame_files[[length(frame_files)]]
        id = .normalize(frame_file)
      } else {
        id = "__runtime__"
      }
    }

    if(id %in% names(.register) & !force)
      return(invisible(id))

    args = list(...)
    nargs = length(args)
    factory = args[[nargs]]
    fargs = formals(factory)
    nfargs = length(fargs)
    dependencies = list()

    if(nargs == 1) { # factory (args?)
      if(nfargs > 0) dependencies = as.list(names(fargs)) # factory (args+)
    } else if(nargs - 1 == nfargs) { # dependencies, factory (args?)
      dependencies = args[1:nfargs]
    } else {
      stop("Dependencies and arguments mismatch.")
    }

    .register[[id]] <<- list(
      id = id,
      dependencies = dependencies,
      factory = factory,
      instance = NULL
    )

    invisible(id)
  }

  .graph = function() {
    to = unlist(lapply(.register, function(r) {
      if(length(r$dependencies) > 0) {
        rep(r$id, length(r$dependencies))
      } else {
        NULL
      }
    }))
    from = unlist(lapply(.register, function(r) {
      if(length(r$dependencies) > 0) {
        as.vector(r$dependencies)
      } else {
        NULL
      }
    }))
    list(from = from, to = to)
  }
  .sub_graph = function(id, graph) {
    if(missing(graph)) {
      graph = .graph()
    }
    from = c(); to = c()
    to_ = id
    while(length(to_) > 0) {
      to__ = graph$from[graph$to %in% to_]
      from = c(from, to__)
      to = c(to, graph$to[graph$to %in% to_])
      to_ = to__
    }
    list(from = from, to = to)
  }
  .order = function(id) {
    graph = .sub_graph(id)
    if(length(graph$from) > 0) {
      pooh::tsort(graph$from, graph$to)
    } else {
      id
    }
  }
  .load = function(id, force = F) {
    if((!(id %in% names(.register)) | force) & id != "module") {
      id = source(.denormalize(id))$value
    }
    for(dep in .register[[id]]$dependencies) {
      .load(dep, force)
    }
  }
  .eval = function(id, force = F) {
    .load(id, force)
    order = .order(id)
    for(id in order[order != "module"]) {
#       if(!(id %in% names(.register))) {
#         id = source(.denormalize(id))$value
#       }
      module = .register[[id]]
      if(is.null(module$instance) | force) {
        if(length(module$dependencies) > 0) {
          args = lapply(.register[[id]]$dependencies, function(dep_id) {
            if(dep_id == "module") { # special module
              list(
                config = function() {
                  .config$config[[id]]
                }
              )
            } else {
              .register[[dep_id]]$instance
            }
          })
          module$instance = do.call(module$factory, args = args)
        } else {
          module$instance = module$factory()
        }
        .register[[id]] <<- module
      }
    }
    invisible(.register[[id]]$instance)
  }
  run = function(...) {
    id = define(..., force = T)
    invisible(.eval(id))
  }
  undef = function(id) {
    .register[[id]] <<- NULL
  }
  reset = function() {
    .register <<- list()
  }
  list(
    define = define,
    run = run,
    .eval = .eval,
    configure = configure,
    reset = reset,
    undef = undef,
    .internals = .internals
  )
})()

.modulr$define(id="require", function() {
  function(file) {
    id = source(file)$value
    invisible(modulr$.eval(id))
  }
})

#' Define a module.
#'
#' @param id  module id
#' @param ... dependencies and factory function
#' @return module factory singleton (invisible)
#' @examples
#' define(id = "module_1", function() {
#'  message("Module 1"); "value 1"})
#' define(id = "module_2", "module_1", function(m1) {
#'  message("Module 2 with one dependency"); paste(m1, "value 2")})
#' @export
define = .modulr$define


#' Configure the runtime environment.
#'
#' @param config  configuration
#' @export
configure = .modulr$configure


#' Run a module.
#'
#' @param id  module id
#' @param ... dependencies and factory function
#' @return module factory singleton (invisible)
#' @export
run = .modulr$run


#' Reset all modules.
#'
#' @export
reset = .modulr$reset


#' Undefine a module.
#'
#' @param id  module id
#' @export
undef = .modulr$undef


#' Evaluate a module.
#'
#' @param id  module id
#' @return module factory singleton (invisible)
#' @export
.eval = .modulr$.eval


#' Get internals
#'
#' @return list of internals
#' @export
.internals = .modulr$.internals
