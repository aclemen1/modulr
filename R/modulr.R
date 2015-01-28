#' modulr -- Module Pattern and DI in R
#'
#' modulr is a Module Pattern and Dependency Injection implementation in R.
#' Module Pattern and DI allows you to encapsulate pieces of code into useful singleton units,
#' namely modules that register their capabilities, export values and rely on other modules as dependencies.
#' modulr is widely inspired from RequireJS and AngularJS for Javascript.
#'
#' @docType package
#' @name modulr
#' @author Alain Clément-Pavon <\email{alain.clement-pavon@@unil.ch}>

NULL

library(knitr)
library(pooh)
library(stringr)

RESERVED_NAMES <- c("modulr")

modulr_env <- new.env()

assign("register", list(), pos = modulr_env)
assign("configuration", list(modules=list()), pos = modulr_env)


#' Configure modulr.
#'
#' @export

# configure <- function(configuration) {
#   assign("configuration", configuration, pos = modulr_env)
# }

.config <- function(scope) {
  set <- function(..., drop = T) {
    options_list = list(...)
    if(is.null(names(options_list))
       & length(options_list) == 1)
      if(is.list(options_list[[1]]))
        options_list <- options_list[[1]]
    if(length(options_list) == 0) return(invisible(NULL))
    configuration <- base::get("configuration", pos = modulr_env)
    if(is.null(configuration[[scope]])) {
      configuration[[scope]] <- options_list
    } else {
      for(key in names(options_list))
        if(is.null(configuration[[scope]][[key]]) | drop)
          configuration[[scope]][[key]] <- options_list[[key]]
    }
    assign("configuration", configuration, pos = modulr_env)
  }
  get_all <- function() {
    config <- base::get("configuration", pos = modulr_env)
    config[[scope[1]]][[scope[2]]]
  }
  get <- function(key) {
    get_all()[[key]]
  }
  list(
    set = set,
    get = get,
    get_all = get_all
    )
}

#' All configurations.
#'
#' @export
get_all_configs <- function() get("configuration", pos = modulr_env)


#' Paths configuration.
#'
#' @export
paths_config <-
  .config("paths")


#' Maps configuration.
#'
#' @export
maps_config <-
  .config("maps")


#' Enable module auto redefinition.
#'
#' @export
enable_auto_redefine_and_reinstanciate <- function(name) {
  module_option(name)$set(".__redefine_and_reinstanciate__" = T)
}


#' Enable module auto reinstanciation.
#'
#' @export
enable_auto_reinstanciate <- function(name) {
  module_option(name)$set(".__reinstanciate__" = T)
}


#' Disable module auto redefinition.
#'
#' @export
disable_auto_redefine_and_reinstanciate <- function(name) {
  module_option(name)$set(".__redefine_and_reinstanciate__" = F)
}


#' Disable module auto reinstanciation.
#'
#' @export
disable_auto_reinstanciate <- function(name) {
  module_option(name)$set(".__reinstanciate__" = F)
}


#' Module options.
#'
#' @export
module_option <- function(name)
  .config(c("modules", name))

#' Syntactic sugar for setting default module options.
#'
#' @export
`%has_default_option%` = function(lhs, rhs) {
  module_option(as.character(lhs))$set(as.list(rhs), drop = F)
}

#' Syntactic sugar for setting module options.
#'
#' @export
`%has_option%` = function(lhs, rhs) {
  module_option(as.character(lhs))$set(as.list(rhs), drop = T)
}


.resolve_mapping <- function(name, scope_name) {
  mappings <- get("configuration", pos = modulr_env)$mappings[[scope_name]]
  if(is.null(mappings)) return(name)

  candidates <- Map(function(map) {
    start_end_pos <- str_locate(name, map)
    list(
      map = map
      , start = start_end_pos[1]
      , end = start_end_pos[2]
    )
  }, names(mappings))

  candidates <- Filter(function(candidate) {
    candidate$start == 1
  }, candidates)

  if(length(candidates) == 0) return(name)

  maximum_length <- max(unlist(Map(function(candidate) {
    candidate$end
  }, candidates)))

  candidates <- Filter(function(candidate) {
    candidate$end == maximum_length
  }, candidates)

  if(length(candidates) > 1) warning(
    "More than one matching mapping. ",
    "Considering only the first occurence.")

  matching_map <- candidates[[1]]$map

  str_replace(name, matching_map, mappings[[matching_map]])

}

.make_path <- function(path) {
  if(str_sub(path, -1) != "/") path <- paste0(path, "/")
  path
}

.split_filename <- function(filename) {
  components <- str_split(filename, "/")[[1]]
  basename <- tail(components, 1)
  basename_components <- str_split(basename, "\\.")[[1]]
  if(length(basename_components) > 1) {
    extension <- tail(basename_components, 1)
    name <- paste(head(basename_components, length(basename_components) - 1), collapse = ".")
  } else {
    extension <- ""
    name <- basename_components
  }
  path <- paste(
    head(components, length(components) - 1),
    collapse = "/")
  list(
    filename = filename
    , path = path
    , basename = basename
    , name = name
    , extension = extension
  )
}

.resolve_path <- function(name, scope_name) {
  configuration <- get("configuration", pos = modulr_env)

  if(missing(scope_name)) injected_name <- name else
    injected_name <- .resolve_mapping(name, scope_name)

  splitted_injected_name <- .split_filename(injected_name)
  injected_namespace <- splitted_injected_name$path
  injected_basename <- splitted_injected_name$basename

  candidates <- Map(function(namespace) {
    path <- .split_filename(.make_path(namespace))$path
    start_end_pos <- str_locate(injected_namespace, path)
    list(
      namespace = namespace
      , start = start_end_pos[1]
      , end = start_end_pos[2]
    )
  },
  names(configuration$paths))

  candidates <- Filter(function(candidate) {
    candidate$start == 1
  }, candidates)

  if(length(candidates) == 0) return(injected_name)

  maximum_length <- max(unlist(Map(function(candidate) {
    candidate$end
  }, candidates)))

  candidates <- Filter(function(candidate) {
    candidate$end == maximum_length
  }, candidates)

  if(length(candidates) > 1) warning(
    "More than one matching namespace. ",
    "Considering only the first occurence.")

  matching_namespace <- candidates[[1]]$namespace

  str_replace(injected_name, matching_namespace,
              normalizePath(configuration$paths[[matching_namespace]]))
}

.is_defined <- function(name) {
  !is.null(get("register", pos = modulr_env)[[name]])
}


#' Import module.
#'
#' @export

import <- function(name, scope_name, force_reimport = F) {
  if(!(name %in% RESERVED_NAMES) & (!.is_defined(name) | force_reimport)) {
    if(missing(scope_name)) path <- .resolve_path(name) else
      path <- .resolve_path(name, scope_name)
    if(file.exists(paste0(path, ".R"))) {
      message("Importing file '", path, ".R'.")
      source(paste0(path, ".R"))
      return(paste0(path, ".R"))
    } else if(file.exists(paste0(path, ".Rmd"))) {
      message("Importing file '", path, ".Rmd'.")
      unnamed_chunk_label_opts = knitr::opts_knit$get("unnamed.chunk.label")
      knitr::opts_knit$set("unnamed.chunk.label" = paste("modulr", name, sep="/"))
      source(knitr::knit(paste0(path, ".Rmd"), output = tempfile(fileext = ".R"), tangle = T, quiet = T))
      knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)
      return(paste0(path, ".Rmd"))
    } else if (!.is_defined(name))
      warning("File '", path, ".R[md]' not found.")
    NULL
  }
}

#' Reimport module.
#'
#' @export

reimport <- function(name, scope_name)
  import(name, scope_name, force_reimport = T)


#' Redefine module.
#'
#' @export

redefine <- reimport

# make sure all dependent modules are defined
.define_all_dependent_modules <- function(name, force_reimport_all = F) {
  visited_dependencies <- list()
  iteration <- function(name, scope_name) {
    if(!(name %in% visited_dependencies)) {
      import(name, scope_name, force_reimport_all)
      visited_dependencies <<- c(visited_dependencies, name)
      Map(function(dependency) iteration(dependency, name),
          get("register", pos = modulr_env)[[name]]$dependencies)
    }
  }
  iteration(name)
  unlist(visited_dependencies)
}

.build_dependency_graph <- function(all_dependencies) {
  from <- c()
  to <- c()
  for(name in all_dependencies) {
    dependencies <- get("register", pos = modulr_env)[[name]]$dependencies
    if(length(dependencies) > 0) {
      array <- rbind(unlist(dependencies), name, deparse.level = 0)
      from <- c(from, array[1, ])
      to <- c(to, array[2, ])
    }
  }
  list(
    from = from,
    to = to
  )
}

.topological_sort <- function(graph) {
  if(length(graph$from) > 0)
    pooh::tsort(graph$from, graph$to)
}


#' Instanciate a module.
#'
#' @export

instanciate <- function(name,
                        force_reinstanciate = F,
                        force_redefine_reinstanciate = F,
                        force_reinstanciate_all = F,
                        force_redefine_reinstanciate_all = F) {
  redefine_and_reinstanciate_mode <-
    module_option(name)$get(".__redefine_and_reinstanciate__")
  if(is.null(redefine_and_reinstanciate_mode))
    redefine_and_reinstanciate_mode <- F
  else if(redefine_and_reinstanciate_mode)
    message("Module '", name,
            "' auto-redefinition and reinstanciation enabled.")
  if(!force_redefine_reinstanciate_all &
       (force_redefine_reinstanciate | redefine_and_reinstanciate_mode))
    redefine(name)
  all_dependencies <-
    .define_all_dependent_modules(
      name,
      force_redefine_reinstanciate_all)
  dependency_graph <- .build_dependency_graph(all_dependencies)
  ordered_names <- .topological_sort(dependency_graph)
  if(is.null(ordered_names)) ordered_names <- name
  register <- get("register", pos = modulr_env)
  for(ordered_name in ordered_names) {
    redefine_and_reinstanciate_mode <-
      module_option(ordered_name)$get(".__redefine_and_reinstanciate__")
    if(is.null(redefine_and_reinstanciate_mode))
      redefine_and_reinstanciate_mode <- F
    else if(redefine_and_reinstanciate_mode)
      message("Module '", ordered_name,
              "' auto-redefinition and reinstanciation enabled.")
    reinstanciate_mode <-
      module_option(ordered_name)$get(".__reinstanciate__")
    if(is.null(reinstanciate_mode))
      reinstanciate_mode <- F
    else if(reinstanciate_mode)
      message("Module '", ordered_name, "' auto-reinstanciation enabled.")
    if(!force_redefine_reinstanciate_all & redefine_and_reinstanciate_mode)
      redefine(ordered_name)
    module <- register[[ordered_name]]
    if(is.null(module))
      stop("Module '", ordered_name, "' not defined.")
    if(!module$instanciated
       | reinstanciate_mode | redefine_and_reinstanciate_mode
       | force_reinstanciate_all
       | force_redefine_reinstanciate_all
       | (force_reinstanciate & ordered_name == name)) {
      env = new.env()
      assign(".__name__", ordered_name, pos = env)
      if(length(module$dependencies) > 0) {
        args <- lapply(module$dependencies,
                       function(name) register[[name]]$instance)
        # tricky bug solution, see below
        module$instance <- evalq(do.call(
          eval(parse(text=deparse(module$factory))),
          args = args), envir = env)
      } else {
        # the deparse %>% parse %>% eval trick solves the following bug
        # WOKS:
        # module$instance <- evalq(do.call(function() {get("variable", pos = env)}, args = list()), envir = env)
        # DOES NOT WORK:
        # f <- function() {get("variable", pos = env)}
        # module$instance <- evalq(do.call(f, args = list()), envir = env)
        # WORKAROUND:
        # module$instance <- evalq(do.call(eval(parse(text=deparse(f))), args = list()), envir = env)
        module$instance <- evalq(do.call(
          eval(parse(text=deparse(module$factory))),
          args = list()), envir = env)
      }
      module$instanciated <- T
      register[[ordered_name]] <- module
      assign("register", register, pos = modulr_env)
      if (!(ordered_name %in% RESERVED_NAMES))
        message("Module '", ordered_name, "' instanciated.")
    }
  }

  get("register", pos = modulr_env)[[name]]$instance
}

#' Reinstanciate a module.
#'
#' @export

reinstanciate <- function(name)
  instanciate(name, force_reinstanciate = T)


#' Get module defininition.
#'
#' @export
get_definition <- function(name) {
  wrapper = function(force_reinstanciate = F,
                     force_redefine_reinstanciate = F,
                     force_reinstanciate_all = F,
                     force_redefine_reinstanciate_all = F)
    instanciate(name,
                force_reinstanciate = force_reinstanciate,
                force_redefine_reinstanciate = force_redefine_reinstanciate,
                force_reinstanciate_all = force_reinstanciate_all,
                force_redefine_reinstanciate_all =
                  force_redefine_reinstanciate_all)

  invisible(wrapper)
}

#' Define a module.
#'
#' @param name  the module name, given as a character string.
#' @param dependencies  the list of module dependencies, given as module names.
#' @param factory the factory function.
#' @return a wrapper function around the module instanciation.
#' @examples
#' # define "module_1"
#' define("module_1", list(), function() {
#'  message("Module 1"); "value 1"})
#'
#' # define "module_2"
#' m2 <- define("module_2", list("module_1"), function(m1) {
#'  message("Module 2 with one dependency"); paste(m1, "value 2")})
#'
#' # instanciate "module_2"
#' m2()
#' @export

define <- function(name, dependencies, factory) {
  if(exists(".__filename__", where = parent.frame())) {
    message("Module imported.")
    filename = get(".__filename__", pos = parent.frame())
    print(filename)
  }

  register <- get("register", pos = modulr_env)

  register[[name]]$name <- name
  register[[name]]$dependencies <- dependencies
  register[[name]]$factory <- factory
  register[[name]]$instance <- NULL
  register[[name]]$instanciated <- F

  assign("register", register, pos = modulr_env)

  if(!(name %in% RESERVED_NAMES))
    message("Module '", name, "' defined.")

  get_definition(name)
}


#' Remove all module definitions.
#'
#' @export

reset <- function() {
  assign("register", NULL, pos = modulr_env)
  init()
}

#' Undefine module.
#'
#' @export

undefine <- function(name) {
  if(!(name %in% RESERVED_NAMES)) {
    register <- get("register", pos = modulr_env)
    register[[name]] <- NULL
    assign("register", register, pos = modulr_env)
  }
}

#' Syntactic sugar to require dependencies, to be used in conjunction with \%provides\%.
#'
#' @export
`%requires%` = function(lhs, rhs) {
  list(name=as.character(lhs), dependencies=as.list(rhs))
}

#' Syntactic sugar to provide a factory, can be used in conjunction with \%requires\%.
#'
#' @export
`%provides%` = function(lhs, rhs) {
  if(!is.function(rhs))
    stop("Type mismatch, factory needed on RHS.")
  if(is.list(lhs)) {
    if(!identical(names(lhs), c("name", "dependencies")))
      stop("Type mismatch, dependencies needed on LHS.")
    name <- lhs$name
    dependencies <- lhs$dependencies
  } else {
    name <- as.character(lhs)
    dependencies <- list()
  }
  factory <- rhs
  do.call(define, args = list(name, dependencies, factory),
          envir = parent.frame())
}


define_modulr = function() {
  define("modulr", list(), function() {
    list(
      get_module_options = function()
        module_option(get(".__name__", pos = parent.frame()))$get_all(),
      get_module_name = function()
        get(".__name__", pos = parent.frame())
    )
  })
}

init = function() {
  define_modulr()
}

init()
