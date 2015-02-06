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

RESERVED_NAMES <- c("modulr")

modulr_env <- new.env()

assign("register", list(), pos = modulr_env)
assign("configuration", list(modules=list()), pos = modulr_env)
assign("message_handler", NULL, pos = modulr_env)
assign("message_closed", "", pos = modulr_env)

.reverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste,
                               collapse="")

.cuts <- function(string, revert = F, ...) {
  if(revert) string <- .reverse(string)
  lines <- unlist(strwrap(string, simplify = F, ...))
  last_line <- tail(lines, 1L)
  if(length(lines) > 1) {
    first_lines <- paste(head(lines, -1), collapse = "\n")
  } else first_lines <- NULL
  if(revert) {
    first_lines <- sapply(first_lines, .reverse)
    last_line <- .reverse(last_line)
  }
  list(
    first_lines = first_lines,
    last_line = last_line
  )
}

.message_reopen <- function(handler) {
  assign("message_handler", handler, pos = modulr_env)
  if(!is.null(handler))
    if(handler$output) {
      if(!is.null(handler$first_lines))
        #message(handler$first_lines)
        cat(handler$first_lines, sep="\n")
      #message(handler$last_line, appendLF = F)
      cat(handler$last_line, sep="")
    }
}

message_open <- function(announce, output = T, ...) {
  cut <- .cuts(paste0("[", Sys.time(), "] ", announce),
               width = 0.9 * getOption("width"), ...)
  handler <- list(
    first_lines = cut$first_lines,
    last_line = cut$last_line,
    output = output,
    args = list(...)
  )
  assign("message_handler", handler, pos = modulr_env)
  assign("message_closed", "", pos = modulr_env)
  .message_reopen(handler)
}

.message <- function(f, type = "INFO", ...) {
  handler <- get("message_handler", pos = modulr_env)
  closed <- get("message_closed", pos = modulr_env)
  if(!is.null(handler) & closed != type) {
    message_close(type)
    assign("message_handler", handler, pos = modulr_env)
    assign("message_closed", type, pos = modulr_env)
  }
  f(...)
}

.dots_print <- function(...) {
  handler <- get("message_handler", pos = modulr_env)
  if(is.null(handler))
    prefix = paste0("[", Sys.time(), "] ")
  else
    prefix = ""
  cat(unlist(strwrap(paste0(prefix, ...),
                     width=0.9 * getOption("width"))),
      sep = "\n")
}

message_info <- function(...) .message(.dots_print, type = "INFO", ...)
message_warn <- function(...) .message(.dots_print, type = "WARN", ...)
message_stop <- function(...) {
  .message(.dots_print, type = "STOP", ...)
  stop("modulr stopped.", call. = F)
}

# message_info <- function(...) .message(message, type = "INFO", ...)
# message_warn <- function(...) .message(
#   function(...) warning(..., immediate. = T), type = "WARN", ...)
# message_stop <- function(...) .message(stop, type = "STOP", ...)

message_close <- function(result) {
  handler <- get("message_handler", pos = modulr_env)
  if(!is.null(handler)) {
    closed <- get("message_closed", pos = modulr_env)
    if(closed != "") {
      .message_reopen(handler)
      assign("message_closed", "", pos = modulr_env)
    }
    if(handler$output) {
      cut <- do.call(.cuts, args =
                       c(list(result, revert = T, width = 0.9 * getOption("width")),
                         handler$args))
      n_dots <-
        0.9 * getOption("width") - nchar(handler$last_line) - nchar(cut$last_line)
      if(n_dots<3) {
        dots <-
          paste(
            paste(rep(".", max(0,
                               0.9 * getOption("width") - nchar(handler$last_line))),
                  collapse=""),
            paste(rep(".", max(3, 0.9 * getOption("width") - nchar(cut$last_line))),
                  collapse=""),
            sep = "\n")
      } else {
        dots <- rep(".", n_dots)
      }
      #message(dots, cut$last_line)
      cat(dots, cut$last_line, "\n", sep="")
      if(length(cut$first_lines) > 0)
        #message(cut$first_lines)
        cat(cut$first_lines, "\n", sep="")
    }
  }
  assign("message_handler", NULL, pos = modulr_env)
}


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

resolve_path <- function(name, scope_name) {
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
    if(missing(scope_name)) path <- resolve_path(name) else
      path <- resolve_path(name, scope_name)
    if(file.exists(paste0(path, ".R"))) {
#       message_open(sprintf("Module '%s'", name))
#       message_close("Found in .R file ")
      source(paste0(path, ".R"))
      return(paste0(path, ".R"))
    } else if(file.exists(paste0(path, ".Rmd"))) {
#       message_open(sprintf("Module '%s'", name))
#       message_close("Found in .Rmd file")
      unnamed_chunk_label_opts = knitr::opts_knit$get("unnamed.chunk.label")
      knitr::opts_knit$set("unnamed.chunk.label" = paste("modulr", name, sep="/"))
      source(knitr::knit(paste0(path, ".Rmd"), output = tempfile(fileext = ".R"), tangle = T, quiet = T))
      knitr::opts_knit$set("unnamed.chunk.label" = unnamed_chunk_label_opts)
      return(paste0(path, ".Rmd"))
    } else if (!.is_defined(name)) {
      message_open(sprintf("Module '%s'", name))
      message_close(sprintf("Not found", path))
    }
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

instanciate <- function(name, debug = F, force = F) {
  all_dependencies <-
    .define_all_dependent_modules(
      name,
      debug)
  dependency_graph <- .build_dependency_graph(all_dependencies)
  ordered_names <- .topological_sort(dependency_graph)
  if(is.null(ordered_names)) ordered_names <- name
  for(ordered_name in ordered_names) {
    register <- get("register", pos = modulr_env)
    module <- register[[ordered_name]]
    if(is.null(module))
      stop("Module '", ordered_name, "' not defined.")
    reinstanciated_by_parent <- any(unlist(lapply(
      module$dependencies,
      function(name) {
        register[[name]]$reinstanciate_children
      })))
    if(reinstanciated_by_parent) {
#       if(module$instanciated & ordered_name != name) {
#         message("Module '", ordered_name, "' needs reinstanciation by parent.")
#       }
      module$reinstanciate_children <- T
    }
    if(!module$instanciated
       | reinstanciated_by_parent
       | ((debug | force) & ordered_name == name)) {
      if (!(ordered_name %in% RESERVED_NAMES))
        message_open(sprintf("Module '%s'", ordered_name))
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
      if (!(ordered_name %in% RESERVED_NAMES))
        if(module$instanciated | !module$first_instance)
          message_close("Re-Instanciated")
        else
          message_close("Instanciated")
      module$instanciated <- T
      module$first_instance <- F
      register[[ordered_name]] <- module
      assign("register", register, pos = modulr_env)
    }
  }

  for(name in ordered_names) {
    register <- get("register", pos = modulr_env)
    register[[name]]$reinstanciate_children <- F
    assign("register", register, pos = modulr_env)
  }

  get("register", pos = modulr_env)[[name]]$instance
}


#' Syntactic sugar to instanciate a module.
#'
#' @export
`%<<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         instanciate(rhs, debug = T), pos = 1)
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%=>>%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(rhs))
  assign(variable_name,
         instanciate(lhs, debug = T), pos = 1)
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%<=%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(lhs))
  assign(variable_name,
         instanciate(rhs, debug = F), pos = 1)
}

#' Syntactic sugar to instanciate a module.
#'
#' @export
`%=>%` <- function(lhs, rhs) {
  variable_name <- as.character(substitute(rhs))
  assign(variable_name,
         instanciate(lhs, debug = F), pos = 1)
}

#' Get module defininition.
#'
#' @export
get_definition <- function(name) {
  wrapper = function(debug = F,
                     force = F)
    instanciate(name,
                debug = debug,
                force = force)
  invisible(wrapper)
}

# get_definition <- function(name) {
#   wrapper = function(force_reinstanciate = F,
#                      force_redefine_reinstanciate = F,
#                      force_reinstanciate_all = F,
#                      force_redefine_reinstanciate_all = F)
#     instanciate(name,
#                 force_reinstanciate = force_reinstanciate,
#                 force_redefine_reinstanciate = force_redefine_reinstanciate,
#                 force_reinstanciate_all = force_reinstanciate_all,
#                 force_redefine_reinstanciate_all =
#                   force_redefine_reinstanciate_all)
#
#   invisible(wrapper)
# }

.signature <- function(name) {
  register <- get("register", pos = modulr_env)
  module <- register[[name]]
  digest(c(
    deparse(module$dependencies),
    deparse(module$factory)), "sha1")
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
#   if(exists(".__filename__", where = parent.frame())) {
#     message("Module imported.")
#     filename = get(".__filename__", pos = parent.frame())
#     print(filename)
#   }

  if(!(name %in% RESERVED_NAMES))
    message_open(sprintf("Module '%s'", name))

  register <- get("register", pos = modulr_env)

  if(is.null(register[[name]])) {
    register[[name]]$name <- name
    register[[name]]$dependencies <- dependencies
    register[[name]]$factory <- factory
    register[[name]]$signature <- digest(c(
      deparse(dependencies),
      deparse(factory)), "sha1")
    register[[name]]$instance <- NULL
    register[[name]]$instanciated <- F
    register[[name]]$first_instance <- T
    register[[name]]$reinstanciate_children <- T

    if(!(name %in% RESERVED_NAMES))
      message_close("Defined")
  } else {
    previous_signature <- register[[name]]$signature
    signature <- digest(c(
      deparse(dependencies),
      deparse(factory)), "sha1")
    if(signature != previous_signature) {
      register[[name]]$dependencies <- dependencies
      register[[name]]$factory <- factory
      register[[name]]$signature <- signature
      register[[name]]$instance <- NULL
      register[[name]]$instanciated <- F
      register[[name]]$first_instance <- F
      register[[name]]$reinstanciate_children <- T
      if(!(name %in% RESERVED_NAMES))
        message_close("Changed and re-defined")
    } else {
      register[[name]]$reinstanciate_children <- F
      if(!(name %in% RESERVED_NAMES))
        message_close("Unchanged")
    }
  }

  assign("register", register, pos = modulr_env)

  get_definition(name)
}


#' Remove all module definitions.
#'
#' @export

reset <- function() {
  message_open("Package 'modulr'")
  assign("register", NULL, pos = modulr_env)
  init()
  message_close("Reset")
}

#' Undefine module.
#'
#' @export

undefine <- function(name) {
  if(!(name %in% RESERVED_NAMES)) {
    message_open(sprintf("Module '%s'", name))
    register <- get("register", pos = modulr_env)
    register[[name]] <- NULL
    assign("register", register, pos = modulr_env)
    message_close("Undefined")
  }
}

#' Touch module.
#'
#' @export

touch <- function(name) {
  if(!(name %in% RESERVED_NAMES)) {
    message_open(sprintf("Module '%s'", name))
    register <- get("register", pos = modulr_env)
    register[[name]]$signature <- 0
    assign("register", register, pos = modulr_env)
    message_close("Touched")
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

#' Get internals
#'
#' @export
.internals <- function() {
  list(
    env = modulr_env,
    register = get("register", pos = modulr_env),
    configuration = get("configuration", pos = modulr_env)
    )
}

define_modulr <- function() {
  define("modulr", list(), function() {
    list(
      get_module_options = function()
        module_option(get(".__name__", pos = parent.frame()))$get_all(),
      get_module_name = function()
        get(".__name__", pos = parent.frame()),
      get_filename = function() {
        name <- get(".__name__", pos = parent.frame())
        resolve_path(name)
      },
      get_dirname = function() {
        name <- get(".__name__", pos = parent.frame())
        dirname(resolve_path(name))
      },
      resolve_path = resolve_path,
      message_info = message_info,
      message_warn = message_warn,
      message_stop = message_stop
      )
  })
}

init = function() {
  define_modulr()
}

init()

