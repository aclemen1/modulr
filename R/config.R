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
    if(is.na(scope[2])) {
      config[[scope[1]]]
    } else {
      config[[scope[1]]][[scope[2]]]
    }
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
