if(!exists(".old_wd")) {
  .old_wd = getwd()
  setwd(file.path(getwd(), "example"))
  message("Switched to example's working directory: ", getwd())
}

# ------------------------------------------------------------------------------

library(modulr)

# Configuration
configure(list(
  # Namespace ==> path correspondances
  paths = list(
    "unisis" = "lib/unisis"
    ),
  # Modules parameters
  parameters = list(
    "unisis/dwh/get_connection" = list(
      configs = list(
        dev = list(
          host = "devcog.unil.ch",
          port = 1521,
          service_name = "Sinfpildev.unil.ch",
          username = "unisis",
          password = "unisis35"
          ),
        prod = list(
          host = "prdcog.unil.ch",
          port = 1521,
          service_name = "Sinfpilprd.unil.ch",
          username = "unisis",
          password = "unisis35"
          )
        ),
      stage = "dev"
      )
    )
  ))

# Main module
.main <- "main" %requires% list("unisis/dwh", "modulr") %provides%
  function(dwh, modulr) {
    all_tables <<- dwh$get_all_tables()
    rawub <<- dwh$get_table("RAWUB")
    NULL
  }

# Main module instanciation
.main()
#.main(force_reinstanciate=T)
#.main(force_reinstanciate_all = T)

# Objects in the environment
print(ls())

# ------------------------------------------------------------------------------

if(exists(".old_wd")) {
  setwd(.old_wd)
  message("Switched back to previous working directory: ", getwd())
  rm(.old_wd)
}
#rm(list=ls(all.names=T))
