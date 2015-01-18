library(modulr)

# Configuration
configure(list(
  # Namespace ==> path correspondances
  paths = list(
    "unisis" = "example/lib/unisis" # register UNISIS modules installation
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
.main(force_reinstanciate=T) # will reload the tables
#.main(force_reinstanciate_all = T)

# Objects in the environment
print(ls())
