library(modulr)
reset()

# Configuration
paths_config$set(
  "unisis" = "example/lib/unisis",
  "test" = "example"
  )

module_config("unisis/dwh/get_connection")$set(
#   stage = "dev"
  )

# Main module
.main <- "main" %requires% list("unisis/dwh") %provides%
  function(dwh) {
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
