
## ----libraries-----------------------------------------------------------
library(modulr)


## ----configuration-------------------------------------------------------
paths_config$set(
  "unisis" = "lib/unisis"
  )

"unisis/dwh/get_connection" %has_option% list(
   #stage = "dev"
  )


## ----definition----------------------------------------------------------
# Main module
.main <- "main" %requires% list("unisis/dwh/raw/table") %provides%
  function(raw_table) {
    all_tables <<- raw_table$get_all()
    rawub <<- raw_table$get("UB")
#    rawadresses <<- raw_table$get("ADRESSES")
    NULL
  }


## ----instanciation, message=FALSE, results='hide'------------------------
.main()


## ----output--------------------------------------------------------------
print(all_tables)


## ----output2-------------------------------------------------------------
str(rawub)


## ----output3-------------------------------------------------------------
#print(head(rawadresses))


## ------------------------------------------------------------------------
toto <- "toto" %provides% (
  toto_factory <- function() {
    "Hello World"
    })



