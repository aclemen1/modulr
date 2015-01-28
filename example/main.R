
## ----libraries-----------------------------------------------------------
library(modulr)
library(magrittr)
#reset()


## ----configuration-------------------------------------------------------
paths_config$set(
  "unisis" = "example/lib/unisis"
  )

"unisis/dwh/get_connection" %has_option% list(
   stage = "dev"
  )

"unisis/dwh/connection" %>% undebug
"main" %>% undebug

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
rm("all_tables")
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



