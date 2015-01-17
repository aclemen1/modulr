require(modulr)

reset()

configure(
  list(
    paths = list(
      "dwh" = "tmp/lib/dwh",
      "table" = "tmp/lib/dwh/table"
      ),
    config = list(
      "dwh/connection" = list(
        dev = T
        )
      )
    )
  )

main = define("table/all_tables", "table/get", function(all_tables, get_table) {
  all_tables <<- all_tables
  RAWUB <<- get_table("RAWUB")
  RAWRPCONTRAT <<- get_table("RAWRPCONTRAT")
  NULL
})
main$eval()

################################

undef("__runtime__")

greeter = define(id="greeter", function() {
  function(who) {
    sprintf("Hello %s", who)
  }
})
greeter$eval()("Alain")

who = define(id="who", function() {
  "World"
})
who$eval()

greetings = define("greeter", "who", function(g, w) {
  g(w)
})
greetings$eval()

reset()
