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
        dev = F
        )
      )
    )
  )

run("table/all_tables", "table/get", function(all_tables, get_table) {
  all_tables <<- all_tables
  RAWUB <<- get_table("RAWUB")
  RAWRPCONTRAT <<- get_table("RAWRPCONTRAT")
})
