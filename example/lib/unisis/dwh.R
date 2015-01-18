"unisis/dwh" %requires% list("unisis/dwh/get_connection", "modulr") %provides%
  function(get_connection, modulr) {

    require("DBI")
    require("dplyr")

    get_all_tables <- function() {
      connection <- get_connection()
      cursor <- dbSendQuery(
        connection,
        "SELECT owner, table_name FROM all_tables")
      all_tables <- fetch(cursor)
      dbDisconnect(connection)
      all_tables
    }

    get_table <- function(table_name) {
      connection <- get_connection()
      cursor <- dbSendQuery(
        connection,
        sprintf("SELECT * from DWH.%s", table_name))
      table <- fetch(cursor)
      dbDisconnect(connection)
      table
    }

    list(
        get_all_tables = get_all_tables
      , get_table = get_table
      )
  }
