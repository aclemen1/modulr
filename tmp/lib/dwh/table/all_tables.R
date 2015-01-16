define("dwh/connection", function(connection) {
  message("all_tables.R")
  con = connection()
  rs = dbSendQuery(con, "SELECT owner, table_name FROM all_tables")
  table = fetch(rs)
  dbDisconnect(con)
  table
  })

