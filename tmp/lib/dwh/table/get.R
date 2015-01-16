define("dwh/connection", function(connection) {
  message("get.R")
  function(table_name) {
    con = connection()
    rs = dbSendQuery(con, sprintf("SELECT * FROM DWH.%s", table_name))
    table = fetch(rs)
    dbDisconnect(con)
    table
  }
})

