require("DBI")
require("ROracle")

define("module", function(module) {
  message("connection.R")
  config = module$config()
  if(config$dev) {
    host = "devcog.unil.ch"
    serviceName = "Sinfpildev.unil.ch"
  } else {
    host = "prdcog.unil.ch"
    serviceName = "Sinfpilprd.unil.ch"
  }
  function() {
    drv = dbDriver("Oracle")

    port = 1521
    sid = ""
    connect.string <- paste(
      "(DESCRIPTION=",
      "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
      "(CONNECT_DATA=(SERVICE_NAME=", serviceName, ")))", sep = "")

    dbConnect(drv, username="unisis", password="unisis35", dbname = connect.string)
  }
  })
