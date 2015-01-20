module_config("unisis/dwh/get_connection")$set(drop = F,
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
  stage = "prod"
)

"unisis/dwh/get_connection" %requires% list("modulr") %provides%
  function(modulr) {
    require("DBI")
    require("ROracle")

    function() {
      parameters <- modulr$get_module_config()

      driver <- dbDriver("Oracle")

      host <- parameters$configs[[parameters$stage]]$host
      port <- parameters$configs[[parameters$stage]]$port
      service_name <- parameters$configs[[parameters$stage]]$service_name

      connect_string <- paste(
        "(DESCRIPTION=",
        "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
        "(CONNECT_DATA=(SERVICE_NAME=", service_name, ")))", sep = "")

      dbConnect(
        driver,
        username = parameters$configs[[parameters$stage]]$username,
        password = parameters$configs[[parameters$stage]]$password,
        dbname = connect_string)
    }
  }
