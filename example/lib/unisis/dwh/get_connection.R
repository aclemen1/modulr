"unisis/dwh/get_connection" %requires% list("modulr") %provides%
  function(modulr) {
    require("DBI")
    require("ROracle")

    function() {
      parameters <- modulr$get_parameters()

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
