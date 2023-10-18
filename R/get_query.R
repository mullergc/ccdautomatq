source('R/dependencies.R')

get_query_auto <- function(usernamedb,passwordb,dbname,query){
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = usernamedb, password = passwordb, dbname = dbname)
  result <- ROracle::dbGetQuery(con, query)

  # Close the database connection
  dbDisconnect(con)

  return(result)
}


get_query_window <- function(query){
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv,
                   username = rstudioapi::showPrompt(
                     title = "Username", message = "Username", default = ""
                   ),
                   password = rstudioapi::askForPassword(prompt = "Password"),
                   dbname = rstudioapi::showPrompt(
                     title = "Database", message = "Which database", default = "HCPAOPS.WORLD")
  )

  result <- ROracle::dbGetQuery(con, query)

  # Close the database connection
  dbDisconnect(con)

  return(result)
}
