source('R/dependencies.R')

get_query_auto <- function(username,password,dbname,query){
  drv <- dbDriver("Oracle")
  con <- dbConnect(drv, username = username, password = password, dbname = dbname)
  result <- dbGetQuery(con, query)

  # Close the database connection
  dbDisconnect(con)

  return(result)
}


get_query_window <- function(query){
  drv <- dbDriver("Oracle")
  con <- dbConnect(drv,
                   username = rstudioapi::showPrompt(
                     title = "Username", message = "Username", default = ""
                   ),
                   password = rstudioapi::askForPassword(prompt = "Password"),
                   dbname = rstudioapi::showPrompt(
                     title = "Database", message = "Which database", default = "HCPAOPS.WORLD")
  )

  result <- dbGetQuery(con, query)

  # Close the database connection
  dbDisconnect(con)

  return(result)
}
