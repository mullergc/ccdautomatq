source('R/dependencies.R')
library(ROracle)
get_query_auto <- function(username,vpassword,vdbname,query){
  drv <- dbDriver("Oracle")
  con <- dbConnect(drv, username = vusername, password = vpassword, dbname = vdbname)
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
