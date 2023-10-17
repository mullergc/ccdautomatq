library(ROracle)
library(googledrive)
library(googlesheets4)


log_oracle_path <- function(username,vpassword,vdbname){
  drv <- dbDriver("Oracle")
  dbConnect(drv, username = vusername, password = vpassword, dbname = vdbname)
}



log_oracle_window <- function(username,vpassword,vdbname){
  drv <- dbDriver("Oracle")
  library(ROracle)
  drv<-dbDriver("Oracle")
  dbConnect(drv,
                   username = rstudioapi::showPrompt(
                     title = "Username", message = "Username", default = ""
                   ),
                   password = rstudioapi::askForPassword(prompt = "Password"),
                   dbname =  rstudioapi::showPrompt(
                     title = "Database", message = "Which database", default = "HCPAOPS.WORLD")
  )


}
