# #source('R/get_query.R')
# source('get_sql.R')
# source('write_sheets.R')

require('ROracle')
require('googlesheets4')
require('dplyr')
require('jsonlite')
require('lubridate')
require('googledrive')
require('devtools')

get_query_auto <- function(username,vpassword,vdbname,query){
  drv <- DBI::dbDriver("Oracle")
  con <- DBI::dbConnect(drv, username = vusername, password = vpassword, dbname = vdbname)
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

write_query_sheet <- function(df, url_destiny, sheetname = 'Página1', date_change = FALSE) {
  if (!date_change) {
    df_final <- df
  } else {
    date_columns <- grep("DT|DTHR|DATA", names(df), value = TRUE)
    df_final <- df %>%
      dplyr::mutate(across(all_of(date_columns), ~ parse_date_time(.x, c("ymd HMS",'ymd','dmy','dmy HMS'),tz='UTC')))
  }

  googlesheets4::write_sheet(df_final, ss = url_destiny, sheet = sheetname)
}

read_sql <- function(email,url) {

  googledrive::drive_auth(email = email)

  codigo <- googledrive::drive_read_string(url,  encoding = "UTF8")
  #cat(codigo,sep = "\n")
  # temp_file <- tempfile()
  # writeLines(codigo, temp_file)
  # source(temp_file)
  # rm(temp_file) # limpa da memória o arquivo temp_file
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  return(codigo)

}


# Define a function to log errors
log_error <- function(error_msg,num_chamado) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = paste0("error_log_",num_chamado,".txt"), append = TRUE)
}

# Define your run_function
run_function <- function(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE, sheetname = 'Página1') {
  # Use tryCatch to handle errors
  tryCatch({
    # Read the JSON file as text
    creds_text <- readLines(credspath, warn = FALSE)

    # Parse the JSON text
    creds <- jsonlite::fromJSON(creds_text)
    # Extract the credentials
    email <- creds$email
    username <- creds$username
    password <- creds$pass
    dbname <- creds$dbname
    sql <- read_sql(email = email, url = url_sql)
    r <- get_query_auto(usernamedb = username, passwordb = password, dbname = dbname, query = sql)
    write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname)

    # Success message
    cat("Function executed successfully\n")
  }, error = function(e) {
    # Handle errors and log them
    error_msg <- conditionMessage(e)
    log_error(error_msg,num_chamado)
    cat("Function encountered an error.See error_log please\n",num_chamado)
  })
}
