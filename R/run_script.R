#library(ROracle)


# Define a function to log errors
log_error <- function(error_msg,num_chamado) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = paste0("error_log_",num_chamado,".txt"), append = TRUE)
}

# Define your run_function
run_function <- function(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE, sheetname = 'Pagina1') {
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



# Define your run_function
gw_query <- function(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE, sheetname = 'Pagina1') {
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


# Define your run_function
gtw_query <- function(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE,url_ds, sheetname = 'Pagina1') {
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
