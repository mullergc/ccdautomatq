#library(ROracle)


# Define a function to log errors
log_error <- function(error_msg,num_chamado) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = paste0("error_log_",num_chamado,".txt"), append = TRUE)
}


# Define a function to log errors
log_error_gen <- function(error_msg) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = paste0("error_log_general",".txt"), append = TRUE)
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

gw_query_auto <- function(url_pedidos, credspath,periodo='Diário',status='ATIVO', date_change = FALSE) {
  # Read the JSON file as text
  creds_text <- readLines(credspath, warn = FALSE)

  # Parse the JSON text
  creds <- jsonlite::fromJSON(creds_text)
  # Extract the credentials
  email <- creds$email
  username <- creds$username
  password <- creds$pass
  dbname <- creds$dbname

  # Authenticate to Google Drive
  googledrive::drive_auth(email = email)

  # Authenticate to Google Sheets
  googlesheets4::gs4_auth(token = googledrive::drive_token())

  # Read the Querys_Automatizacao_Gestao (Teti) sheet from the Google Sheets spreadsheet
  df <- googlesheets4::read_sheet(ss=url_pedidos,sheet="Querys_Automatizacao_Gestao") %>%
        filter(stringr::str_detect(Periodicidade,periodo)) %>%
        filter(Status==status)

  # Iterate through each row of the table
  for (i in 1:nrow(df)) {
    start_time <- Sys.time()

    # Get the num_chamado, url_sheets, and url_sql for the current row
    num_chamado <- df$Qualitor[i]
    url_sheets <- df$url_output_sheet[i]
    url_sql <- df$url_sql[i]

    # Wrap the code that may cause an error in a tryCatch block
    tryCatch({
      # Read the SQL query from the Google Drive file
      sql <- read_sql(email = email, url = url_sql)

      # Execute the SQL query and get the results
      r <- get_query_auto(usernamedb = username, passwordb = password, dbname = dbname, query = sql)
     # urlsheets_teste = 'https://docs.google.com/spreadsheets/d/1Dj1sbvi-TAN6-llwRdvlMGO5sr5bteeWSEQU9VU65lA/edit?usp=sharing'
      # Write the results to the Google Sheets spreadsheet
      write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname='Pagina1')
    }, error = function(e) {
      # Handle the error
      error_msg <- conditionMessage(e)
      log_error(error_msg, num_chamado)
    })

  }

  # Success message
  cat("Function executed successfully\n")
}


#### FOR TESTING -----------------------------------------------------------------------------------------
gw_query_test <- function(url_pedidos, credspath,periodo='Diário',status='TESTE', date_change = FALSE) {
  # Read the JSON file as text
  creds_text <- readLines(credspath, warn = FALSE)

  # Parse the JSON text
  creds <- jsonlite::fromJSON(creds_text)
  # Extract the credentials
  email <- creds$email
  username <- creds$username
  password <- creds$pass
  dbname <- creds$dbname

  # Authenticate to Google Drive
  googledrive::drive_auth(email = email)

  # Authenticate to Google Sheets
  googlesheets4::gs4_auth(token = googledrive::drive_token())

  # Read the Querys_Automatizacao_Gestao (Teti) sheet from the Google Sheets spreadsheet
  df <- googlesheets4::read_sheet(ss=url_pedidos,sheet="Querys_Automatizacao_Gestao") %>%
    filter(stringr::str_detect(Periodicidade,periodo)) %>%
    filter(Status==status)
  common_start_time <- Sys.time()

  # Iterate through each row of the table
  for (i in 1:nrow(df)) {

    # Get the num_chamado, url_sheets, and url_sql for the current row
    num_chamado <- df$Qualitor[i]
    url_sheets <- df$url_output_sheet[i]
    url_sql <- df$url_sql[i]
    start_time <- Sys.time()

    # Wrap the code that may cause an error in a tryCatch block
    tryCatch({
      # Read the SQL query from the Google Drive file
      sql <- read_sql(email = email, url = url_sql)

      # Execute the SQL query and get the results
      r <- get_query_auto(usernamedb = username, passwordb = password, dbname = dbname, query = sql)
      urlsheets_teste = 'https://docs.google.com/spreadsheets/d/1Dj1sbvi-TAN6-llwRdvlMGO5sr5bteeWSEQU9VU65lA/edit?usp=sharing'
      # Write the results to the Google Sheets spreadsheet
      write_query_sheet(df = r, url_destiny = urlsheets_teste, date_change = date_change, sheetname='Pagina1')
    }, error = function(e) {
      # Handle the error
      error_msg <- conditionMessage(e)
      log_error(error_msg, num_chamado)
      end_time <- Sys.time()

      # Calculate the time taken for the loop iteration
      time_taken <- as.integer(difftime(end_time, start_time, units = "mins"))

      # Print the start and end times and the time taken for the current iteration
      print(paste("Chamado", num_chamado, "Hora de Início", start_time, "Hora de Fim", end_time, "Tempo query", time_taken, "minutos"))

    })
  }
  # Success message
  total_end_time <- Sys.time()
  total_time_taken <- as.integer(difftime(total_end_time, common_start_time, units = "mins"))

  cat("Function executed successfully\n")
  cat("Total execution time:", total_time_taken, "minutos")
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
