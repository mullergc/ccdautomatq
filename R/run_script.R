source('R/dependencies.R')
source('R/get_query.R')
source('R/get_sql.R')
source('R/write_sheets.R')

# Define a function to log errors
log_error <- function(error_msg) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = "error_log.txt", append = TRUE)
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
    sql <- read_sql(email = creds$email, password = creds$email_pass, url = url_sql)

    # Prevent the SQL query from being printed in the terminal
    invisible(r <- get_query_auto(username = creds$username, password = creds$pass, dbname = creds$dbname, sql))

    write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname)

    # Success message
    cat("Function executed successfully\n")
  }, error = function(e) {
    # Handle errors and log them
    error_msg <- conditionMessage(e)
    log_error(error_msg)
    cat("Function encountered an error.See error_log please\n",num_chamado)
  })
}