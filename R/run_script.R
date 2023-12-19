#library(ROracle)

#'@title log_error_gen(error_msg)
#'@description Função para erro por chamado
#' @param error_msg Mensagem de erro que será impressa caso haja erro no chamado
#' @param num_chamado Número do chamado passado automaticamente para a função, em caso de erro
log_error <- function(error_msg,num_chamado) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = paste0("error_log_",num_chamado,".txt"), append = TRUE)
}


#'@title log_error_gen(error_msg)
#'@description Função para erro geral
#' @param error_msg Mensagem de erro que será impressa caso haja erro geral
log_error_gen <- function(error_msg) {
  error_log <- paste0(Sys.time(), " - Error: ", error_msg, "\n")
  cat(error_log, file = paste0("error_log_general",".txt"), append = TRUE)
}


#'@title get_query(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE, sheetname = 'Pagina1')
#'@description Função para extração e escrever uma query apenas, sem filtrar
#' @param num_chamado número do chamado dessa query.
#' @param credspath caminho do arquivo em json com as credenciais de acesso para a base e email, deve ser armazenado localmente.
#' @param url_sheets url onde será escrita a tabela.
#' @param url_sql URl da localização do SQL
#' @param date_change Parâmetro se a data-hora será convertida para o fuso correto, preferencialmente para impedir inconsistências de datas, TRUE/FALSE,default é FALSE.
#' @param sheetname Parâmetro com o nome da aba, default é 'Pagina1'
get_query <- function(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE, sheetname = 'Pagina1') {
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
    cat("Query executada sem erros!\n")
  }, error = function(e) {
    # Handle errors and log them
    error_msg <- conditionMessage(e)
    log_error(error_msg,num_chamado)
    cat("Função encontrou erro, veja arquivo de log\n",num_chamado)
  })
}


#'@title get_write_query_sqlserver(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE,sheetname="Pagina1")
#'@description Função para extração e escrever uma query apenas, usando a conexão do sql server, que funcione, é necessario q o driver odbc esteja funcionante já
#'para instruções: https://turbofuture.com/computers/Connect-to-SQL-Server-from-R
#' @param num_chamado número do chamado dessa query.
#' @param credspath caminho do arquivo em json com as credenciais de acesso para a base e email, deve ser armazenado localmente.
#' @param url_sheets url onde será escrita a tabela.
#' @param url_sql URL da localização do SQL
#' @param date_change Parâmetro se a data-hora será convertida para o fuso correto, preferencialmente para impedir inconsistências de datas, TRUE/FALSE,default é FALSE.
#' @param sheetname Parâmetro com o nome da aba, default é 'Pagina1'
get_write_query_sqlserver <- function(num_chamado,url_sql, url_sheets, credspath, date_change = FALSE,sheetname="Pagina1"){
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
    r <-get_query_sqlserver(sql)
    write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname)

    # Success message
    cat("Query executada sem erros!\n")
  }, error = function(e) {
    # Handle errors and log them
    error_msg <- conditionMessage(e)
    log_error(error_msg,num_chamado)
    cat("Função encontrou erro, veja arquivo de log\n",num_chamado)
  })
}

#' @title gw_query_auto
#'@description Função para automatização total porém, com o objetivo de execução
#' @param url_pedidos url ou link onde estão as informações para o loop.
#' @param credspath caminho do arquivo em json com as credenciais de acesso para a base e email, deve ser armazenado localmente.
#' @param periodo periodicidade das queries, deve ser selecionada/ser igual a Diario/Mensal/Homolog/Semanal.
#' @param status status do pedido na tabela das queries,  deve ser selecionada/ser igual ATIVO,Cancelada,TESTE
#' @param date_change Parâmetro se a data-hora será convertida para o fuso correto, preferencialmente para impedir inconsistências de datas, TRUE/FALSE.
gw_query_auto <- function(url_pedidos, credspath,periodo='Diario',status='ATIVO', date_change = FALSE) {
  # Read the JSON file as text
  error_counter <- 0  # Initialize error counter to 0
  common_start_time <- Sys.time()  # Define a default start time
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
  df <- googlesheets4::read_sheet(ss = url_pedidos, sheet = "Querys_Automatizacao_Gestao") %>%
    filter(stringr::str_detect(Periodicidade, periodo)) %>%
    filter(Status == status)

  # Iterate through each row of the table
  for (i in 1:nrow(df)) {
    # Get the num_chamado, url_sheets, and url_sql for the current row
    num_chamado <- df$Qualitor[i]
    url_sheets <- df$url_output_sheet[i]
    url_sql <- df$url_sql[i]

    # Wrap the code that may cause an error in a tryCatch block
    tryCatch({
      start_time <- Sys.time()
      cat('\n', 'Inicio chamado', num_chamado, 'as', format(start_time, format = "%Y-%m-%d %H:%M:%S"), '\n')
      # Read the SQL query from the Google Drive file
      sql <- read_sql(email = email, url = url_sql)

      # Execute the SQL query and get the results
      r <- get_query_auto(usernamedb = username, passwordb = password, dbname = dbname, query = sql)
      # Write the results to the Google Sheets spreadsheet
      write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname = 'Pagina1')
      endtime <- Sys.time()
      time_taken <- as.integer(difftime(endtime, start_time, units = "mins"))
      cat('Fim do chamado', num_chamado, 'as', format(endtime, format = "%Y-%m-%d %H:%M:%S"), '\n', 'tempo de processamento:', time_taken, 'minutos')
    }, error = function(e) {
      # Handle the error
      error_msg <- conditionMessage(e)
      log_error(error_msg, num_chamado)
      cat('\n','ERRO OCORRIDO NO CHAMADO', num_chamado, 'as', format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), '\n')
      cat('\n','### VER ARQUIVO DE LOG ###','\n')

      error_counter <- error_counter + 1  # Increment the error counter
    })
  }

  total_end_time <- Sys.time()
  total_time_taken <- as.integer(difftime(total_end_time, common_start_time, units = "mins"))
  cat('\n', 'Tempo de execução Total:', total_time_taken, 'minutos')
}


#### FOR TESTING -----------------------------------------------------------------------------------------
#' @title gw_query_test(url_pedidos, credspath,periodo='Homolog',status='TESTE', date_change = FALSE)
#' @description Função para automatização total porém, com o objetivo de teste
#' @param url_pedidos url ou link onde estão as informações para o loop.
#' @param credspath caminho do arquivo em json com as credenciais de acesso para a base e email, deve ser armazenado localmente.
#' @param periodo periodicidade das queries, deve ser selecionada/ser igual a Diario/Mensal/Homolog/Semanal.
#' @param status status do pedido na tabela das queries,  deve ser selecionada/ser igual ATIVO,Cancelada,TESTE
#' @param date_change parâmetro se a data-hora será convertida para o fuso correto, preferencialmente para impedir inconsistências de datas .
gw_query_test <- function(url_pedidos, credspath,periodo='Homolog',status='TESTE', date_change = FALSE) {
  # Read the JSON file as text
  error_counter <- 0  # Initialize error counter to 0
  common_start_time <- Sys.time()  # Define a default start time
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
  df <- googlesheets4::read_sheet(ss = url_pedidos, sheet = "Querys_Automatizacao_Gestao") %>%
    filter(stringr::str_detect(Periodicidade, periodo)) %>%
    filter(Status == status)

  # Iterate through each row of the table
  for (i in 1:nrow(df)) {
    # Get the num_chamado, url_sheets, and url_sql for the current row
    num_chamado <- df$Qualitor[i]
    url_sheets <- df$url_output_sheet[i]
    url_sql <- df$url_sql[i]

    # Wrap the code that may cause an error in a tryCatch block
    tryCatch({
      start_time <- Sys.time()
      cat('\n', 'Inicio chamado', num_chamado, 'as', format(start_time, format = "%Y-%m-%d %H:%M:%S"), '\n')
      # Read the SQL query from the Google Drive file
      sql <- read_sql2(email = email, url = url_sql)

      # Execute the SQL query and get the results
      r <- get_query_auto(usernamedb = username, passwordb = password, dbname = dbname, query = sql)
      # Write the results to the Google Sheets spreadsheet
      write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname = 'Pagina1')
      endtime <- Sys.time()
      time_taken <- as.integer(difftime(endtime, start_time, units = "mins"))
      cat('Fim do chamado', num_chamado, 'as', format(endtime, format = "%Y-%m-%d %H:%M:%S"), '\n', 'tempo de processamento:', time_taken, 'minutos')
    }, error = function(e) {
      # Handle the error
      error_msg <- conditionMessage(e)
      log_error(error_msg, num_chamado)
      cat('\n','ERRO OCORRIDO NO CHAMADO', num_chamado, 'as', format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), '\n')
      cat('\n','### VER ARQUIVO DE LOG ###','\n')

      error_counter <- error_counter + 1  # Increment the error counter
    })
  }

  total_end_time <- Sys.time()
  total_time_taken <- as.integer(difftime(total_end_time, common_start_time, units = "mins"))
  cat('\n', 'Tempo de execução Total:', total_time_taken, 'minutos')
}


#'@title gfw_query(params)
#'@description Função para extração e escrever uma query apenas, com uma tabela de filtro, escrevendo a tabela resultante no sheets.
#' @param params lista com os parâmetros da execução da função, deve ser da seguinte forma:params <- list(num_chamado = 123,url_sql = "your_sql_url",url_sheets = "your_sheets_url",url_tabela_filtro = "url da tabela de filtro",date_change = FALSE,DTHR_MIN_PERIODO = 'Data hora minima de filtro',FILTR_COL='Nome da coluna de filtro',DTHR_MAX_PERIODO = 'Data hora máxima de filtro').
gfw_query <- function(params) {
  # Use tryCatch to handle errors
  tryCatch({
    # Extract parameters from the list
    num_chamado <- params$num_chamado
    url_sql <- params$url_sql
    url_sheets <- params$url_sheets
    credspath <- params$credspath
    date_change <- params$date_change
    sheetname <- params$sheetname
    url_tabela_filtro <- params$url_tabela_filtro
    DTHR_MIN <- params$DTHR_MIN_PERIODO
    DTHR_MAX <- params$DTHR_MAX_PERIODO
    FILTRO <- params$FILTR_COL

    # Read the JSON file as text
    creds_text <- readLines(credspath, warn = FALSE)

    # Parse the JSON text
    creds <- jsonlite::fromJSON(creds_text)
    # Extract the credentials
    email <- creds$email
    username <- creds$username
    password <- creds$pass
    dbname <- creds$dbname
    sql <- read_sql2(email = email, url = url_sql)
    df_filtro <- googlesheets4::read_sheet(ss = url_tabela_filtro)
    PRONTUARIO =  df_filtro %>% dplyr::select(matches(FILTRO))
    # Convert PRONTUARIO to a comma-separated string
    prontuario_list <- paste(PRONTUARIO$PRONTUARIO, collapse = ',')



    #param_values<-seq_along(PRONTUARIO)
    #sql <- read_sql2(email = email, url = url_sql2)
    sql2 <- stringr::str_replace_all(sql, c(":DTHR_MIN_REPLACE" = paste0("'",DTHR_MIN,"'"), ":DTHR_MAX_REPLACE" = paste0("'",DTHR_MAX,"'"),':PRONTS_REPLACE'=prontuario_list))
    r <- get_query_auto2(usernamedb = username, passwordb = password, dbname = dbname, query = sql2)
    write_query_sheet(df = r, url_destiny = url_sheets, date_change = date_change, sheetname)

    # Success message
    cat("Consulta executada com sucesso\n")
  }, error = function(e) {
    # Handle errors and log them
    error_msg <- conditionMessage(e)
    log_error(error_msg, num_chamado)
    cat("Erro encontrado, veja error_log \n", num_chamado)
  })
}
