#'@title  read_sql
#'@description Função específica para ler o código em sql e  retorná-lo em formato de string
#' @param email email de acesso, deve estar dentro da credencial em json
#' @param url url onde está o sql, deve estar compartilhado com o email de acesso.
#' @return retorna o código em sql, em formato de string para utilização na função que faz a consulta à query
#' @examples
#' url_pedidos='https://docs.google.com/spreadsheets/TESTE'
#' credspath = 'C:/Users/Usuario/Desktop/creds.json'
#' gw_query_test(url_pedidos, credspath,periodo='Homolog',status='TESTE', date_change = TRUE)
read_sql <- function(email,url) {
codigo <- googledrive::drive_read_string(url,  encoding = "UTF8")
googlesheets4::gs4_auth(token = googledrive::drive_token())
return(codigo)
}

#'@title read_sql2
#'@description Função para leitura do sql, com maior estabilidade e independente do encoding
#' @param email email de acesso, deve estar dentro da credencial em json
#' @param url url onde está o sql, deve estar compartilhado com o email de acesso.
#' @return retorna o código em sql, em formato de string para utilização na função que faz a consulta à query
#' @examples
#' url_pedidos='https://docs.google.com/spreadsheets/TESTE'
#' credspath = 'C:/Users/Usuario/Desktop/creds.json'
#' gw_query_test(url_pedidos, credspath,periodo='Homolog',status='TESTE', date_change = TRUE)
read_sql2 <- function(email,url) {
  googledrive::drive_auth(email=email,use_oob = FALSE)

  ## identify this folder on Drive
  ## let googledrive know this is a file ID or URL, as opposed to file name
  id_sql <- as_id(url)
  temp <- tempfile(fileext = '.sql')
  dl <- googledrive::drive_download(id_sql, path = temp, overwrite = TRUE)
  sql_content <- readLines(temp,encoding='UTF-8')
  sql_query <- paste(sql_content, collapse = "\n")
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  return(sql_query)
}
