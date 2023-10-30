#'Função específica para ler o código em sql e  retorná-lo em formato de string
#' @param email email de acesso, deve estar dentro da credencial em json
#' @param url url onde está o sql, deve estar compartilhado com o email de acesso.
#' @return retorna o código em sql, em formato de string para utilização na função que faz a consulta à query
#' @examples
#' NOT RUN
#' url_pedidos='https://docs.google.com/spreadsheets/TESTE'
#' credspath = 'C:/Users/Usuario/Desktop/creds.json'
#' gw_query_test(url_pedidos, credspath,periodo='Homolog',status='TESTE', date_change = TRUE)
read_sql <- function(email,url) {
codigo <- googledrive::drive_read_string(url,  encoding = "UTF8")
googlesheets4::gs4_auth(token = googledrive::drive_token())
return(codigo)

}
