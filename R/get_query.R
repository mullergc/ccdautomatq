source('R/dependencies.R')
#
#

#

#library(ROracle)

#' Conecta e realiza à consulta à base, retorna a query
#' @param usernamedb credencial de usuário para acesso à base.
#' @param passwordb credencial de senha para acesso à base.
#' @param dbname A number.
#' @param query Código em sql, preferencialmente em string para realizar a consulta.
#' @return Dataframe query.
#' @examples
#' get_query_auto('user', 'senha','nome_base','SELECT * FROM TABLE')
get_query_auto <- function(usernamedb,passwordb,dbname,query){
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = usernamedb, password = passwordb, dbname = dbname)
  result <- ROracle::dbGetQuery(con, query)

  # Close the database connection
  return(result)
}


#' Conecta e realiza à consulta à base, retorna a query
#' Para setar a conexão com o SQL server, primeiramente necessário configurar o driver, com  a base, ver
#'https://turbofuture.com/computers/Connect-to-SQL-Server-from-R
#' @param query Código em sql, preferencialmente em string para realizar a consulta.
#' @return Dataframe query.
#' @examples
get_query_sqlserver <- function(query){
  conn = RODBC::odbcConnect("SQL Server")
  result <- sqlQuery(conn, query)
  return(result)
}



get_query_auto2 <- function(usernamedb,passwordb,dbname,query){
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = usernamedb, password = passwordb, dbname = dbname)
  result <- ROracle::oracleProc(con, query)

  # Close the database connection
  return(result)
}


get_query_window <- function(query){
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv,
                   username = rstudioapi::showPrompt(
                     title = "Username", message = "Username", default = ""
                   ),
                   password = rstudioapi::askForPassword(prompt = "Password"),
                   dbname = rstudioapi::showPrompt(
                     title = "Database", message = "Which database", default = "HCPAOPS.WORLD")
  )

  result <- ROracle::dbGetQuery(con, query)


  return(result)
}
