% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_query.R
\name{get_query_sqlserver}
\alias{get_query_sqlserver}
\title{get_query_sqlserver(query)}
\usage{
get_query_sqlserver(query)
}
\arguments{
\item{query}{Código em sql, preferencialmente em string para realizar a consulta.}
}
\value{
Dataframe query.
}
\description{
Função para extração de uma query apenas, usando a conexão do sql server, que funcione, é necessario q o driver odbc esteja funcionante já
Para setar a conexão com o SQL server, primeiramente necessário configurar o driver, com  a base, ver
https://turbofuture.com/computers/Connect-to-SQL-Server-from-R
para instruções: https://turbofuture.com/computers/Connect-to-SQL-Server-from-R#' Conecta e realiza à consulta à base, retorna a query
é possivel passar uma string com o SQL como "select * from dbase" ou uma string lida por um arquivo.
é importante ressaltar que nesse caso, no SQL o caminho da base deve estar completo
}
