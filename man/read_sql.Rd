% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sql.R
\name{read_sql}
\alias{read_sql}
\title{read_sql}
\usage{
read_sql(email, url)
}
\arguments{
\item{email}{email de acesso, deve estar dentro da credencial em json}

\item{url}{url onde está o sql, deve estar compartilhado com o email de acesso.}
}
\value{
retorna o código em sql, em formato de string para utilização na função que faz a consulta à query
}
\description{
Função específica para ler o código em sql e  retorná-lo em formato de string
}
