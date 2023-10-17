read_sql <- function(email,password,url) {

drive_auth(email = email)
codigo <- drive_read_string(url,  encoding = "UTF8")
  #cat(codigo,sep = "\n")
  # temp_file <- tempfile()
  # writeLines(codigo, temp_file)
  # source(temp_file)
  # rm(temp_file) # limpa da memória o arquivo temp_file
  gs4_auth(token = drive_token())
  return(codigo)

}
