read_sql <- function(email,password,url) {

googledrive::drive_auth(email = email)

codigo <- googledrive::drive_read_string(url,  encoding = "UTF8")
  #cat(codigo,sep = "\n")
  # temp_file <- tempfile()
  # writeLines(codigo, temp_file)
  # source(temp_file)
  # rm(temp_file) # limpa da memória o arquivo temp_file
  googlesheets4::gs4_auth(token = drive_token())
  return(codigo)

}
