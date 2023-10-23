
read_sql <- function(email,url) {

codigo <- googledrive::drive_read_string(url,  encoding = "UTF8")
googlesheets4::gs4_auth(token = googledrive::drive_token())
return(codigo)

}
