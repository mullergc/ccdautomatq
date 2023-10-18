write_query_sheet <- function(df, url_destiny, sheetname = 'Página1', date_change = FALSE) {
  if (!date_change) {
    df_final <- df
  } else {
    date_columns <- grep("DT|DTHR|DATA", names(df), value = TRUE)
    df_final <- df %>%
      dplyr::mutate(across(all_of(date_columns), ~ parse_date_time(.x, c("ymd HMS",'ymd','dmy','dmy HMS'),tz='UTC')))
  }

  googlesheets4::write_sheet(df_final, ss = url_destiny, sheet = sheetname)
}
