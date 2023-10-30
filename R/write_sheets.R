#require('dplyr')
#' Função para escrever a tabela recuperada pela query em um documento do sheets.
#'#'@param df dataframe correspondente à consulta realizada a partir do sql.
#' @param url_destiny url onde será escrito a tabela.
#' @param sheetname nome da aba em que será escrita a tabela.
#' @param date_change Parâmetro que realiza a mudança de formato e fuso da data, TRUE/FALSE.
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
