build_api_url <- function(year, theme, format = ".csv") {
  return(
    paste0(
      "http://dadosabertos.camara.leg.br/arquivos/",
      theme, "/",
      format, "/",
      theme, "-",
      year, ".",
      format
    )
  )   
}
