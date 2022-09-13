load_related_proposal_data_by_year <- function (
    year, 
    codes, 
    spec_reference,
    path = "DataInput/proposicoes-"
) {
  file <- paste0(path, year, ".csv")
  return(
    file %>% 
      read_csv2(
        col_types = spec_reference$cols,
        show_col_types = FALSE
      ) %>% 
      filter(id %in% codes)
  )
}
