possibly_extract_or_default <- function(obj, name, default_value) {
  if (name %in%  names(obj) & !is.null(obj[[name]])) {
    return(obj[[name]])
  } else {
    return(default_value)
  }
}

read_author_entry_from_api <- function(single_entry) {
  tibble(
    autor = possibly_extract_or_default(single_entry, "nome", NA),
    idAutor = str_extract(possibly_extract_or_default(single_entry, "uri", NA), "\\d+$"),
    tipo = possibly_extract_or_default(single_entry, "tipo", NA),
    ordemAssinatura = possibly_extract_or_default(single_entry, "ordemAssinatura", NA)
  )
}

extract_author_info_from_api <- function(api_response) {
  
  dados <- api_response$dados
  authors <- map_dfr(dados, read_author_entry_from_api)
  
  return(authors)
}

read_procedure_from_api <- function(single_entry) {
  tibble(
    sequencia = possibly_extract_or_default(single_entry, "sequencia", NA),
    dataHora = str_extract(possibly_extract_or_default(single_entry, "dataHora", NA), "\\d{4}.\\d{2}.\\d{2}"),
    siglaOrgao = possibly_extract_or_default(single_entry, "siglaOrgao", NA),
    idOrgao = str_extract(possibly_extract_or_default(single_entry, "uriOrgao", NA), "\\d+$"),
    descricaoTramitacao = possibly_extract_or_default(single_entry, "descricaoTramitacao", NA),
    despacho = possibly_extract_or_default(single_entry, "despacho", NA)
  )
}

extract_procedure_info_from_api <- function(api_response) {
  
  dados <- api_response$dados
  authors <- map_dfr(dados, read_procedure_from_api)
  
  return(authors)
}

extract_sigla_partido <- function(entry) {
  output <- entry$dados$ultimoStatus$siglaPartido
  if (is.null(output)) {
    return(NA)
  } else {
    return(output)
  }
}

extract_cod_partido <- function(entry) {
  output <- entry$dados$ultimoStatus$uriPartido
  if (is.null(output)) {
    return(NA)
  } else {
    return(str_extract(output, "\\d+$"))
  }
}

extract_uf <- function(entry) {
  output <- entry$dados$ultimoStatus$siglaUf
  if (is.null(output)) {
    return(NA)
  } else {
    return(output)
  }
}

