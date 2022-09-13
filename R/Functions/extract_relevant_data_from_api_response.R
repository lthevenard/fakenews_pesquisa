extract_relevant_data_from_api_response <- function(api_response) {
  tibble(
    id = api_response$dados$id,
    siglaTipo = api_response$dados$siglaTipo,
    descricaoTipo = api_response$dados$descricaoTipo,
    dataApresentacao = api_response$dados$dataApresentacao,
    ano = api_response$dados$ano,
    ementa = api_response$dados$ementa,
    urlInteiroTeor = api_response$dados$urlInteiroTeor
  )
}
