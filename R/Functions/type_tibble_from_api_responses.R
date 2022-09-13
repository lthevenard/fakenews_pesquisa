type_tibble_from_api_responses <- function(api_responses) {
  codes <- names(api_responses)
  output <- vector('list', length(codes))
  for (i in seq_along(codes)) {
    output[[i]] <- tibble(
      id = codes[[i]],
      siglaTipo = api_responses[[i]]$dados$siglaTipo,
      descricaoTipo = api_responses[[i]]$dados$descricaoTipo
    )
  }
  bind_rows(output)
}
