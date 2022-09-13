build_attached_docs_url <- function(code) {
  return(
    paste0(
      "https://www.camara.leg.br/proposicoesWeb/prop_arvore_tramitacoes;jsessionid=node0pqpyfpzppq8j1fsvcdei9wjn064849336.node0?idProposicao=",
      code
    )
  )
}

download_attached_docs_pages <- function(code) {
  url <- build_attached_docs_url(code)
  return(read_html(url))
}

safely_and_slowly_download_attached_docs_pages <- download_attached_docs_pages %>%
  slowly(rate_delay(10)) %>% 
  safely()