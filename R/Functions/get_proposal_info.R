prepare_api_request_urls <- function (
    codes, base_url = "https://dadosabertos.camara.leg.br/api/v2/proposicoes/"
) {
  paste0(base_url, codes)
}


get_proposal_info_from_api <- function(url) {
  GET(url) %>% 
    content(as = "parsed")
}

safely_and_slowly_get_proposal_info_from_api <- get_proposal_info_from_api %>% 
  slowly(rate = rate_delay(10)) %>% 
  safely()
