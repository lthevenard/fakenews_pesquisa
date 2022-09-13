build_general_page_url <- function (
    code, 
    base_url = "https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao="
) {
  paste0(base_url, code)
}

get_type_from_general_page <- function(url, css = ".tipoProposicao") {
  read_html(url) %>% 
    html_nodes(css) %>% 
    html_text() %>% 
    str_flatten()
}

safely_and_slowly_get_type_from_general_page <- get_type_from_general_page %>% 
  slowly(rate = rate_delay(10)) %>% 
  safely()
