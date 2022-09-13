library(tidyverse)
library(httr)
library(rvest)
library(readxl)

senado_search_pages <- list.files(
  path = "DataInput/busca_fake_news_senado/",
  pattern = "\\.html", 
  full.names = TRUE
) %>% map(read_html)

css <- list(
  title = ".sf-busca-resultados-item a",
  description = ".sf-busca-resultados-item-ementa"
)

extract_text_from_css <- function(page, css) {
  page %>% 
    html_nodes(css) %>% 
    html_text() %>% 
    str_trim()
}

extract_href_from_css <- function(page, css) {
  page %>% 
    html_nodes(css) %>% 
    html_attr("href") %>% 
    str_trim()
}

senado_results <- vector('list', 3)

for (i in seq_along(senado_search_pages)) {
  senado_results[[i]] <- tibble(
    title = extract_text_from_css(senado_search_pages[[i]], css$title),
    link = extract_href_from_css(senado_search_pages[[i]], css$title)
  )
}

senado_results <- bind_rows(senado_results) %>% 
  filter(!duplicated(link)) %>% 
  mutate(year = str_extract(title, "(?<=/)\\d{4}") %>% as.numeric(),
         type = str_extract(title, "^[A-Z]+")) %>% 
  relocate(type, .before = link) %>% 
  relocate(year, .before = type)

exclude_types <- c(
  "PCS", "PRM", "REQ", "RQN", "RQS"
)

senado_results %>% 
  filter(!(type %in% exclude_types)) %>% 
  write_csv2("DataOutput/senado_results.csv")


felipe_senado <- read_xlsx("DataOutput/senado_results_flvr_done.xlsx")
lucas_senado <- read_csv2("DataOutput/senado_results_lt_done.csv")

lucas_senado %>% 
  filter(Fake_news != 99) %>% 
  write_csv2("DataOutput/senado_results_final.csv")
  

