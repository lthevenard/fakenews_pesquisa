# Libraries and code imports ----------------------------------------------

library(tidyverse)
library(rvest)
library(httr)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

codes <- readRDS("DataOutput/codes.rds")

universe_codes <- codes$universe

# PROPOSAL INFO ----

universe_info <- universe_codes %>% 
  prepare_api_request_urls() %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))

# Test results
length(universe_info) == universe_info %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

universe_results <- transpose(universe_info)$result

universe_basic_data <- universe_results %>% 
  map_dfr(extract_relevant_data_from_api_response) %>% 
  mutate(dataApresentacao = str_extract(dataApresentacao, "\\d{4}-\\d{2}-\\d{2}"),
         ano = str_extract(dataApresentacao, "\\d{4}") %>% as.numeric()) %>% 
  filter(ano > 2009)

write_csv2(universe_basic_data, "DataOutput/universe_basic_data.csv")

# AUTORES INFO ----

universe_info_autores <- universe_codes %>% 
  prepare_api_request_urls() %>% 
  paste0("/autores") %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))

# Test results
length(universe_info_autores) == universe_info_autores %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

universe_autores_results <- transpose(universe_info_autores)$result

# TRAMITACOES INFO ----

universe_info_tramitacoes <- universe_codes %>% 
  prepare_api_request_urls() %>% 
  paste0("/tramitacoes") %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))

# Test results
length(universe_info_tramitacoes) == universe_info_tramitacoes %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

universe_tramitacoes_results <- transpose(universe_info_tramitacoes)$result

# TABLE ENHANCED

universe_table <- universe_results %>% 
  map_dfr(extract_relevant_data_from_api_response)


universe_table_enhanced <- universe_table %>% 
  mutate(
    autores = map(universe_autores_results, extract_author_info_from_api),
    procedure = map(universe_tramitacoes_results, extract_procedure_info_from_api),
  )

universe_table_authors <- universe_table_enhanced %>% 
  select(id, autores) %>% 
  unnest(autores)

universe_table_procedure <- universe_table_enhanced %>% 
  select(id, procedure) %>% 
  unnest(procedure) %>% 
  arrange(id, sequencia)


author_info <- tibble(
  idAutor = unique(universe_table_authors$idAutor)
) %>%
  mutate(
    info = idAutor %>% 
      prepare_api_request_urls(
        base_url = "https://dadosabertos.camara.leg.br/api/v2/deputados/"
      ) %>% 
      map(slowly(get_proposal_info_from_api, rate = rate_delay(6)))
  )


author_info <- author_info %>% 
  mutate(siglaPartido = map_chr(info, extract_sigla_partido),
         idPartido = map_chr(info, extract_cod_partido),
         siglaUf = map_chr(info, extract_uf))

saveRDS(author_info, "DataOutput/author_info_universe.rds")

universe_table_authors <- universe_table_authors %>% 
  left_join(
    author_info %>% 
      select(all_of(c("idAutor", "siglaPartido", "idPartido", "siglaUf"))),
    by = "idAutor"
  )

body_info <- tibble(
  idOrgao = unique(universe_table_procedure$idOrgao),
  siglaOrgao = unique(universe_table_procedure$siglaOrgao)
) %>% 
  mutate(
    info = idOrgao %>% 
      prepare_api_request_urls(
        base_url = "https://dadosabertos.camara.leg.br/api/v2/orgaos/"
      ) %>% 
      map(slowly(get_proposal_info_from_api, rate = rate_delay(6)))
  )

saveRDS(body_info, "DataOutput/body_info_universe.rds")

party_info <- tibble(
  idPartido = unique(universe_table_authors$idPartido)
) %>%
  mutate(
    info = idPartido %>% 
      prepare_api_request_urls(
        base_url = "https://dadosabertos.camara.leg.br/api/v2/partidos/"
      ) %>% 
      map(slowly(get_proposal_info_from_api, rate = rate_delay(6)))
  )

universe_table_enhanced <- universe_table_enhanced %>% 
  mutate(autores = map(autores, ~left_join(., select(author_info, -all_of(c("info", "siglaPartido", "idPartido"))), by = "idAutor")))

saveRDS(universe_table_enhanced, "DataOutput/universe_table_enhanced.rds")

universe_table_procedure <- universe_table_enhanced %>% 
  select(id, procedure) %>% 
  unnest(procedure)

party_engagement <- universe_table_authors %>% 
  filter(!is.na(siglaPartido)) %>% 
  count(id, siglaPartido) %>% 
  count(siglaPartido)

