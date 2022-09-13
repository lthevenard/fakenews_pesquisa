library(tidyverse)
library(rvest)
library(httr)

list.files("R/Functions", pattern = "\\.R$", full.names = TRUE) %>% 
  walk(source)

codes <- readRDS("DataOutput/codes.rds")

primary_classification_codes <- codes$primary_codes

primary_classification_info <- primary_classification_codes %>% 
  prepare_api_request_urls() %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))


primary_classification_info_autores <- primary_classification_codes %>% 
  prepare_api_request_urls() %>% 
  paste0("/autores") %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))


primary_classification_info_tramitacoes <- primary_classification_codes %>% 
  prepare_api_request_urls() %>% 
  paste0("/tramitacoes") %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))


# Test results
length(primary_classification_info) == primary_classification_info %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()


length(primary_classification_info_autores) == primary_classification_info_autores %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()


length(primary_classification_info_tramitacoes) == primary_classification_info_tramitacoes %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

primary_classification_results <- transpose(primary_classification_info)$result
primary_classification_autores_results <- transpose(primary_classification_info_autores)$result
primary_classification_tramitacoes_results <- transpose(primary_classification_info_tramitacoes)$result


primary_classification_table <- primary_classification_results %>% 
  map_dfr(extract_relevant_data_from_api_response)


primary_classification_table_enhanced <- primary_classification_table %>% 
  mutate(
    autores = map(primary_classification_autores_results, extract_author_info_from_api),
    procedure = map(primary_classification_tramitacoes_results, extract_procedure_info_from_api),
  )


primary_classification_table_procedure <- primary_classification_table_enhanced %>% 
  select(id, procedure) %>% 
  unnest(procedure) %>% 
  arrange(id, sequencia)

author_info <- tibble(
  idAutor = unique(primary_classification_table_authors$idAutor)
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

saveRDS(author_info, "DataOutput/author_info.rds")


body_info <- tibble(
  idOrgao = unique(primary_classification_table_procedure$idOrgao),
  siglaOrgao = unique(primary_classification_table_procedure$siglaOrgao)
) %>% 
  mutate(
    info = idOrgao %>% 
      prepare_api_request_urls(
        base_url = "https://dadosabertos.camara.leg.br/api/v2/orgaos/"
      ) %>% 
      map(slowly(get_proposal_info_from_api, rate = rate_delay(6)))
  )

saveRDS(body_info, "DataOutput/abody_info.rds")


party_info <- tibble(
  idPartido = unique(primary_classification_table_authors$idPartido)
) %>%
  mutate(
    info = idPartido %>% 
      prepare_api_request_urls(
        base_url = "https://dadosabertos.camara.leg.br/api/v2/partidos/"
      ) %>% 
      map(slowly(get_proposal_info_from_api, rate = rate_delay(6)))
  )

primary_classification_table_enhanced <- primary_classification_table_enhanced %>% 
  mutate(autores = map(autores, ~left_join(., select(author_info, -all_of(c("info", "siglaPartido", "idPartido"))), by = "idAutor")))

saveRDS(primary_classification_table_enhanced, "DataOutput/primary_classification_table_enhanced.rds")

primary_classification_table_authors <- primary_classification_table_enhanced %>% 
  select(id, autores) %>% 
  unnest(autores)

primary_classification_table_procedure <- primary_classification_table_enhanced %>% 
  select(id, procedure) %>% 
  unnest(procedure)

party_engagement <- primary_classification_table_authors %>% 
  filter(!is.na(siglaPartido)) %>% 
  count(id, siglaPartido) %>% 
  count(siglaPartido)



