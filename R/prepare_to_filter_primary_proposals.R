# Libraries and code imports ----------------------------------------------

library(tidyverse)
library(lubridate)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

# Load Data ---------------------------------------------------------------

codes <- readRDS("DataOutput/codes.rds")

search_results <- list.files(
  path = "DataInput", 
  pattern = "^busca_", 
  full.names = TRUE
) %>% 
  map_dfr(read_csv2) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(link)) %>% 
  mutate(date = dmy(apresentac),
         year = str_extract(apresentac, "\\d{4}"),
         code = str_extract(link, "\\d+$"))

related_proposals <- map_dfr(
  1992:2022,
  load_related_proposal_data_by_year,
  codes = search_results$code,
  spec_reference = readRDS("DataInput/proposicoes-colspec.rds")
)

saveRDS(related_proposals, "DataOutput/all_related_proposals.rds")

# Inspect search codes that are not in the api database
search_results %>% 
  filter(!is.na(link)) %>% 
  filter(!(code %in% related_proposals$id)) %>% 
  mutate(link = paste0(
    "https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=", 
    code
  ))


# Type classification for exclusion ---------------------------------------

related_proposals %>%
  filter(!duplicated(siglaTipo)) %>%
  select(siglaTipo, descricaoTipo) %>% 
  full_join(count(related_proposals, siglaTipo), by = "siglaTipo") %>% 
  arrange(desc(n)) %>% 
  write_csv2("DataClassification/classificar_tipos.csv")


# Filtering types ---------------------------------------------------------

related_proposals <- readRDS("DataOutput/all_related_proposals.rds")

remaining_types <- 
  read_csv2("DataClassification/classificar_tipos_manual.csv") %>% 
  filter(as.logical(manter)) %>% 
  .$siglaTipo

related_proposals_filtered <- related_proposals %>% 
  filter(siglaTipo %in% remaining_types)

saveRDS(
  related_proposals_filtered,
  "DataOutput/type_selection_of_related_proposals.rds"
)

write_csv2(
  related_proposals_filtered,
  "DataOutput/type_selection_of_related_proposals.csv"
)


