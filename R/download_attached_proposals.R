# Libraries and code imports ----------------------------------------------

library(tidyverse)
library(rvest)
library(httr)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

# Get the selected primary codes ------------------------------------------

double_review_codes <- readRDS("DataOutput/comparison_table.rds") %>% 
  filter(classification | review) %>% 
  .$id

single_review_codes <- read_xlsx(
  "DataOutput/type_selection_of_related_proposals_FLVR.xlsx"
) %>%
  mutate(Fake_news = as.numeric(Fake_news)) %>% 
  filter(Fake_news != 99 & !(id %in% double_review_codes)) %>% 
  filter(as.logical(Fake_news)) %>% 
  .$id

selected_codes <- c(single_review_codes, double_review_codes)  
  

# Download attached docs pages --------------------------------------------

attached_docs_pages <- map(
  selected_codes, 
  safely_and_slowly_download_attached_docs_pages
)

# Test if every page was downloaded correctly
length(attached_docs_pages) == attached_docs_pages %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

# Save results
attached_docs_pages_results <- attached_docs_pages %>% 
  transpose() %>% 
  .$result

walk2(
  attached_docs_pages_results,
  selected_codes,
  write_html_page_by_code,
  path = "DataOutput/paginas/arvores_apensados"
)


# Build table relating attached docs to the selected primary codes --------

attached_docs_list <- map2(
  attached_docs_pages_results,
  selected_codes,
  safely_build_attached_documents_relational_table
)

# Test if every page was downloaded correctly
length(attached_docs_list) == attached_docs_list %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

attached_docs_table <- attached_docs_list %>% 
  transpose() %>% 
  .$result %>% 
  bind_rows()


# Find attached docs codes in the API -------------------------------------

attached_docs_codes <- attached_docs_table %>% 
  filter(!duplicated(attached_code)) %>% 
  filter(!(attached_code %in% selected_codes)) %>% 
  .$attached_code

codes <- readRDS("DataOutput/codes.rds")

# Careful! The following process cannot be reversed ###

# codes[["excluded_codes"]] <- codes$primary_codes[!(codes$primary_codes %in% selected_codes)]
# codes[["excluded_codes"]] <- as.numeric(codes[["excluded_codes"]])
# codes$primary_codes <- selected_codes
# codes$new_codes <- attached_docs_codes
# codes$full_codes <- c(selected_codes, attached_docs_codes)
# codes[["codes_tibble"]] <- count_codes(codes)
# saveRDS(codes, "DataOutput/codes.rds")

###

options(timeout = 600)

api_responses_for_attached_docs <- attached_docs_codes %>% 
  prepare_api_request_urls() %>% 
  map(safely_and_slowly_get_proposal_info_from_api)

length(api_responses_for_attached_docs) == api_responses_for_attached_docs %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

api_responses_for_attached_docs <- api_responses_for_attached_docs %>% 
  transpose() %>% 
  .$result %>% 
  setNames(attached_docs_codes)


attached_docs_type_tibble <- type_tibble_from_api_responses(
  api_responses_for_attached_docs
)

# 58 codes were not found by the API
type_NAs <- attached_docs_type_tibble %>% filter(is.na(descricaoTipo)) %>% .$id

attached_docs_table <- attached_docs_table %>% 
  mutate(not_found = (as.character(attached_code) %in% type_NAs))

saveRDS(attached_docs_table, "DataOutput/attached_docs_relational_table.rds")


# Filter attached docs using the previous type selection ------------------

types <- attached_docs_type_tibble %>% 
  filter(!is.na(siglaTipo)) %>% 
  .$siglaTipo %>% 
  unique()
  
previously_classified <- read_csv2("DataClassification/classificar_tipos.csv") %>% 
  .$siglaTipo

new_types <- types[!(types %in% previously_classified)]

attached_docs_type_tibble %>% 
  filter(!is.na(siglaTipo) & siglaTipo %in% new_types) %>% 
  count(siglaTipo, descricaoTipo, sort = TRUE) %>% 
  mutate(manter = "", justificativa = "") %>% 
  write_csv2("DataClassification/classificar_tipos_2.csv")

type_classification_keep <- bind_rows(
  read_csv2("DataClassification/classificar_tipos_manual.csv"),
  read_csv2("DataClassification/classificar_tipos_2_manual.csv")
) %>% 
  filter(as.logical(manter)) %>% 
  .$siglaTipo


new_and_filtered_codes <- attached_docs_type_tibble %>% 
  filter(!is.na(siglaTipo) & siglaTipo %in% type_classification_keep) %>% 
  mutate(id = as.numeric(id)) %>% 
  .$id
  
codes_revised <- list(
  primary_codes = codes$primary_codes,
  excluded_primary_codes = codes$excluded_codes,
  attached_codes_all = codes$new_codes,
  attached_codes_filtered_by_type = new_and_filtered_codes,
  current_code_selection = c(codes$primary_codes, new_and_filtered_codes)
)

codes_revised[["codes_tibble"]] <- count_codes(codes_revised)
saveRDS(codes_revised, "DataOutput/codes.rds")

# Trying to find the codes that received a 404 response from the API

NAs_descricaoTipo <- type_NAs %>% 
  build_general_page_url() %>% 
  map(safely_and_slowly_get_type_from_general_page)

# Test if every page was downloaded correctly
length(NAs_descricaoTipo) == NAs_descricaoTipo %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()

NAs_descricaoTipo_results <- NAs_descricaoTipo %>% 
  transpose() %>% 
  .$result %>% unlist()

# They were all "Declaração de Voto", and that is an excluded type anyway



