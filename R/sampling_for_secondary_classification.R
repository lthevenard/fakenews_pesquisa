library(tidyverse)
library(rvest)
library(httr)

list.files("R/Functions", pattern = "\\.R$", full.names = TRUE) %>% 
  walk(source)

codes <- readRDS("DataOutput/codes.rds")

second_classification_codes <- codes$attached_codes_filtered_by_type_and_first_resolution

second_classification_info <- second_classification_codes %>% 
  prepare_api_request_urls() %>% 
  map(safely(slowly(get_proposal_info_from_api, rate = rate_delay(6))))

# Test results
length(second_classification_info) == second_classification_info %>% 
  transpose() %>% 
  .$error %>% 
  map_lgl(is.null) %>% 
  sum()


second_classification_table <- transpose(second_classification_info)$result %>% 
  map_dfr(extract_relevant_data_from_api_response)


second_classification_table <- second_classification_table %>% 
  mutate(dataApresentacao = str_extract(dataApresentacao, "\\d{4}.\\d{2}.\\d{2}"))

researchers <- c("Felipe Roquete", "Felipe Godoy", "Guilherme Aleixo", "Lucas Thevenard")

assigned_classification_table <- second_classification_table %>% 
  randomly_assign_researchers_twice(researchers, seed = 1234)

saveRDS(assigned_classification_table, "DataOutput/assigned_classification_table.rds")

felipe_roquete <- assigned_classification_table %>% 
  filter(primary_classification == "Felipe Roquete" | reviewer == "Felipe Roquete") %>% 
  select(-all_of(c("primary_classification", "reviewer")))

write_csv2(felipe_roquete, "DataOutput/sec_felipe_roquete.csv")

felipe_godoy <- assigned_classification_table %>% 
  filter(primary_classification == "Felipe Godoy" | reviewer == "Felipe Godoy") %>% 
  select(-all_of(c("primary_classification", "reviewer")))

write_csv2(felipe_godoy, "DataOutput/sec_felipe_godoy.csv")

guilherme <- assigned_classification_table %>% 
  filter(primary_classification == "Guilherme Aleixo" | reviewer == "Guilherme Aleixo") %>% 
  select(-all_of(c("primary_classification", "reviewer")))

write_csv2(guilherme, "DataOutput/sec_guilherme.csv")

lucas <- assigned_classification_table %>% 
  filter(primary_classification == "Lucas Thevenard" | reviewer == "Lucas Thevenard") %>% 
  select(-all_of(c("primary_classification", "reviewer")))

write_csv2(lucas, "DataOutput/sec_lucas.csv")


