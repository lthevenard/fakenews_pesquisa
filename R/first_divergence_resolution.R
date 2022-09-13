library(tidyverse)
library(readxl)

# Preparing the data for manual review of the divergent cases ----

comparison_table <- readRDS("DataOutput/comparison_table.rds")

diverge <- comparison_table %>% 
  filter(!converge) %>% 
  .$id

divergence_table <- read_xlsx(
  "DataOutput/type_selection_of_related_proposals_FLVR.xlsx"
) %>%
  mutate(Fake_news = as.numeric(Fake_news)) %>% 
  filter(id %in% diverge)

# Manual classification of the divergent cases ----

write_csv2(table, "DataClassification/divergence_table.csv")

divergence_table_resolved <- read_csv2("DataClassification/divergence_table_manual.csv") %>% 
  select(Fake_news...1, id) %>% 
  rename(resolution = "Fake_news...1")

# Adding resolution information to the comparison table ----

comparison_table <- comparison_table %>% 
  left_join(divergence_table_resolved, by = "id")

saveRDS(comparison_table, "DataOutput/comparison_table.rds")

# Consolidate new code selection ----

codes_excluded_by_resolution <- comparison_table %>% 
  filter(!converge & !as.logical(resolution)) %>% 
  .$id

codes <- readRDS("DataOutput/codes.rds")

### Careful: the following operations cannot be reversed ----
# codes$primary_codes <- codes$primary_codes[!(codes$primary_codes %in% codes_excluded_by_resolution)]
# codes[["primary_codes_excluded_by_resolution"]] <- codes_excluded_by_resolution
# attached_codes_remaining <- attached_docs_table %>% 
#   filter(!not_found) %>%
#   filter(attached_code %in% codes$attached_codes_filtered_by_type) %>% 
#   filter(primary_code %in% codes$primary_codes) %>% 
#   .$attached_code %>% 
#   unique()
# codes[["attached_codes_filtered_by_type_and_first_resolution"]] <- attached_codes_remaining
# codes[["current_code_selection"]] <- c(codes$primary_codes, attached_codes_remaining)
# codes[["codes_tibble"]] <- count_codes(codes)
# saveRDS(codes, "DataOutput/codes.rds")
### ----

primary_selection <- read_xlsx(
  "DataOutput/type_selection_of_related_proposals_FLVR.xlsx"
) %>% 
  filter(id %in% codes$primary_codes)

saveRDS(primary_selection, "DataOutput/final_primary_selection.rds")
write_csv2(primary_selection, "DataOutput/final_primary_selection.csv")
