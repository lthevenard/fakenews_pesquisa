# Libraries and code imports ----------------------------------------------

library(tidyverse)
library(readxl)


# Utility functions -------------------------------------------------------

clean_select <- function (
    data, 
    revisor, 
    id_column = id, 
    review_column = Fake_news
) {
  return(
    data %>% 
      mutate(review = {{review_column}},
             revisor = revisor) %>% 
      select({{id_column}}, review, revisor)
  )
}


convergence_by_revisor <- function (
    data, 
    revisor_value, 
    revisor_column = revisor,
    convergence_column = converge
) {
  filtered <- data %>% 
    filter({{revisor_column}} == revisor_value) %>% 
    select({{convergence_column}})
  return(sum(filtered[[1]]) / nrow(filtered))
}

# Data --------------------------------------------------------------------

lucas <- read_csv2("DataOutput/blind_review_lucas_done.csv") %>% 
  clean_select("lucas")

guilherme <- read_xlsx("DataOutput/blind_review_guilherme_done.xlsx") %>% 
  clean_select("guilherme")

felipe <- read_xlsx("DataOutput/blind_review_felipe_done.xlsx") %>% 
  clean_select("felipe")

review <- bind_rows(lucas, guilherme, felipe)

review %>% 
  filter(is.na(id) | is.na(review))

original_classifications <- read_xlsx(
  "DataOutput/type_selection_of_related_proposals_FLVR.xlsx"
) %>% 
  filter(id %in% review$id) %>% 
  mutate(classification = as.numeric(Fake_news)) %>% 
  select(id, classification)

comparison_table <- original_classifications %>% 
  right_join(review, by = "id") %>% 
  mutate(converge = (classification - review) == 0)


model <- glm(classification ~ review + revisor, data = comparison_table, family = binomial)

convergence_lucas <- comparison_table %>% 
  convergence_by_revisor("lucas")

convergence_felipe <- comparison_table %>% 
  convergence_by_revisor("felipe")

convergence_guilherme <- comparison_table %>% 
  convergence_by_revisor("guilherme")

saveRDS(comparison_table, "DataOutput/comparison_table.rds")
saveRDS(model, "DataOutput/model_blind_review_primary.rds")
