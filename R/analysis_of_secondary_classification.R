library(tidyverse)
library(readxl)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

sec_classified <- bind_rows(
  read_xlsx("DataOutput/sec_felipe_roquete_done.xlsx") %>% 
    mutate(researcher = "Felipe Roquete"),
  read_xlsx("DataOutput/sec_felipe_godoy_done.xlsx") %>% 
    mutate(researcher = "Felipe Godoy"),
  read_xlsx("DataOutput/sec_guilherme_done.xlsx") %>% 
    mutate(researcher = "Guilherme"),
  read_csv2("DataOutput/sec_lucas_done.csv") %>% 
    mutate(dataApresentacao = as.character(dataApresentacao), # for vector type convergence
           researcher = "Lucas")
) %>% 
  mutate(division = duplicated(id))

sec_classified_group1 <- sec_classified %>% filter(division)
sec_classified_group2 <- sec_classified %>% filter(!division)

results <- sec_classified_group1 %>% 
  mutate(classification_group1 = Fake_news,
         researcher_group1 = researcher) %>% 
  select(id, classification_group1, researcher_group1) %>% 
  left_join(
    sec_classified_group2 %>% 
      mutate(classification_group2 = Fake_news,
             researcher_group2 = researcher) %>% 
      select(id, classification_group2, researcher_group2),
    by = "id"
  ) %>% 
  mutate(converge = classification_group1 - classification_group2 == 0)

by_researcher = list(
  roquete = results %>% 
    filter(researcher_group1 == "Felipe Roquete" | researcher_group2 == "Felipe Roquete"),
  godoy = results %>% 
    filter(researcher_group1 == "Felipe Godoy" | researcher_group2 == "Felipe Godoy"),
  guilherme = results %>% 
    filter(researcher_group1 == "Guilherme" | researcher_group2 == "Guilherme"),
  lucas = results %>% 
    filter(researcher_group1 == "Lucas" | researcher_group2 == "Lucas")
)

convergence_roquete <- sum(by_researcher$roquete$converge) / nrow(by_researcher$roquete)
convergence_godoy <- sum(by_researcher$godoy$converge) / nrow(by_researcher$godoy)
convergence_guilherme <- sum(by_researcher$guilherme$converge) / nrow(by_researcher$guilherme)
convergence_lucas <- sum(by_researcher$lucas$converge) / nrow(by_researcher$lucas)


select_results_from_pair <- function(results, researchers) {
  results %>% 
    filter(researcher_group1 %in% researchers & researcher_group2 %in% researchers)
}

calc_convergence_by_pairs <- function(results, researchers) {
  convergence_table <- tibble(
    researcher_1 = c(
      rep(researchers[[1]], 3),
      rep(researchers[[2]], 2),
      researchers[[3]]
    ),
    researcher_2 = c(
      researchers[[2]],
      researchers[[3]],
      researchers[[4]],
      researchers[[3]],
      rep(researchers[[4]], 2)
    ),
    convergence_rate = 0
  )
  for (i in 1:nrow(convergence_table)) {
    pair <- select_results_from_pair(
      results, c(convergence_table$researcher_1[[i]], convergence_table$researcher_2[[i]])
    )
    convergence_table$convergence_rate[[i]] <- nrow(filter(pair, converge)) / nrow(pair)
  }
  return(convergence_table)
}

convergence_table_in_pairs <- calc_convergence_by_pairs(
  results, c("Felipe Roquete", "Felipe Godoy", "Guilherme", "Lucas")
)

write_csv2(convergence_table_in_pairs, "DataOutput/convergence_pairs.csv")

ids_divergence <- results %>% filter(!converge) %>% .$id

divergence_table <- sec_classified %>% 
  filter(id %in% ids_divergence) %>% 
  select(-all_of(c("Fake_news", "researcher", "division"))) %>% 
  filter(!duplicated(id)) %>% 
  mutate(Fake_news = "")

write_csv2(divergence_table, "DataOutput/secondary_classification_divergence_table.csv")

divergence_table_solved <- read_csv2("DataOutput/secondary_classification_divergence_table_done.csv")

new_codes <- c(
  results %>% 
    filter(converge & classification_group1 == 1) %>% 
    .$id,
  divergence_table_solved %>% 
    filter(Fake_news == 1) %>% 
    .$id
)

excluded_codes <- results %>% 
  filter(!(id %in% new_codes)) %>% 
  .$id

codes <- readRDS("DataOutput/codes.rds")
codes[["secondary_codes_excluded_by_resolution"]] <- excluded_codes
codes[["secondary_codes"]] <- new_codes
codes[["universe"]] <- c(codes[["primary_codes"]], codes[["secondary_codes"]])
codes[["codes_tibble"]] <- NULL
codes[["codes_tibble"]] <- count_codes(codes)
saveRDS(codes, "DataOutput/codes.rds")
