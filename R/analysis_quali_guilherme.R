# Libraries ---------------------------------------------------------------

library(tidyverse)
library(pdftools)
library(readxl)

# Import Classification Data ----------------------------------------------

class_guilherme <- read_xlsx("DataOutput/fake_news_class_guilherme.xlsx")

columns_to_clean <- names(class_guilherme)[10:13] 

for (i in seq_along(columns_to_clean)) {
  column <- columns_to_clean[[i]]
  class_guilherme[[column]] <- ifelse(is.na(class_guilherme[[column]]) | class_guilherme[[column]] != "x", 0, 1)
}

class_guilherme <- class_guilherme %>% 
  mutate(moderacao = as.numeric(moderacao))

# Donwload Proposition Texts ----------------------------------------------

for (i in 1:nrow(class_guilherme)) {
  dest_file <- paste("DataInteiroTeor/", class_guilherme$id[[i]], ".pdf")
  pdf_file <- class_guilherme$urlInteiroTeor[[i]]
  download.file(pdf_file, destfile = dest_file, mode = "wb")
  Sys.sleep(4)
}

pdf_files <- list.files(path = "DataInteiroTeor", full.names = TRUE)

pdf_df <- vector('list', length(pdf_files))

for (i in seq_along(pdf_files)) {
  pdf_file <- pdf_files[[i]]
  text <- pdf_text(pdf_file) %>% unlist() %>% paste(collapse = "\n>>>\n")
  id <- pdf_file %>% str_extract("\\d+")
  pdf_df[[i]] <- tibble(
    id = id,
    text = text
  )
}

pdf_df <- bind_rows(pdf_df)

saveRDS(pdf_df, "DataOutput/inteiroTeor_class_guilherme.rds")

class_guilherme %>% 
  filter(as.logical(moderacao)) %>% 
  saveRDS("DataOutput/df_class_guilherme.rds")
