# Libraries and code imports ----------------------------------------------

library(tidyverse)
library(rvest)
library(httr)
library(readxl)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

# Get search codes --------------------------------------------------------

primary_codes <- list.files(
  path = "DataInput", 
  pattern = "^busca_", 
  full.names = TRUE
) %>% 
  map_dfr(read_csv2) %>% 
  mutate(code = str_extract(Link, "\\d+$")) %>% 
  .$code

primary_codes <- primary_codes[!is.na(primary_codes)] %>% unique()

codes <- list(
  primary_codes = primary_codes,
  new_codes = NULL,
  full_codes = primary_codes
)

saveRDS(codes, "DataOutput/codes.rds")

# Download attached docs pages --------------------------------------------

attached_docs_pages <- map(
  primary_codes, 
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
  primary_codes,
  write_html_page_by_code,
  path = "DataOutput/paginas/arvores_apensados"
)

