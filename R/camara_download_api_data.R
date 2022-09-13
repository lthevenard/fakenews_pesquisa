# Libraries and code imports ----------------------------------------------

library(tidyverse)
library(jsonlite)
library(rvest)
library(httr)
library(lubridate)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)


# Download proposals database ---------------------------------------------

options(timeout=1200)

## Proposals

walk(
  1992:2022, 
  safely_and_slowly_download_api_file, 
  theme = "proposicoes", 
  path = "DataInput"
)

## Proposals' authors

walk(
  1992:2009, 
  safely_and_slowly_download_api_file, 
  theme = "proposicoesAutores", 
  path = "DataInput"
)

## Proposals' themes

walk(
  1992:2022, 
  safely_and_slowly_download_api_file, 
  theme = "proposicoesTemas", 
  path = "DataInput"
)

