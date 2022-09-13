library(tidyverse)
library(readxl)

classified <- read_xlsx("DataOutput/type_selection_of_related_proposals_FLVR.xlsx")

classified %>% count(Fake_news)

set.seed(1234)

selection <- classified %>% 
  filter(Fake_news != 99) %>% 
  group_by(Fake_news) %>% 
  slice_sample(prop = 0.33) %>% 
  ungroup()

shuffled_selection <- selection %>% 
  slice_sample(prop = 1)

select_rows <- function(df) {
  df %>% 
    select(id, uri, siglaTipo, numero, ano, ementa, urlInteiroTeor) %>% 
    mutate(Fake_news = "", OBS = "")
}

guilherme <- select_rows(shuffled_selection[1:50,])

felipe <- select_rows(shuffled_selection[51:100,])

lucas <- select_rows(shuffled_selection[101:145,])

guilherme %>% write_csv2("DataOutput/blind_review_guilherme.csv")
felipe %>% write_csv2("DataOutput/blind_review_felipe.csv")
lucas %>% write_csv2("DataOutput/blind_review_lucas.csv")

