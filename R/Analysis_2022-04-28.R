library(tidyverse)
library(scales)
library(lubridate)
library(DescTools)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

set_theme_fgv_digital()

primary_classification_table_enhanced <- readRDS("DataOutput/primary_classification_table_enhanced.rds")

ufs <- read_csv2("DataOutput/UF_representation.csv")

primary_classification_table_authors <- primary_classification_table_enhanced %>% 
  select(id, autores) %>% 
  unnest(autores)

primary_classification_table_procedures <- primary_classification_table_enhanced %>% 
  select(id, procedure) %>% 
  unnest(procedure)

calc_procedure_delay <- function(sequence, date) {
  
  output <- vector('numeric', length(date))
  
  for (i in seq_along(output)) {
    if (sequence[[i]] > 1) {
      output[[i]] <- as.numeric(date[[i]]) - as.numeric(date[[i-1]])
    } else {
      output[[i]] <- NA
    }
  }
  return(output)
}

primary_classification_table_procedures <- primary_classification_table_procedures %>% 
  mutate(delay = calc_procedure_delay(sequencia, ymd(dataHora)))

party_engagement <- primary_classification_table_authors %>% 
  filter(!is.na(siglaPartido)) %>% 
  count(id, siglaPartido) %>% 
  count(siglaPartido)

# Analysis ----------------------------------------------------------------


# Yearly evolution --------------------------------------------------------

primary_classification_table_enhanced %>% 
  mutate(ano = as_factor(str_extract(dataApresentacao, "\\d{4}")),
         ano = fct_recode(ano, `< 2009` = "2009", `< 2009` = "1992")) %>% 
  ggplot(aes(x = ano)) +
  geom_bar(fill = fgv_palette$primary) +
  labs(x = "Ano", y = "Número de proposições")


# Party engagement --------------------------------------------------------

party_engagement %>% 
  mutate(siglaPartido = fct_reorder(siglaPartido, n)) %>% 
  ggplot(aes(x = siglaPartido, y = n)) +
  geom_col(fill = fgv_palette$primary) +
  labs(x = "", y = "Número de Proposições em que Participou") +
  coord_flip()


# UF engagement -----------------------------------------------------------

# BLIND TO PROP
primary_classification_table_authors %>% 
  filter(!is.na(siglaUf)) %>%
  count(siglaUf) %>%
  mutate(siglaUf = fct_reorder(siglaUf, n)) %>% 
  ggplot(aes(x = siglaUf, y = n)) +
  geom_col(fill = fgv_palette$primary) +
  labs(x = "", y = "Participação nos Projetos") +
  coord_flip()


# N-PROP
primary_classification_table_authors %>% 
  filter(!is.na(siglaUf)) %>%
  count(siglaUf) %>% 
  left_join(select(ufs, !cadeiras), by = "siglaUf") %>% 
  mutate(n_prop = n * multiplicador,
         siglaUf = fct_reorder(siglaUf, n_prop)) %>% 
  ggplot(aes(x = siglaUf, y = n_prop)) +
  geom_col(fill = fgv_palette$primary) +
  labs(x = "", y = "Participação Proporcional nos Projetos") +
  coord_flip()

# Procedures - Histogram --------------------------------------------------

primary_classification_table_procedures %>% 
  count(id) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(fill = fgv_palette$secondary, bins = 20) +
  scale_x_continuous(breaks = seq(0, 100, by = 15), limits = c(0, 100)) +
  labs(x = "Número de tramitações", y = "Número de proposições")


# Procedures - Dispersion -------------------------------------------------

primary_classification_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  .$delay %>% 
  mean(na.rm = TRUE)

primary_classification_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  .$delay %>% 
  median(na.rm = TRUE)

primary_classification_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  ggplot(aes(x = "", y = delay)) +
  geom_jitter(color = fgv_palette$primary, alpha = 0.8)

primary_classification_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  ggplot(aes(x = "", y = Winsorize(delay, probs = c(0.1, 0.9)))) +
  geom_jitter(color = fgv_palette$primary, alpha = 0.8)

primary_classification_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  ggplot(aes(x = Winsorize(delay, probs = c(0.1, 0.9)))) +
  geom_density(color = fgv_palette$primary, fill = fgv_palette$blues4)
  


