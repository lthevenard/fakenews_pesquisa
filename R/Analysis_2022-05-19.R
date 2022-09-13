library(tidyverse)
library(rvest)
library(scales)
library(lubridate)
library(DescTools)
library(geobr)
library(janitor)

list.files("R/Functions/", full.names = TRUE) %>%
  walk(source)

set_theme_fgv_digital()

universe_table_enhanced <- readRDS("DataOutput/universe_table_enhanced.rds") %>% 
  mutate(dataApresentacao = str_extract(dataApresentacao, "\\d{4}-\\d{2}-\\d{2}"),
         ano = str_extract(dataApresentacao, "\\d{4}")) %>% 
  filter(as.numeric(ano) > 2009)

ufs <- read_csv2("DataOutput/UF_representation.csv")

estados <- read_state(code_state = "all") %>% as_tibble()

universe_table_authors <- universe_table_enhanced %>% 
  select(id, autores) %>% 
  unnest(autores) %>% 
  left_join(select(author_info, idAutor, siglaPartido), by = "idAutor")

universe_table_procedures <- universe_table_enhanced %>% 
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

universe_table_procedures <- universe_table_procedures %>% 
  mutate(delay = calc_procedure_delay(sequencia, ymd(dataHora)))

party_engagement <- universe_table_authors %>% 
  filter(!is.na(siglaPartido)) %>% 
  count(id, siglaPartido) %>% 
  count(siglaPartido)
  
party_count_id <- universe_table_authors %>% 
  filter(!is.na(siglaPartido)) %>% 
  count(id, siglaPartido)

party_ideology <- read_csv2("DataOutput/Partidos_espectro.csv") %>% 
  rename(siglaPartido = "Partido")

party_ideology_mesh <- 
  party_engagement %>% 
  mutate(siglaPartido = siglaPartido %>% 
           str_replace("^PFL$", "DEM") %>% 
           str_replace("^PMDB$", "MDB") %>% 
           str_replace("^PR$", "PL")) %>% 
  group_by(siglaPartido) %>% 
  summarise(n = sum(n)) %>% 
  left_join(party_ideology, by = "siglaPartido") %>%
  filter(!is.na(Media)) %>%
  arrange(Media) %>% 
  mutate(siglaPartido = fct_reorder(siglaPartido, Media),
         vert = rep(c(9.5, 10.5), 9))

libpat <- "liberdade\\s*de\\s*(express[ãa]o|pensamento|opini[ãa]o|imprensa)|censura"

pdf_df <- readRDS("DataOutput/inteiroTeor_class_guilherme.rds") %>% 
  mutate(libexp = text %>% str_to_lower() %>% str_count(libpat),
         has_libexp = libexp > 2,
         id = as.numeric(id))

party_mesh_libexp <- 
  party_count_id %>% 
  left_join(pdf_df, by = "id") %>% 
  mutate(siglaPartido = siglaPartido %>% 
           str_replace("^PFL$", "DEM") %>% 
           str_replace("^PMDB$", "MDB") %>% 
           str_replace("^PR$", "PL")) %>% 
  group_by(siglaPartido) %>% 
  summarise(libexp_count = sum(has_libexp), libexp_n = sum(libexp), n = n(), libexp_prop = libexp_count / n) %>% 
  left_join(party_ideology, by = "siglaPartido") %>%
  filter(!is.na(Media)) %>%
  arrange(Media) %>% 
  mutate(siglaPartido = fct_reorder(siglaPartido, Media),
         vert = rep(c(9.5, 10.5), 9))

regioes <- read_html(
  "https://www.todamateria.com.br/siglas-estados-brasileiros/"
) %>% 
  html_table() %>% .[[1]] %>% 
  rename(siglaUf = "Sigla") %>% 
  mutate(regiao = str_extract(`Observação`, "\\S+$")) %>% 
  select(siglaUf, regiao)

regioes_representatividade <- ufs %>% 
  left_join(regioes, by = "siglaUf") %>% 
  group_by(regiao) %>% 
  summarise(cadeiras = sum(cadeiras)) %>% 
  mutate(mult = 179 / cadeiras)

# Analysis ----------------------------------------------------------------

save_plot <- function(filename, path = "./Plots", height= 6, width= 10, dpi = 400, ...) {
  ggsave(filename=filename, height= height, width= width, path = path, dpi = dpi, ...)
}

save_plot_short <- function(filename, path = "./Plots", height= 5, width= 10, dpi = 400, ...) {
  ggsave(filename=filename, height= height, width= width, path = path, ...)
}


# Yearly evolution --------------------------------------------------------

universe_table_enhanced %>% 
  arrange(ano) %>% 
  mutate(ano = as_factor(str_extract(dataApresentacao, "\\d{4}")),
         ano = fct_recode(ano, `≤ 2017` = "2011", `≤ 2017` = "2012", `≤ 2017` = "2015", `≤ 2017` = "2017",)) %>% 
  ggplot(aes(x = ano)) +
  geom_bar(fill = fgv_palette$primary) +
  labs(x = "", y = "Número de proposições")

save_plot("yealy_evolution.png")

save_plot_short("yealy_evolution_short.png")

universe_table_enhanced %>% 
  filter(ymd(dataApresentacao) >= dmy("01/01/2020")) %>% 
  mutate(ano_mes = str_extract(dataApresentacao, "\\d{4}-\\d{2}") %>% 
           str_replace_all("-0[1234]$", "-1ºT") %>% 
           str_replace_all("-0[5678]$", "-2ºT") %>%
           str_replace_all("-09$|-1[012]$", "-3ºT")) %>% 
  arrange(ano_mes) %>% 
  ggplot(aes(x = as_factor(ano_mes))) +
  geom_bar(fill = fgv_palette$primary) +
  labs(x = "", y = "Número de proposições") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))



# Party engagement --------------------------------------------------------

party_ideology_mesh %>%
  filter(siglaPartido != "S.PART.") %>% 
  mutate(siglaPartido = fct_reorder(siglaPartido, n)) %>% 
  ggplot(aes(x = siglaPartido, y = n)) +
  geom_col(fill = fgv_palette$primary) +
  labs(x = "", y = "Número de Proposições em que Participou") +
  coord_flip()

save_plot("party_engagement.png")

save_plot_short("party_engagement_short.png")


# Party ideology ----------------------------------------------------------
  
party_ideology_mesh %>% 
  ggplot(aes(y = vert, x = Media, fill = siglaPartido, size = n)) +
  geom_point(color = "black", alpha = 0.5, shape = 21) +
  labs(x = "\nPosição no espectro político\n\nEsquerda   ←                                                                      →   Direita\n", 
       y = "", fill = "Partido:  ") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  scale_y_continuous(limits = c(4, 16)) +
  scale_size(guide="none", range = c(4,14)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
  
save_plot("ideology.png", width = 8, height = 4, dpi = 600)

sum(party_ideology_mesh$n * party_ideology_mesh$Media) / sum(party_ideology_mesh$n)

party_mesh_libexp %>% 
  ggplot(aes(y = libexp_count, x = Media, fill = siglaPartido, size = n)) +
  geom_point(color = "black", alpha = 0.5, shape = 21) +
  labs(x = "\nPosição no espectro político\n\nEsquerda   ←                                                                      →   Direita\n", 
       y = "No de proposições com menções a termos relacionados à liberdade de expressão\n", fill = "Partido:  ") +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  scale_y_continuous(limits = c(-1.5, 13.5), breaks = c(seq(0, 12, by = 2))) +
  scale_size(guide="none", range = c(4,14))

save_plot("ideology_libexp.png", width = 8, height = 8, dpi = 600)


 # Count authors -----------------------------------------------------------

most_engaged <- universe_table_authors %>% 
  count(autor, siglaUf, siglaPartido, sort = TRUE) %>% 
  filter(n >= 10)

write_csv2(most_engaged, "DataOutput/most_engaged_authors.csv")

# UF engagement -----------------------------------------------------------

# BLIND TO PROP
universe_table_authors %>% 
  filter(!is.na(siglaUf)) %>%
  count(siglaUf) %>%
  mutate(siglaUf = fct_reorder(siglaUf, n)) %>% 
  ggplot(aes(x = siglaUf, y = n)) +
  geom_col(fill = fgv_palette$primary) +
  labs(x = "", y = "Participação nos Projetos") +
  coord_flip()


# N-PROP
universe_table_authors %>% 
  filter(!is.na(siglaUf)) %>%
  count(siglaUf) %>% 
  left_join(select(ufs, !cadeiras), by = "siglaUf") %>% 
  mutate(n_prop = n * multiplicador,
         siglaUf = fct_reorder(siglaUf, n_prop)) %>% 
  ggplot(aes(x = siglaUf, y = n_prop)) +
  geom_col(fill = fgv_palette$primary) +
  labs(x = "", y = "Participação Proporcional nas Proposições") +
  coord_flip()

save_plot("ufs_bars.png")

universe_table_authors %>% 
  filter(!is.na(siglaUf)) %>%
  count(siglaUf) %>% 
  left_join(select(ufs, !cadeiras), by = "siglaUf") %>% 
  mutate(n_prop = n * multiplicador,
         siglaUf = fct_reorder(siglaUf, n_prop)) %>% 
  arrange(desc(n_prop)) %>% 
  left_join(regioes, by = "siglaUf") %>% 
  group_by(regiao) %>% 
  summarise(N = sum(n)) %>% 
  left_join(regioes_representatividade, by = "regiao") %>% 
  mutate(n_prop = round(N * mult, digits=2)) %>% 
  arrange(desc(n_prop)) %>% 
  select(!mult) %>% 
  write_csv2("DataClassification/engagement_regioes.csv")

universe_table_authors %>% 
  filter(!is.na(siglaUf)) %>%
  count(siglaUf) %>% 
  left_join(select(ufs, !cadeiras), by = "siglaUf") %>% 
  mutate(n_prop = n * multiplicador,
         siglaUf = fct_reorder(siglaUf, n_prop)) %>% 
  left_join(
    select(estados, abbrev_state, name_region, geom) %>% rename(siglaUf = "abbrev_state"),
    by = "siglaUf"
  ) %>% 
  ggplot() + 
  geom_sf(aes(fill = n_prop, geometry = geom), color = "#081734") +
  labs(fill = "Participação nas proposições (Score Proporcional)") +
  scale_fill_distiller(direction = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom")

save_plot("map.png", dpi = 600)

# Procedures - Histogram --------------------------------------------------

universe_table_procedures %>% 
  count(id) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(fill = fgv_palette$secondary, bins = 20) +
  scale_x_continuous(breaks = seq(0, 100, by = 15), limits = c(0, 100)) +
  labs(x = "Número de tramitações", y = "Número de proposições")


save_plot("procedure_histogram.png")

more_than_90_procedures <- universe_table_procedures %>% 
  count(id, sort = TRUE) %>% .$id %>% .[1:3]

universe_table %>% filter(id %in% more_than_90_procedures) %>% .$urlInteiroTeor

# Procedures - Dispersion -------------------------------------------------

universe_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  .$delay %>% 
  mean(na.rm = TRUE)

universe_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  .$delay %>% 
  median(na.rm = TRUE)

universe_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  ggplot(aes(x = "", y = delay)) +
  geom_jitter(color = fgv_palette$primary, alpha = 0.8)

save_plot("procedure_time_dispersion.png")

universe_table_procedures %>% 
  filter(!is.na(delay) & delay > 0) %>% 
  ggplot(aes(x = "", y = Winsorize(delay, probs = c(0.1, 0.9)))) +
  geom_jitter(color = fgv_palette$primary, alpha = 0.8)


universe_table_procedures %>% 
  arrange(id, sequencia) %>% 
  mutate(dataHora = as_date(dataHora)) %>% 
  mutate(delay = calc_procedure_delay(sequencia, dataHora)) %>% 
  filter(!is.na(delay) & delay > 75) %>% 
  ggplot(aes(x = delay)) +
  geom_density(color = fgv_palette$primary, fill = fgv_palette$blues4) +
  labs(x = "Tempo de tramitação", y = "Proposições (Densidade de Kernel)")

save_plot("procedure_time_distribution.png")

# Class Guilherme -----------------------------------------------------------

class_guilherme <- readRDS("DataOutput/df_class_guilherme.rds")

cats <- tibble(
  category = c(
    "Diretrizes e Princípios",
    "Sanções",
    "Direitos e Obrigações"
  ),
  prob = c(
    sum(class_guilherme$principios) / nrow(class_guilherme),
    sum(class_guilherme$sancoes) / nrow(class_guilherme),
    sum(class_guilherme$prerrogativas | class_guilherme$obrigacoes) / nrow(class_guilherme)
  )
)
  
cats %>% 
  mutate(category = fct_reorder(category, prob),
         perc = paste0(round(100*prob, digits = 1), "%") %>% str_replace("\\.", ",")) %>% 
  ggplot(aes(x = category, y = prob, label = perc)) +
  geom_col(fill = fgv_palette$secondary, width = 0.5) +
  geom_label(nudge_y = -0.04, color = fgv_palette$secondary, size = 3) +
  scale_y_continuous(limits = c(0, 0.9), labels = scales::label_percent()) +
  labs(x = "", y = "\nPorcentagem das Proposições sobre Moderação de Conteúdo") +
  coord_flip()

save_plot("guilherme_class.png", height = 4.5)

universe_table_authors %>% filter(str_detect(autor, "Manato")) %>% 
  left_join(universe_table_enhanced, by = "id") %>% 
  .$urlInteiroTeor


# Gianne ------------------------------------------------------------------

gianne <- list.files(
  "DataClassification/Tabela_gianne", 
  pattern = "csv$",
  full.names = TRUE
) %>% 
  map(read_csv2)

gianne[[1]] %>% 
  mutate(partido = fct_reorder(partido, N, .desc = TRUE)) %>% 
  ggplot(aes(x = partido, y = N)) + 
  geom_col(fill = fgv_palette$primary) + 
  labs(x = "", y = "Número de Emendas")

save_plot("gianne_1.png", width = 12) 

evolucao_datas <- gianne[[3]] %>% 
  janitor::clean_names() %>% 
  count(data_de_apresentacao)

datas_periodo <- paste0(1:25, "/06/2020") %>% 
  str_replace("^(?=\\d/)", "0")

datas_df <- tibble(
  data = datas_periodo,
  n = 0
)

for (i in 1:nrow(datas_df)) {
  if (datas_df$data[[i]] %in% evolucao_datas$data_de_apresentacao) {
    datas_df$n[[i]] <- evolucao_datas %>% 
      filter(data_de_apresentacao == datas_df$data[[i]]) %>% 
      .$n %>% .[[1]]
  }
}

datas_df <- datas_df %>% mutate(data = factor(data, ordered = TRUE, levels = datas_df$data))


datas_df %>% 
  ggplot(aes(x = data, y = n, group = "", label = ifelse(n ==0, NA, n))) +
  geom_point(color = fgv_palette$primary) +
  geom_line(color = fgv_palette$primary) +
  geom_label(fill = fgv_palette$secondary, color = "white", nudge_y = 3) +
  scale_y_continuous(limits = c(0, 69)) +
  scale_x_discrete(expand = expansion(add = 1.5)) +
  labs(x = "", y = "Número de Emendas") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 9))

save_plot("gianne_2.png")

gianne[[2]] %>% 
  filter(VOTO %in% c("Sim", "Não", "Abstenção")) %>% 
  mutate(voto = VOTO,
         PARTIDO = str_replace(PARTIDO, "S/Partido", "SEM PARTIDO"),
         partido = fct_infreq(PARTIDO)) %>% 
  count(partido, voto) %>% 
  arrange(partido, desc(voto)) %>% 
  group_by(partido) %>% 
  mutate(label_x = cumsum(n) - 0,5*n) %>% 
  ggplot(aes(y = partido, x = n, fill = voto)) +
  geom_col() +
  geom_text(aes(label = n, x = label_x), hjust = 2, color = "white") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_fill_manual(values = c("orange", "red", fgv_palette$secondary)) +
  labs(x = "Número de Votos", y = "", fill = "Voto: ") +
  guides(fill = guide_legend(reverse = TRUE))

save_plot("gianne_3.png")



