library(tidyverse)
library(here)
library(gt)

ocorrencias <- 
  read_csv(here("dados", "dados_limpo.csv"))


paleta_situacao <- c("#fdb750", "#fc2e20")
theme_set(
  theme_minimal()
)

plot_categories <- function(data, 
                            col, 
                            fill_col = NULL,
                            bar_color = NULL){
  
  p <- ggplot(data, aes({{ col }}, n))
  
  if (is.null(bar_color)) {
    p +
      geom_col(aes(fill = {{ fill_col }})) +
      coord_flip()
  } else {
    p +
      geom_col(fill = bar_color) +
      coord_flip()
      
  }
  
}

# Quais localidades tem mais ocorrências ----------------------------------

ocorrencias %>% 
  count(city_name, victims_situation, sort = TRUE) %>% 
  mutate(city_name = fct_reorder(city_name, n)) %>%
  filter(n > 200) %>% 
  plot_categories(city_name, fill_col = victims_situation) + 
  scale_fill_manual(values = paleta_situacao) +
  scale_y_continuous(breaks = seq(0, 8000, 1000)) +
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")


ocorrencias %>% 
  count(neighborhood_name, victims_situation, sort = TRUE) %>% 
  mutate(total = sum(n), .by = neighborhood_name) %>%
  filter(total > 100) %>%
  mutate(neighborhood_name = fct_reorder(neighborhood_name, n),
         label = paste0(round(n / total, 2) * 100, "%")) 
  plot_categories(neighborhood_name, victims_situation) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  scale_fill_manual(values = paleta_situacao, name = "") +
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")



# Quais são os principais motivos das ocorrências ------------------------

ocorrencias %>% 
  count(contextInfo_mainReason_name, sort = TRUE) %>%
  mutate(contextInfo_mainReason_name = fct_reorder(contextInfo_mainReason_name, n)) %>% 
  plot_categories(contextInfo_mainReason_name, bar_color = "firebrick") + 
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")

# Principais locais que as vitimas se encontravam nas ocorrências ---------------------------------------

  ocorrencias %>% 
  count(victims_place_name, sort = TRUE) %>%
  mutate(victims_place_name = fct_reorder(victims_place_name, n)) %>% 
  plot_categories(victims_place_name, bar_color = "firebrick") + 
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")
  

ocorrencias_resumido <- ocorrencias %>% 
  group_by(id) %>% 
  summarize(
    date = unique(date),
    across(c(city_name, neighborhood_name, latitude, longitude),
           unique),
    n_mortos = sum(victims_situation == "Morto"),
    n_feridos = sum(victims_situation == "Ferido"),
    n_vitimas = n()
  )
  
ocorrencias_resumido %>% 
  mutate(date = floor_date(as_date(date), "month")) %>% 
  group_by(date) %>% 
  summarize(across(c(n_vitimas, n_mortos, n_feridos), sum)) %>% 
  pivot_longer(-date,
               names_to = "variavel",
               values_to = "valor") %>%
  mutate(variavel = fct_reorder(variavel, -valor)) %>% 
  ggplot(aes(date, valor, color = variavel)) +
  geom_line(lwd = 1) +
  scale_color_viridis_d(begin = 0.3, 
                        name = "Situação da Vítima")
  
ocorrencias_resumido %>% 
  summarize(
    across(n_mortos:n_vitimas, sum),
    .by = neighborhood_name
  ) %>% 
  mutate(prop_mortos = n_mortos / n_vitimas) %>%
  filter(n_vitimas > 100) %>% 
  arrange(desc(prop_mortos)) %>% 
  gt() %>% 
  cols_label(
    neighborhood_name = "Bairro",
    n_mortos          = "N° de mortos",
    n_feridos         = "N° de feridos",
    n_vitimas         = "N° de vítimas",
    prop_mortos       = "Proporção de mortos"
  ) %>% 
  fmt_percent(
    columns = prop_mortos,
    drop_trailing_zeros = TRUE,
    drop_trailing_dec_mark = TRUE,
    dec_mark = ","
  )


