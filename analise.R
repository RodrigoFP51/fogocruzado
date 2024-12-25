library(tidyverse)
library(here)
library(gt)

ocorrencias <- 
  read_csv(here("dados", "dados_limpo.csv"))


paleta_situacao <- c("#fdb750", "#fc2e20")
theme_set(
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
)

plot_categories <- function(data, 
                            col, 
                            fill_col = NULL,
                            bar_color = NULL){
  
  p <- ggplot(data, aes({{ col }}, n))
  
  if (is.null(bar_color)) {
    p +
      geom_col(aes(fill = {{ fill_col }}),
               position = position_dodge()) +
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
  mutate(total = sum(n), .by = city_name) %>%
  filter(total > 100) %>% 
  mutate(city_name = fct_reorder(city_name, n)) %>% 
  ggplot(aes(n, city_name)) +
  geom_line(lwd = 1.5, color = "brown") +
  geom_point(aes(fill = victims_situation),
             size = 5,
             color = "white",
             alpha = 0.8,
             shape =  21) +
  #plot_categories(city_name, fill_col = victims_situation) + 
  scale_fill_manual(values = paleta_situacao,
                    name = "Situação da vítima") +
  scale_x_continuous(breaks = seq(0, 8000, 1000)) +
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")


ocorrencias %>% 
  count(neighborhood_name, victims_situation, sort = TRUE) %>% 
  mutate(total = sum(n), .by = neighborhood_name) %>%
  filter(total > 100) %>%
  mutate(neighborhood_name = fct_reorder(neighborhood_name, n),
         label = paste0(round(n / total, 2) * 100, "%")) %>% 
  ggplot(aes(n, neighborhood_name)) +
  geom_line(lwd = 1.2, color = "brown") +
  geom_point(aes(fill = victims_situation),
             size = 5,
             color = "white",
             alpha = 0.8,
             shape =  21) +
  #plot_categories(neighborhood_name, victims_situation) +
  scale_fill_manual(values = paleta_situacao, name = "") +
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")



# Quais são os principais motivos das ocorrências ------------------------

ocorrencias %>% 
  mutate(
    contextInfo_mainReason_name = fct_lump_min(
      contextInfo_mainReason_name,
      min = 50,
      other_level = "Outro"
    )
  ) %>% 
  count(contextInfo_mainReason_name, sort = TRUE) %>% 
  mutate(contextInfo_mainReason_name = fct_reorder(contextInfo_mainReason_name, n)) %>% 
  plot_categories(contextInfo_mainReason_name, bar_color = "firebrick") + 
  labs(x = "", y = "N° Ocorrências") +
  theme(legend.position = "bottom")

# Principais locais que as vitimas se encontravam nas ocorrências ---------------------------------------

ocorrencias %>%
  count(victims_place_name, sort = TRUE) %>%
  filter(victims_place_name != "Sem identificação") %>% 
  mutate(victims_place_name = fct_reorder(victims_place_name, n)) %>%
  plot_categories(victims_place_name, bar_color = "firebrick") +
  labs(x = "", y = "N° Ocorrências",
       title = "Local em que as vítimas estavam no momento da ocorrência",
       subtitle = "Excluindo registros sem identificação") 

# Evolução no tempo -------------------------------------------------------

ocorrencias_resumido <- ocorrencias %>% 
  group_by(id) %>% 
  summarize(
    across(
      c(date, city_name, neighborhood_name,
        latitude, longitude),
      unique
    ),
    n_mortos           = sum(victims_situation == "Morto"),
    n_feridos          = sum(victims_situation == "Ferido"),
    n_agentes_mortos   = sum(victims_personType == "Agente" & victims_situation == "Morto"),
    n_civis_mortos     = sum(victims_personType == "Civil" & victims_situation == "Morto"),
    n_homens_mortos    = sum(victims_genre_name == "Homem cis" & victims_situation == "Morto"),
    n_mulheres_mortos  = sum(victims_genre_name == "Mulher cis" & victims_situation == "Morto"),
    n_trans_mortos     = sum(victims_genre_name == "Mulher trans e travesti" & victims_situation == "Morto"),
    n_homens_feridos   = sum(victims_genre_name == "Homem cis" & victims_situation == "Ferido"),
    n_mulheres_feridos = sum(victims_genre_name == "Mulher cis" & victims_situation == "Ferido"),
    n_vitimas          = n()
  )

write_csv(ocorrencias_resumido, here("dados", "dados_resumido.csv"))
  
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


# Detalhes por bairro -----------------------------------------------------

ocorrencias_resumido %>% 
  summarize(
    across(c(n_mortos, n_feridos, n_vitimas), sum),
    .by = neighborhood_name
  ) %>% 
  mutate(prop_mortos = n_mortos / n_vitimas) %>%
  filter(n_vitimas > 100) %>% 
  arrange(desc(n_vitimas)) %>% 
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
  ) %>% 
  data_color(
    columns = prop_mortos,
    method = "numeric",
    palette = "rcartocolor::RedOr"
  ) %>% 
  gtExtras::gt_theme_espn()


ocorrencias_resumido %>% 
  group_by(
    hora = factor(hour(date)),
    minuto = factor(floor(minute(date) / 15) * 15)
  ) %>% 
  summarize(
    n_vitimas = sum(n_vitimas)
  ) %>% 
  ggplot(aes(minuto, hora)) +
  geom_tile(aes(fill = n_vitimas)) +
  scale_fill_viridis_c(begin = 0.25)


# Sazonalidade mes x ano --------------------------------------------------

ocorrencias_resumido %>% 
  group_by(
    mes = month(date, abbr = TRUE, label = TRUE),
    ano = year(date)
  ) %>% 
  filter(ano != 2024) %>% 
  summarize(
    n_vitimas = sum(n_vitimas)
  ) %>% 
  ggplot(aes(mes, ano)) +
  geom_tile(aes(fill = n_vitimas),
            color = "white") +
  geom_text(aes(label = n_vitimas),
            color = "white",
            size = 3) +
  scale_y_continuous(breaks = 2017:2024) +
  scale_fill_gradient(name = "N° de Vítimas",
                      low = "#fca379",
                      high = "#913101") +
  labs(x='', y='',
       title = "Sazonalidade ano x mês")







