library(tidyverse)

ocorrencias <- 
  read_csv(here("dados", "dados_limpo.csv"))


paleta_situacao <- c("#fdb750", "#fc2e20")
theme_set(
  theme_minimal()
)

plot_categories <- function(data, col, fill = NULL){
  
  ggplot(data, aes({{ col }}, n, fill = {{ fill }})) +
    geom_col() +
    coord_flip()
    
}

# Quais localidades tem mais ocorrências ----------------------------------

ocorrencias %>% 
  count(city_name, victims_situation, sort = TRUE) %>% 
  mutate(city_name = fct_reorder(city_name, n)) %>% 
  plot_categories(city_name, victims_situation) + 
  scale_fill_manual(values = paleta_situacao) 
  
ocorrencias %>% 
  count(neighborhood_name, victims_situation, sort = TRUE) %>% 
  mutate(total = sum(n), .by = neighborhood_name) %>%
  filter(total > 100) %>%
  mutate(neighborhood_name = fct_reorder(neighborhood_name, n)) %>% 
  plot_categories(neighborhood_name, victims_situation) +
  scale_fill_manual(values = paleta_situacao)
  



