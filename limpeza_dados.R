library(tidyverse)
library(janitor)
library(here)

dados_raw <- read_csv(here("dados/dados_raw.csv")) %>% 
  distinct(.keep_all = TRUE)


dados_limpo <- dados_raw %>% 
  mutate(
    across(c(city_name, neighborhood_name, 
             subNeighborhood_name, locality_name),
           str_to_title),
    victims_situation = fct_recode(victims_situation,
                                   "Morto"   = "Dead",
                                   "Ferido"  = "Wounded"),
    victims_personType = fct_recode(victims_personType,
                                   "Agente" = "Agent",
                                   "Civil"  = "Civilian")
  ) %>% 
  select(
    ## Retirar colunas que tem apenas 1 valor único
    -where(\(x) length(unique(x)) == 1),
    ## Retirar colunas que a proporção de NA's é maior do que 50%
    -where(\(x) mean(is.na(x)) >= 0.5)  
  ) %>%
  mutate(
    neighborhood_name = ifelse(neighborhood_name == "Centro",
                               str_c(neighborhood_name, " - ", city_name),
                               neighborhood_name),
    neighborhood_name = ifelse(neighborhood_name == "Ni",
                               "Não identificado",
                               neighborhood_name)
  )

write_csv(dados_limpo, 
          file = here("dados", "dados_limpo.csv"))




