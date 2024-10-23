library(tidyverse)
library(janitor)

dados_raw <- read_csv(here("dados/dados_raw.csv"))

dados_raw %>% 
  tabyl(city_name)

dados_raw %>% 
  tabyl(genre_name)
  
dados_raw %>% 
  glimpse()
