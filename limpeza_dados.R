library(tidyverse)

dados_raw <- read_csv(here("dados/dados_raw.csv"))

dados_raw %>% 
  count(locality_name, sort = TRUE)

dados_raw %>% 
  glimpse()
