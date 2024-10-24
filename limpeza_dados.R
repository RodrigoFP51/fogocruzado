library(tidyverse)
library(janitor)

dados_raw <- read_csv(here("dados/dados_raw.csv")) %>% 
  distinct(.keep_all = TRUE)

## cada ID representa uma ocorrência
dados_raw %>% 
  count(id, sort = TRUE)

glimpse(dados_raw)

dados_raw %>% 
  add_count(id)



