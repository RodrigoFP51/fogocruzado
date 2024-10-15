library(tidyverse)
library(httr)
library(jsonlite)
library(here)


# Adquirir token ----------------------------------------------------------
readRenviron(".Renviron")

auth_url <- "https://api-service.fogocruzado.org.br/api/v2/auth/login"

auth <- POST(
  auth_url,
  body = toJSON(
    list("email" = Sys.getenv("EMAIL"),
         "password" = Sys.getenv("PASSWORD")),
    auto_unbox = TRUE
  ),
  encode = "raw",
  config = add_headers("Content-Type" = "application/json")
)

access_token <- content(auth, as = "parsed")$data$accessToken

# auth2 <- POST("https://api-service.fogocruzado.org.br/api/v2/auth/refresh",
#               body = toJSON(list(Authorization = paste0("Bearer ", access_token)), 
#                             auto_unbox = TRUE))

# Acesso aos dados --------------------------------------------------------

json_to_tbl <- function(resp){
  content(resp, "text") %>%
    fromJSON() %>%
    pluck("data") %>%
    as_tibble() %>% 
    unnest(cols = c(state, region, city,
                    neighborhood, subNeighborhood, locality,
                    victims),
           names_sep = "_") %>%
    unnest(cols = c(victims_genre, victims_place, victims_politicalPosition,
                    victims_politicalStatus, victims_coorporation),
           names_sep = "_") %>% 
    discard(\(x) is.data.frame(x) | is.list(x))
}

base_url <- "https://api-service.fogocruzado.org.br/api/v2"
endpoint <- "/occurrences"

resp <- GET(
  #url = "https://api-service.fogocruzado.org.br/api/v2/occurrences?order=ASC&page=1&take=20&idState=813ca36b-91e3-4a18-b408-60b27a1942ef", 
  paste0(base_url, endpoint),
  config = add_headers("Authorization" = paste("Bearer", access_token),
                       "Content-Type" = "application/json"),
  query = list(idState = "b112ffbe-17b3-4ad0-8f2a-2038745d1d14",
               initialdate="2020-01-01",
               finaldate="2024-10-01")
)

total_pag <- resp %>% 
  content() %>%  
  pluck("pageMeta") %>% 
  pluck("pageCount")

all_data <- map(
  .x = 1:total_pag,
  #.x = 1:total_pag,
  .f = function(x){
    url <- paste0("https://api-service.fogocruzado.org.br/api/v2/occurrences?order=ASC&page=",
                  x,
                  "&take=20")
    
    resp <- GET(
      url    = paste0(base_url, endpoint),
      config = add_headers("Authorization" = paste("Bearer", access_token),
                           "Content-Type"  = "application/json"),
      query  = list(idState     = "b112ffbe-17b3-4ad0-8f2a-2038745d1d14",
                    initialdate = "2020-01-01",
                    finaldate   = "2024-10-01")
    )
    
    content <- json_to_tbl(resp)
    
    Sys.sleep(1)
    
    return(content)
  }
)

all_data <- all_data %>% 
  list_rbind()

write_csv(all_data, 
          here("dados/dados_raw.csv"))

