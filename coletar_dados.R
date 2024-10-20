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

base_url <- "https://api-service.fogocruzado.org.br/api/v2/"
endpoint <- "occurrences"

query_params <- list(
  idState        = "b112ffbe-17b3-4ad0-8f2a-2038745d1d14",
  initialdate    = "2017-01-01",
  finaldate      = "2024-01-01",
  typeOccurrence = "all",
  take           = 20
)

resp <- GET(
  paste0(base_url, endpoint),
  config = add_headers("Authorization" = paste("Bearer", access_token),
                       "Content-Type" = "application/json"),
  query = query_params
)

total_pag <- resp %>% 
  content() %>%  
  pluck("pageMeta") %>% 
  pluck("pageCount")

all_data <- map(
  .x = 1:total_pag,
  #.x = 1:total_pag,
  .f = function(x){
    url <- paste0("https://api-service.fogocruzado.org.br/api/v2/occurrences?page=", x)
    
    resp <- GET(
      url    = paste0(base_url, endpoint),
      config = add_headers("Authorization" = paste("Bearer", access_token),
                           "Content-Type" = "application/json"),
      query  = query_params
    )
    
    content <- json_to_tbl(resp)
    
    Sys.sleep(1)
    
    return(content)
  },
  .progress = TRUE
) %>% 
  list_rbind()

write_csv(all_data, 
          here("dados/dados_raw.csv"))

