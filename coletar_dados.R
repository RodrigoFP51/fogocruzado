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
    list("email"    = Sys.getenv("EMAIL"),
         "password" = Sys.getenv("PASSWORD")),
    auto_unbox = TRUE
  ),
  encode = "raw",
  config = add_headers("Content-Type" = "application/json")
)

access_token <- content(auth, as = "parsed")$data$accessToken

# Acesso aos dados --------------------------------------------------------

json_to_tbl <- function(resp){
  content(resp, "text") %>%
    fromJSON() %>%
    pluck("data") %>%
    as_tibble() %>% 
    unnest(cols = c(
      #state,
      #region,
      city, 
      neighborhood, subNeighborhood, locality, victims),
      names_sep = "_") %>% 
    unnest(cols = c(victims_ageGroup, victims_genre, victims_place,
                    victims_serviceStatus, victims_politicalPosition, victims_politicalStatus,
                    victims_coorporation, victims_agentPosition, victims_agentStatus),
           names_sep = "_") %>% 
    unnest(contextInfo, names_sep = "_") %>% 
    unnest(contextInfo_mainReason, names_sep = "_") %>% 
    discard(\(x) is.data.frame(x) | is.list(x))
}

base_url <- "https://api-service.fogocruzado.org.br/api/v2/"
endpoint <- "occurrences"

query_params <- list(
  idState        = "b112ffbe-17b3-4ad0-8f2a-2038745d1d14",
  initialdate    = "2017-01-01",
  finaldate      = "2024-01-01",
  typeOccurrence = "all"
  #take           = 200
)

resp <- GET(
  paste0(base_url, endpoint),
  config = add_headers("Authorization" = paste("Bearer", access_token),
                       "Content-Type"  = "application/json"),
  query = query_params
)

total_pag <- resp %>% 
  content() %>%  
  pluck("pageMeta") %>% 
  pluck("pageCount")

safely_get_data <- safely(
  .f = function(x){
    url <- paste0(
      base_url, endpoint
      #"https://api-service.fogocruzado.org.br/api/v2/occurrences?page=", 
      #x
    )
    
    query_params$page <- x
    
    resp <- GET(
      url    = url,
      config = add_headers("Authorization" = paste("Bearer", access_token),
                           "Content-Type"  = "application/json"),
      query  = query_params
    )
    
    
    if(!resp$status_code %in% 200){
      stop("Status code: ", resp$status_code)
    }
    
    content <- json_to_tbl(resp)
    
    Sys.sleep(1.2)
    
    #return(content)
    return(content)
  }
)

all_data <- map(
  #.x = 1:10,
  .x = 1:total_pag,
  .f = safely_get_data,
  .progress = TRUE
)

all_data <- all_data %>% 
  map("result") %>% 
  list_rbind()

write_csv(x = all_data,
          file = here("dados/dados_raw.csv"))

