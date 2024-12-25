library(shiny)
library(tidyverse)
library(here)

# Carregando os dados
data <- read_csv(here("dados", "dados_resumido.csv"))
indicadores_possiveis <- names(data)[str_detect(names(data), "^n_")]



# UI do aplicativo
ui <- fluidPage(
  titlePanel("Exploração de Dados"),
  
  tabsetPanel(
    tabPanel(
      "Exploração Geral",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "cidade",
            "Escolha a cidade:",
            choices = unique(data$city_name),
            selected = unique(data$city_name)[1]
          )
        ),
        
        mainPanel(
          plotOutput("grafico_n_vitimas")
        )
      )
    ),
    tabPanel(
      "Indicadores",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "indicador",
            "Escolha o indicador:",
            choices = indicadores_possiveis,
            selected = indicadores_possiveis[1]
          )
        ),
        mainPanel(
          plotOutput("grafico_indicadores")
        )
      )
    )
  )
)

# Server do aplicativo
server <- function(input, output, session) {
  
  # Gráfico de evolução de n_vitimas
  output$grafico_n_vitimas <- renderPlot({
    data %>%
      filter(city_name == input$cidade) %>%
      group_by(date = floor_date(date, "month")) %>%
      summarise(n_vitimas = sum(n_vitimas, na.rm = TRUE)) %>%
      ggplot(aes(x = date, y = n_vitimas)) +
      geom_line(color = paleta_situacao[1],
                size = 1) +
      labs(
        title = "Evolução de n_vitimas",
        x = "Data",
        y = "Número de Vítimas"
      ) +
      theme_minimal()
  })
  
  # Gráfico de indicadores
  output$grafico_indicadores <- renderPlot({
    data %>%
      group_by(city_name) %>%
      summarise(valor = sum(.data[[input$indicador]], na.rm = TRUE)) %>%
      arrange(desc(valor)) %>%
      ggplot(aes(x = reorder(city_name, valor), y = valor)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Indicador:", input$indicador),
        x = "Cidade",
        y = "Valor Total"
      ) +
      theme_minimal()
  })
}

# Executa o aplicativo
shinyApp(ui = ui, server = server)
