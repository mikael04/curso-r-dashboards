# navbarPage
# 
# Utilizando a base cetesb, faça um shiny app que tenha duas abas:
#
# - a primeira com uma série temporal da média diária do ozônio (O3),
# permitindo a escolha do intervalo de dias em que o gráfico é gerado
#
# - a segunda com a série temporal da média diária do último mês da base 
# permitindo a escolha do poluente.

library(shiny)
library(bs4Dash)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
# library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)

# df <- readRDS("Material-curso-r/dados/cetesb.rds")
df <- readRDS("../dados/cetesb.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "med_oz", text = "Média diária Oz", icon = icon("home")),
    menuItem(tabName = "med_mes_ant", text = "Média mês anterior", icon = icon("tree"))
  )),
  dashboardBody(
    fluidRow(
      br(),
      h4("Tabela de crimes para o mês selecionado"),
      reactable::reactableOutput("table"),
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  teste = F
  # df_auxx <- shiny::eventReactive(input$selecionar_data, {
  #   # browser()
  #   df
  # })
  df_aux <- shiny::eventReactive(input$date, {
    df
  })
  output$table <- reactable::renderReactable({
    # browser()
    df_ <- df_aux()
    reactable::reactable(df_)
    
  })
}

shinyApp(ui, server)

