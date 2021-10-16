# Explorando inputs
# 
# Utilizando a base de criminalidade, faça um Shiny app que, dado um 
# mês/ano escolhido pelo usuário, mostre uma tabela com o número de ocorrências 
# de cada tipo que aconteceram naquele mês. 
# O nível territorial (Estado, região, município ou delegacia) também pode 
# ser um filtro.
# 

library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)
library(shinyWidgets)
# library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)

# df <- readRDS("Material-curso-r/dados/ssp.rds")
df <- readRDS("../dados/ssp.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "home", text = "Criminosos", icon = icon("home"))
  )),
  dashboardBody(
    fluidRow(
      # box(
      #   pickerInput(inputId = "data_ano",
      #               label = "Selecione o ano",
      #               choices = distinct(df, ano)),
      #               # choices = c(min(df$ano):max(df$ano))),
      #   
      #   pickerInput(inputId = "data_mes",
      #               label = "Selecione o mes",
      #               choices = distinct(df, mes)),
      #   actionBttn(inputId = "selecionar_data",
      #              label = "Selecionar data")
      # ),
      
      box(airMonthpickerInput(
        inputId = "date",
        label = "Selecione o ano/mês:",
        multiple = F, clearButton = TRUE,
        addon = "left"
      ),
      br(),
      h4("Tabela de crimes para o mês selecionado"),
      reactable::reactableOutput("table")),
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  teste=F
  histdata <- rnorm(500)
  # df_auxx <- shiny::eventReactive(input$selecionar_data, {
  #   # browser()
  #   df
  # })
  df_aux <- shiny::eventReactive(input$date, {
    if(teste){
      # input <- NULL
      input$date <- as.Date("2020-01-20")
      mes_lido <- lubridate::month(input$date)
      ano_lido <- lubridate::year(input$date)
    }else{
      mes_lido <- lubridate::month(input$date)
      ano_lido <- lubridate::year(input$date)
    }
    # browser()
    df_count <- df |> 
      dplyr::filter(ano == ano_lido, mes == mes_lido) |> 
      dplyr::select(-mes, -ano, -delegacia_nome, -municipio_nome, -regiao_nome)
    
    df_count_long <- df_count |> 
      tidyr::pivot_longer(cols = tidyr::everything()) |> 
      dplyr::mutate(tipo = gsub("_.*", "", name)) |> 
      dplyr::group_by(tipo) |> 
      dplyr::summarise(total = sum(value))
    
    df_count_long
  })
  output$table <- reactable::renderReactable({
    # browser()
    df_ <- df_aux()
    reactable::reactable(df_)
    
  })
}

shinyApp(ui, server)
