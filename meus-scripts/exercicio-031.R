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
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(fresh)
# library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)

# df <- readRDS("Material-curso-r/dados/cetesb.rds")
df <- readRDS("../Material-curso-r/dados/cetesb.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Cetesb"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(id = "menu_1",
               tabName = "med_oz", text = "Média diária Oz", icon = icon("home"),
               airDatepickerInput(
                 inputId = "range",
                 label = "Selecione o período de datas:",
                 range = TRUE
               )),
      menuItem(id = "menu_2",
               tabName = "med_mes_ant", text = "Média mês anterior", icon = icon("tree"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "med_oz",
        fluidRow(
          br(),
          box(
            h4("Série temporal para período selecionado"),
            plotly::plotlyOutput("serie")
          )
        )
      ),
      tabItem(
        tabName = "med_mes_ant",
        br(),
        box(
          h4("Série temporal 2 para período selecionado"),
        )
      ),
      tabItem(
        tabName = "pag3",
        h2("Conteúdo da página 3")
      ),
      tabItem(
        tabName = "pag4",
        h2("Conteúdo da página 4")
      ),
      tabItem(
        tabName = "pag5",
        h2("Conteúdo da página 5")
      ),
      tabItem(
        tabName = "pag6",
        h2("Conteúdo da página 6")
      )
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
  teste = F
  if(teste){
    input <- NULL
    input$range[1] = "2019-01-01"
    input$range[2] = "2020-01-01"
  }
  df_aux <- shiny::eventReactive(input$range, {
    if(!is.null(input$range) & length(input$range) > 1){
      # browser()
      df_mean <- df |> 
        dplyr::filter(!is.na(concentracao)) |> 
        dplyr::filter(as.Date(input$range[1]) <= data & data <= as.Date(input$range[2])) |> 
        dplyr::group_by(data) |> 
        dplyr::summarise(mean = mean(concentracao)) |> 
        dplyr::ungroup()
      df_mean
      # df |> 
      #   dplyr::filter(as.Date(input$range[1]) <= as.Date(data))
    }else{
      df |> 
        dplyr::filter(!is.na(concentracao)) |> 
        dplyr::group_by(data) |> 
        dplyr::summarise(mean = mean(concentracao)) |> 
        dplyr::ungroup()
    }
  })
  output$serie <- plotly::renderPlotly({
    # browser()
    df_ <- df_aux()
    # browser()
    plot <- ggplot2::ggplot(df_, aes(x = data, y = mean)) +
      geom_line()+
      xlab("Data") +
      theme_minimal()
    plotly::ggplotly(plot)
    
  })
}

shinyApp(ui, server)

