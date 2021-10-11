# navbarPage
# 
# Utilizando a base cetesb, faça um shiny app que tenha duas abas:
#
# - a primeira com uma série temporal da média diária do ozônio (O3),
# permitindo a escolha do intervalo de dias em que o gráfico é gerado
#
# - a segunda com a série temporal da média diária do último mês da base 
# permitindo a escolha do poluente.


library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)
# library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)

df <- readRDS("Material-curso-r/dados/cetesb.rds")
# df <- readRDS("../dados/cetesb.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "home", text = "Filtrar pokemao", icon = icon("home"),
             shiny::selectInput(inputId = "tipo_1",
                                label = "Tipo do pkm",
                                choices = distinct(df, tipo_1),
                                selectize = T,
                                multiple = F
             ))
  )),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot_dispersao")),
    )
  )
)

server <- function(input, output) {
  
  df_aux <- shiny::eventReactive(input$tipo_1, {
    # browser()
    df |> 
      dplyr::filter(tipo_1 == input$tipo_1)
  })
  output$plot_dispersao <- plotly::renderPlotly({
    # browser()
    df_ <- df_aux()
    plot <- ggplot2::ggplot(df_, aes(x = ataque, y = defesa)) +
      ggplot2::geom_point(size = 2, shape = 23, fill="lightgreen") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title= paste0("Contagem de status por agrupamento: "),
                    x= "Status", y="Contagem")
    
    # plot
    plotly::ggplotly(plot)
    # xlim(c(18,52)) + 
    # ylim(c(0,30))
    # )}
    
  })
}

shinyApp(ui, server)
