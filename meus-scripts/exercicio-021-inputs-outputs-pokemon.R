# Explorando inputs
# 
# Utilizando a base de pokemon, faça um Shiny app que permite escolher
# um tipo (tipo_1) e uma geração e faça um gráfico de dispersão do ataque 
# vs defesa para os pokemon com essas características.



library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)
# library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)

df <- readRDS("Material-curso-r/dados/pkmn.rds")
# df <- readRDS("../dados/pkmn.rds")

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
  set.seed(122)
  histdata <- rnorm(500)
  df_aux <- shiny::eventReactive(input$tipo_1, {
    browser()
    df |> 
      dplyr::filter(tipo_1 == input$tipo_1)
  })
  output$plot_dispersao <- plotly::renderPlotly({
    browser()
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
