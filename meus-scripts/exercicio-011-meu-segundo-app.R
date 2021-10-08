# Meu segundo shiny app (agora importando uma base de dados)
# 
# Escolha uma das bases da pasta dados ou use uma base própria.
# 
# - Crie um shiny app com pelo menos um input e um output 
# para visualizarmos alguma informação interessante da base.
# 
# - Suba o app para o shinyapps.io.
# 
# Observação: se você usar uma base própria, 
# não se esqueça de descrever as variáveis utilizadas na hora 
# de tirar dúvidas.

# df <- readRDS("../dados/credito.rds")
df <- readRDS("Material-curso-r/dados/credito.rds")

# dplyr::glimpse(df)
# skimr::skim(df)
library(shiny)
library(bs4Dash)
library(ggplot2)

ui <- bs4Dash::dashboardPage(
  title = "Segundo exercício",
  header = dashboardHeader(),
  sidebar = bs4Dash::dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = bs4Dash::dashboardBody(
    # shinyWidgets::pickerGroupUI()
    shiny::selectInput(inputId = "variavel",
                       label = "Escolha a variável",
                       choices = names(df),
                       multiple = F,
                       selectize = T,
                       selected = NULL
    ),
    plotly::plotlyOutput("hist_plot")
    
    
  )
)

server <- function(input, output, session){
  # summary(df)
  # observeEvent(input$variavel, {
    # req(input$variavel)
    # input <- NULL
    # input$variavel <- "status"
    # validate(!is.numeric(df$carat), "Variável não numérica")
    output$hist_plot <- plotly::renderPlotly({
      plot <- ggplot2::ggplot(df, ggplot2::aes(x = get(input$variavel))) +
        ggplot2::geom_histogram(stat = "count",
                                col="lightblue", 
                                fill="green", 
                                alpha = .2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title= paste0("Histograma da variavel: ",
                                    input$variavel), x=input$variavel, y="Count")
      
      
      plotly::ggplotly(plot)
      # xlim(c(18,52)) + 
      # ylim(c(0,30))
    # )}
    
  })
}


shinyApp(ui = ui, server = server)
