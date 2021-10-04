# Meu primeiro shiny app
# 
# Faça um shiny app para visualizar histogramas da base diamonds 
# e o coloque no shinyapps.io.
# 
# Seu shiny deve ter um input e um output.
# 
# - Input: variáveis numéricas da base diamonds.
# - Output: histograma da variável selecionada.
# 
# Para acessar a base diamonds, carrrege o pacote ggplot2
# 
# library(ggplto2)
# 
# ou rode 
# 
# ggplot2::diamonds

library(shiny)
library(bs4Dash)
library(ggplot2)

func_get_numeric_columns <- function(df){
  col_names <- colnames(df)
  col_names_ <- ""
  j = 1
  for(i in 1:length(col_names)){
    if(is.numeric(df[[i]])){
      col_names_[j] <- col_names[i]
      j = j+1
    }
  }
}

## df usado
df <- ggplot2::diamonds
## traz apenas colunas numéricas
col_names <- func_get_numeric_columns(df)


ui <- bs4Dash::dashboardPage(
  title = "Primeiro exercício",
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
    plotOutput("hist_plot")
    
    
  )
)

server <- function(input, output, session){
  # summary(df)
  observeEvent(input$variavel, {
    # req(input$variavel)
    # validate(!is.numeric(df$carat), "Variável não numérica")
    output$hist_plot <- renderPlot(
      ggplot2::ggplot(df, ggplot2::aes(x = get(input$variavel))) +
        ggplot2::geom_histogram(stat = "count",
                                col="lightblue", 
                                fill="green", 
                                alpha = .2) + 
        ggplot2::labs(title= paste0("Histograma da variavel: ", input$variavel), x=input$variavel, y="Count") +
        ggplot2::theme_minimal()
      # xlim(c(18,52)) + 
      # ylim(c(0,30))
    )
    
  })
}

shinyApp(ui = ui, server = server)
