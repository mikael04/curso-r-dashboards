# Explorando inputs
# 
# Utilizando a base de crédito, faça um Shiny app que permite escolher
# o estado civil, tipo de moradia e/ou trabalho e mostra uma visualização
# representando a proporção de clientes "bons" e "ruins" da base.


# df <- readRDS("../dados/credito.rds")
# dplyr::glimpse(df)
# skimr::skim(df)
library(shiny)
library(dplyr)
library(bs4Dash)
library(ggplot2)

# df <- readRDS("Material-curso-r/dados/credito.rds")
df <- readRDS("../dados/credito.rds")

choices_estado_civil <- df |> 
  dplyr::filter(!is.na(estado_civil)) |> 
  dplyr::select(estado_civil) |> 
  dplyr::distinct(estado_civil) |> 
  pull()

choices_moradia <- df |> 
  dplyr::filter(!is.na(moradia)) |> 
  dplyr::select(moradia) |> 
  dplyr::distinct(moradia) |> 
  pull()

choices_trabalho <- df |> 
  dplyr::filter(!is.na(trabalho)) |> 
  dplyr::select(trabalho) |> 
  dplyr::distinct(trabalho) |> 
  pull()


ui <- bs4Dash::dashboardPage(
  title = "Segundo exercício",
  header = dashboardHeader(),
  sidebar = bs4Dash::dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = bs4Dash::dashboardBody(
    # shinyWidgets::pickerGroupUI()
    # shiny::selectInput(inputId = "variavel",
    #                    label = "Escolha a variável",
    #                    choices = names(df),
    #                    multiple = F,
    #                    selectize = T,
    #                    selected = NULL
    # ),
    fluidRow(
      selectInput(inputId = "estado_civil",
                  label = "Selecione o estado civil",
                  choices = choices_estado_civil,
                  selectize = T,
                  selected = "casada(o)",
                  multiple = F
      ),
      selectInput(inputId = "moradia",
                  label = "Selecione o tipo de moradia",
                  choices = choices_moradia,
                  selectize = T,
                  selected = "alugada",
                  multiple = F
      ),
      selectInput(inputId = "trabalho",
                  label = "Selecione o tipo de trabalho",
                  choices = choices_trabalho,
                  selectize = T,
                  selected = "autônomo",
                  multiple = F
      )
    ),
    fluidRow(
      plotly::plotlyOutput("hist_plot")
    )
  )
)

server <- function(input, output, session){
  # summary(df)
  # observeEvent(input$variavel, {
  # req(input$variavel)
  # input <- NULL
  # input$variavel <- "status"
  df_ <- reactive({
    df |>
      dplyr::filter(estado_civil == input$estado_civil, moradia == input$moradia,
                    trabalho == input$trabalho)
  })
  # validate(!is.numeric(df$carat), "Variável não numérica")
  output$hist_plot <- plotly::renderPlotly({
    # browser()
    if(is.null(input$estado_civil) | is.null(input$moradia) | is.null(input$trabalho)){
      
    }else{
      df_aux <- df_()
      df_aux <- df_aux |> 
        dplyr::group_by(status) |> 
        dplyr::summarise(count_ = n()) |>
        dplyr::ungroup()
    }
    plot <- ggplot2::ggplot(df_aux, aes(x = status, y = count_)) +
      ggplot2::geom_bar(stat="identity",
                        fill="lightgreen") +
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


shinyApp(ui = ui, server = server)
