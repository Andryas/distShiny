library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Principais Modelos Discretos e Contínuos"),
    withMathJax(),
    sidebarLayout(
    sidebarPanel(
      selectInput("dist",label = "Escolha uma distribuição",
                   choices = c("Normal" = "normal")),
      fluidRow(
        column(width = 6,
               numericInput("mu", label = HTML("&mu;"), value = 0)),
        column(width = 6,
               numericInput("sigma", label = HTML("&sigma;"),1, min = 1))
        ),
      fluidRow(
        column(width = 5,
               numericInput("x1","",-3)),
        column(width = 2,
               HTML("<br /><div>&leq;X&leq;</div>")), # Arrumar a posição
        column(width = 5,
               numericInput("x2","",3))
      )
    ),
    mainPanel(
       plotOutput("distPlot"),
       uiOutput("formula") 
    )
  )
 )
)