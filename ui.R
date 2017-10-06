library(shiny)


# Reseta o ID 
# jscode <- "Shiny.addCustomMessageHandler('resetValue', function(variableName) {
#                 Shiny.onInputChange(variableName, null);
#             });"

shinyUI(
  fluidPage(
    titlePanel("Principais Modelos Discretos e Contínuos"),
    withMathJax(),
    sidebarLayout(
    sidebarPanel(
      # tags$head(tags$script(jscode)),
      fluidRow(
        column(width = 8,
               div(style = "height:64px;",
                 selectInput("dist",label = "Escolha uma distribuição",
                   choices = c("Normal" = "normal")))),
        column(width = 4, align = "center",
               div(style ="height:64px; margin-top: 26px",
                 actionButton("sobre","",icon("book"),width = "100%")))
        ),
      conditionalPanel(condition = "input.dist == 'normal'",
                       fluidRow(
                         column(width = 6,
                               numericInput("mu", label = HTML("<div style='width: 164px;'><center>&mu;</center></div>"), value = 0)),
                         column(width = 6,
                               numericInput("sigma", label = HTML("<div style='width: 164px;'><center>&sigma;</center></div>"),1, min = 1))
                         ),
                       fluidRow(
                         column(width = 5,
                               numericInput("x1",HTML("<div style='width: 132px;'><center>X<sub>1</sub></center></div>"),-3)),
                         column(width = 2,
                               HTML("<div style='margin-top:26px; font-size:20px;'>&leq;X&leq;</div>")), # Arrumar a posição
                         column(width = 5,
                               numericInput("x2",HTML("<div style='width: 132px;'><center>X<sub>2</sub></center></div>"),3))
                        ))
    ),
    mainPanel(
       plotOutput("distPlot"),
       uiOutput("formula") 
    )
  )
 )
)