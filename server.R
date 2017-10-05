library(shiny)

shinyServer(function(input, output, session) {
  # Variável auxiliar
  aux <- reactiveValues()
    
  # Normal ---------------------------------------------------------------------
  # Calcula z1 e z2 padronizado e retorna a probabilidade entre eles
  rn <- reactive({pnormal(input$mu,input$sigma,input$x1,input$x2)})
  
  # Valor reativo para média da normal
  mun <- reactive({input$mu})
  
  observe({
    # Este 'se' so serve para a primeira iteração
    if(is.null(aux$mu)) {aux$mu <- mun()}
    
    # Caso haja uma alteração de input$mu, as arrow's mudam igualmente de -3 a 3.
    if(aux$mu != input$mu) {
      updateNumericInput(session,"x1",label = "",value = input$mu - 3 * input$sigma)
      updateNumericInput(session,"x2",label = "",value = input$mu + 3 * input$sigma)
      aux$mu <- input$mu 
    }
  })
  
  # Gráfico das distribuições
  output$distPlot <- renderPlot({
    validate(
      need(!(input$x1 >= input$x2), "x1 não pode ser maior que x2.")
    )
    switch(input$dist,
           normal = dnormal(input$mu,input$sigma,input$x1,input$x2)
      )
  })
  
  output$formula <- renderUI({
    withMathJax(helpText(paste0("$$P(",input$x1,"\\leq X \\leq",input$x2,")=",
                "P(\\frac{",input$x1,"- \\mu}{\\sigma} \\leq \\frac{X - \\mu}",
                "{\\sigma} \\leq \\frac{",input$x2," - \\mu}{\\sigma})= P(\\frac{",
                input$x1,"-",input$mu,"}{",input$sigma,"}\\leq Z \\leq \\frac{",
                input$x2,"-",input$mu,"}{",input$sigma,"}) \\\\ P(",rn()[[1]]),
                "\\leq Z \\leq",rn()[[2]],")=",rn()[[3]],"$$"))
  })
  
  output$descricao <- renderUI({})
  
})
