library(shiny)

shinyServer(function(input, output, session) {
  # Variável auxiliar
  aux <- reactiveValues()
    
  # Normal ---------------------------------------------------------------------
  # Calcula z1 e z2 padronizado e retorna a probabilidade entre eles
  rn <- reactive({normal(input$mu,input$sigma,input$x1,input$x2)[2:4]})
  
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
  
  # Descrição da Normal
  observeEvent(input$sobre,{
    switch (input$dist,
            normal = showModal(modalDialog(
              title = "Modelo Normal",
              "Distribuição densidade de probabilidade:",
              withMathJax(helpText("$$f(x)=\\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$")),
              "Propriedades:",
              div("i) f(x) é simétrica em relação à ",HTML("&mu;")),
              div("ii) f(x) -> 0 quando x", HTML("&rightarrow; &pm;  &infin;")),
              div("iii) o valor máximo de f(x) se dá para x = ",HTML("&mu;")),
              "Representação:",
              div(HTML("X &sim; N(&mu;,&sigma;<sup>2</sup>) e imediatamente segue-se que E(X) = &mu; e Var(X) = &sigma;<sup>2</sup>")),
              easyClose = TRUE,
              footer = a("Mais informações",href = "https://pt.wikipedia.org/wiki/Distribui%C3%A7%C3%A3o_normal")
              ))
    )
  })
  
  
  
  
  # Gráfico das distribuições
  output$distPlot <- renderPlot({
    validate(
      need(!(input$x1 >= input$x2), "x1 não pode ser maior que x2.")
    )
    switch(input$dist,
           normal = normal(input$mu,input$sigma,input$x1,input$x2)[[1]]
      )
  })
  
  output$formula <- renderUI({
    switch(input$dist,
           normal = withMathJax(helpText(paste0("$$P(",input$x1,"\\leq X \\leq",
              input$x2,")= P(\\frac{X1 - \\mu}{\\sigma} \\leq \\frac{X - \\mu}",
                "{\\sigma} \\leq \\frac{X2 - \\mu}{\\sigma})= P(\\frac{",
                input$x1,"-",input$mu,"}{",input$sigma,"}\\leq Z \\leq \\frac{",
                input$x2,"-",input$mu,"}{",input$sigma,"}) \\\\ P(",rn()[[1]]),
                "\\leq Z \\leq",rn()[[2]],")=",rn()[[3]],"$$")))
  })
  
  output$descricao <- renderUI({})
  
})
