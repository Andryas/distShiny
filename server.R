library(shiny)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    switch(input$dist,
           normal = fnormal(input$mu,input$sigma,input$x1,input$x2)
      )
  })  
})
