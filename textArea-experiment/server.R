library(shiny)

shinyServer(function (input, output, session) { 
  
  output$textOut <- renderPlot({
    plot.new()
    text(1/2, 1/2, input$input_list)
  })
  
})
