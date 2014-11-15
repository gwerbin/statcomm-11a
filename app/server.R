library(shiny)

source("./anova.R")

shinyServer(function (input, output, session) { 
  
#   observe({
#     
#   })
#   
#   reactive({
#     
#   })
  
  output$boxplots <- renderPlot(data_boxplot())
  output$sampling_dist <- renderPlot(sampling_distribution_plot())
  output$mean_squares <- renderPlot(mean_squares_plot())
  output$f_stat <- renderPlot(f_stat_plot())
  
})
