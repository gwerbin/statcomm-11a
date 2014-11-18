library(shiny)

shinyServer(function (input, output, session) { 
  
  input_list <- reactive(eval(parse(text = input$input_list)))
  n_rep <- reactive(input$n_rep)
  alpha <- reactive(input$alpha)
  
  plot_env <- reactive({
    source("anova.R")
    plots$d <- plots$generate_data(input_list, n_rep, alpha)
    plots
  })
  
  output$boxplots <- renderPlot(plot_env()$data_boxplot(plot_env()$d))
  output$sampling_dist <- renderPlot(plot_env()$sampling_distribution_plot(plot_env()$d))
  output$mean_squares <- renderPlot(plot_env()$mean_squares_plot(plot_env()$d))
#     {plot.new(); text(0, 0, length(y_rep))})
  output$f_stat <- renderPlot(plot_env()$f_stat_plot(plot_env()$d))
  
})
