library(shiny)

shinyServer(function (input, output, session) { 
  
  source("anova.R")
  
  input_list <- reactive({
    input_list <- input$input_list %>%
      gsub("#.*?\n", "", .) %>%
      gsub("\n", ",", ., fixed = TRUE) %>%
      gsub(",\\s+$", "", .) %>%
      sprintf("list(%s)", .)
    eval(parse(text = input_list))
  })
  n_rep <- reactive(input$n_rep)
  alpha <- reactive(input$alpha)
  
  data_env <- reactive(generate_data(input_list(), n_rep(), alpha()))
  
  output$boxplots <- renderPlot(data_boxplot(data_env()))
  output$sampling_dist <- renderPlot(sampling_distribution_plot(data_env()))
  output$mean_squares <- renderPlot(mean_squares_plot(data_env()))
  output$f_stat <- renderPlot(f_stat_plot(data_env()))
  
})
