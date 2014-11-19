library(shiny)
library(dplyr)

shinyServer(function (input, output, session) { 
  
  source("anova.R")
  
  input_list <- reactive({
#     catn <- function(x) { cat(x); cat("\n"); x }
    input_list <- input$input_list %>%
      gsub("#.*?\n", "", .) %>%# catn %>%
      gsub("\n", ",", ., fixed = TRUE) %>%# catn %>%
      gsub(",,+", ",", .) %>%# catn %>%
      gsub("(,$)|((\\s+|\n)$)", "", .) %>%# catn %>%
      sprintf("list(%s)", .) %>%# catn
#     cat(input_list)
    stopApp()
    eval(parse(text = input_list))
  })
  n_rep <- reactive(as.numeric(input$n_rep))
  alpha <- reactive(as.numeric(input$alpha))
  
  data_env <- reactive(generate_data(input_list(), n_rep(), alpha()))
  
  output$boxplots <- renderPlot(data_boxplot(data_env()))
  output$sampling_dist <- renderPlot(sampling_distribution_plot(data_env()))
  output$mean_squares <- renderPlot(mean_squares_plot(data_env()))
  output$f_stat <- renderPlot(f_stat_plot(data_env()))
  
})
