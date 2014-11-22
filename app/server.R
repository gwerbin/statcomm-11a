library(shiny)
library(dplyr)

shinyServer(function (input, output, session) { 
  
  # to do / would be nice to have:
  # - save previous plot in another tab
  # - button to refresh app instead of constant refresh when it finds text
  # - 
  
  source("anova.R")
  
  input_list <- reactive({
    input_list <- input$input_list %>%
      gsub("#.*?\n", "", .) %>%
      gsub("\n", ",", ., fixed = TRUE) %>%
      gsub(",,+", ",", .) %>%
      gsub("(,$)|((\\s+|\n)$)", "", .) %>%
      sprintf("list(%s)", .)
    eval(parse(text = input_list))
  })
  n_rep <- reactive(as.numeric(input$n_rep))
  alpha <- reactive(as.numeric(input$alpha))
  
  data_env <- reactive({
    withProgress(
      message = "Generating data", value = 0, {
      out <<- generate_data(input_list(), n_rep(), parallel = FALSE)
      setProgress(value = 1)
      out
    })
  })
  
  output$boxplots <- renderPlot(data_boxplot(data_env()))
  output$sampling_dist <- renderPlot(sampling_distribution_plot(data_env()))
  output$mean_squares <- renderPlot(mean_squares_plot(data_env()))
  output$f_stat <- renderPlot(f_stat_plot(data_env(), alpha()))
  
})
