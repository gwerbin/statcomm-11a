library(shiny)

# selectInput()

shinyUI(fluidPage(
  fluidRow(
    column(6, plotOutput("boxplots")),
    column(6, plotOutput("sampling_dist"))
  ),
  fluidRow(
    column(6, plotOutput("mean_squares")),
    column(6, plotOutput("f_stat"))
  )
))

