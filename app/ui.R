library(shiny)

source("textArea.R")

fluidPage(
  titlePanel("Under the hood of ANOVA"),
  fluidRow(
    column(6, plotOutput("boxplots")),
    column(6, plotOutput("sampling_dist"))
  ),
  fluidRow(
    column(6, plotOutput("mean_squares")),
    column(6, plotOutput("f_stat"))
  ),
  fluidRow(
    column(8,
           inputTextArea("input_list", "
# list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0 + .6, sd = 1)
list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0, sd = 1)
list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0, sd = 1)
list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0, sd = 1)
list(rng = rt, inv_F = qt, n = 15, df = 2, ncp = 1)
                         ", nrows = 15, ncols = 80)
    ),
    column(4,
           textInput("n_rep", "Number of simulations:", 5000),
           textInput("alpha", "Alpha level of F test:", 0.05)
    )
  ),
  theme = "bootstrap.css"
)

