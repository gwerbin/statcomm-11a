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
n_group <- 15
m_group <- 0
sd_group <- 1

input_list <- list(
  # list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group + .6, sd = sd_group),
  list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group, sd = sd_group),
  list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group, sd = sd_group),
  list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group, sd = sd_group),
  list(rng = rt, inv_F = qt, n = n_group, df = 2, ncp = 1)
)
", nrows = 15, ncols = 80)
    ),
    column(4,
           textInput("n_rep", "Number of simulations:", 5000),
           textInput("alpha", "Alpha level of F test:", 0.5)
    )
  ),
  theme = "bootstrap.css"
)

