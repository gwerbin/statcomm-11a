library(shiny)

source("textArea.R")

fluidPage(
  fluidRow(inputTextArea("input_list",
"
test text
", nrows = 15, ncols = 80)),
  plotOutput("textOut"),
  theme = "bootstrap.css"
)

