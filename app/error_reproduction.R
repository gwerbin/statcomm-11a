library(shiny)

server <- function(input, output) {
  e <- reactive({
    e <- new.env()
    e$text_input <- sprintf("input: %s", input$text)
    e$text_dummy <- "not the input"
    e
  })
  output$print_text <- renderPlot({
    text(1/2, 1/2, e()$text_input)
  })
  output
}
  
ui <- fluidPage(
  textInput("text", "text: ", value = "1"),
  plotOutput("print_text")
)
  
print(shinyApp(ui, server))
