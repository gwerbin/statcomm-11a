library(shiny)

server <- function(input, output) {
  
  txt_fn <- function(e){
#     browser()
    text(1/2, 1/2, e$text_input)
  }
  
  e <- reactive({
    e <- new.env()
    e$text_input <- sprintf("input: %s", input$text)
    e$text_dummy <- "not the input"
    e
  })
  output$print_text <- renderPlot({
    txt_fn(e())
  })
  output
}
  
ui <- fluidPage(
  textInput("text", "text: ", value = "1"),
  plotOutput("print_text")
)
  
print(shinyApp(ui, server))
