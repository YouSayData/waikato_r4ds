library(shiny)

# your basic structure ----------------------------------------------------

ui <- fluidPage(
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText("Hello human!")
}

# Run the application 
shinyApp(ui = ui, server = server)
