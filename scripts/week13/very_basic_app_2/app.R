library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Greet the human!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("user_name", "Human", value = "human")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("greeting")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$greeting <- renderText(paste("Hello", input$user_name))
}

# Run the application 
shinyApp(ui = ui, server = server)
