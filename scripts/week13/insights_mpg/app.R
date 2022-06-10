
# libraries ----------------------------------------------------

library(tidyverse)
library(shiny)

# ui ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fuell efficiency"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("class",
                        "Class of cars:",
                        mpg %>% pull(class))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("fuelPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$fuelPlot <- renderPlot({
        mpg %>% 
        filter(class == input$class) %>%
        ggplot(aes(displ, cty)) +
        geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
