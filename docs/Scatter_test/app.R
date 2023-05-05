#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("An interactive scatter plot for teaching"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "VarX",
                        label = "Select X-axis Variable:",
                        choices = list("mpg",
                                        "disp", "hp",
                                        "drat", "wt", "qsec", "carb")),
            selectInput(inputId = "VarY",
                        label = "Select Y-axis Variable:",
                        choices = list("mpg",
                                       "disp", "hp",
                                       "drat", "wt", "qsec", "carb"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Scatter")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Scatter <- renderPlot({
      
      mtc <- mtcars[, c(input$VarX, input$VarY)]
      
      gplot <-  ggplot(mtc, aes(mtc[,1], mtc[,2])) +
        geom_point() +
        ggplot2::geom_smooth(method = "lm")
      
      gplot
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
