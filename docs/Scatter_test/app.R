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
library(ggpmisc)
library(tidyverse)
library(dplyr)





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
      
      summ_X <- paste("Mean of", input$VarX, ":", mean(mtcars[, c(input$VarX)]))

      summ_y <- paste("Mean of", input$VarY, ":", mean(mtcars[, c(input$VarY)]))
      
      max_y_low <- round(as.numeric(max(input$VarX) - 4), digits = 3)
      max_y_up <- round(as.numeric(max(input$VarY) - 2), digits = 3)
      
      gplot <-  ggplot(mtc, aes(mtc[,1], mtc[,2]), color = mtc[,1]) +
        geom_point() +
        geom_smooth(method = "lm") + 
        annotate(geom = "text", x = 15, y = max_y_up, label = summ_X,
                 color = "red") +
        annotate(geom = "text", x = 15, y = max_y_low, label = summ_y,
                 color = "red")
      
      gplot
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
