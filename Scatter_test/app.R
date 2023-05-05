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
    titlePanel("An interactive scatter plot to learn about linear regression"),

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
      
      mtc <- mtcars[, c(input$VarY, input$VarX)]
      
      lm_model <-  lm(mtc[,1] ~ mtc[,2])
      lm_model_sum <- summary(lm_model)
      
      cf <- lm_model_sum$coefficients
      Intercept <- round(cf[1], digits = 3)
      Slope <- round(cf[2], digits = 3)
      r_2 <-  round(lm_model_sum$r.squared, digits = 3)
      
      int <- paste("intercept", Intercept)
      slp <-  paste("slope: ", Slope)
      r2 <-  paste("R^2:", r_2)

      summ_y <- paste("Mean of", input$VarY, ":", mean(mtc[, 2]))
      
      max_y_low <- round(as.numeric(max(mtc[, 2]) * 1.4), digits = 3)
      max_y_up <- round(as.numeric(max(mtc[, 2]) * 1.2), digits = 3)
      
      max_x_up <- round(as.numeric(min(mtc[,1]) * 1.2), digits = 3)
      max_x_low <- round(as.numeric(min(mtc[, 1]) * 1.2), digits = 3)
      

      
      gplot <-  ggplot(mtc, aes(mtc[,1], mtc[,2])) +
        geom_point(aes(colour = factor("red"), alpha = .5)) +
        geom_smooth(method = "lm") + 
        annotate(geom = "text", x = max_x_up, y = max_y_up, 
                 label = paste("linear model's output", int, slp, r2, sep = "\n"),
                 color = "red") +
        
        xlab(paste("", input$VarX)) +
        ylab(paste("", input$VarY)) +
        theme_classic() +
        theme(legend.position = "none")
      
      gplot
      
      
      data(iris)
      ggplot_2 <-  ggplot(iris, aes(x=Petal.Width, y=Sepal.Width, z=Petal.Length, color=Species)) + 
        theme_void() +
        axes_3D() +
        stat_3D()
      
      ggplot_2
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
