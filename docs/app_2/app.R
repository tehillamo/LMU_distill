library(shiny)
library(plotly)

# Create example data
set.seed(123)
data <- data.frame(
  x = mtcars$mpg,
  y = mtcars$cyl,
  z = mtcars$hp
)

# Define UI for app
ui <- fluidPage(
  titlePanel("3D Scatter Plot with Best Fitting Line Regression"),
  sidebarLayout(
    sidebarPanel(
      # Add controls here, if desired
    ),
    mainPanel(
      # Add plot output here
      plotlyOutput("scatterplot")
    )
  )
)

# Define server logic for app
server <- function(input, output) {
  
  # Create scatter plot with best fitting line
  output$scatterplot <- renderPlotly({
    plot <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers") 
    
    # Add best fitting line
    model <- lm(z ~ x + y, data = data)
    model_data <- data.frame(x = range(data$x), y = range(data$y))
    model_data$z <- predict(model, newdata = model_data)
    plot <- plot %>% add_trace(x = model_data$x, y = model_data$y, z = model_data$z, 
                               line = list(width = 10, dash = "dash", color = "red"), 
                               type = "scatter3d", mode = "lines", name = "Best Fitting Line")
    
    
    # Set layout options
    plot <- plot %>% layout(scene = list(xaxis = list(title = paste("Age")),
                                         yaxis = list(title = paste("Salary")),
                                         zaxis = list(title = paste("Number of Cats"))))
    plot
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
