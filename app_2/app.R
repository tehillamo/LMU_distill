library(shiny)
library(plotly)

# Create example data
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  z = rnorm(100)
)

# Define UI for app
ui <- fluidPage(
  titlePanel("3D Scatter Plot with Best Fitting Line Regression"),
  sidebarLayout(
    sidebarPanel(
      # Add dropdown menu for choosing columns
      selectInput(inputId = "x_col", label = "X-axis:", choices = names(data)),
      selectInput(inputId = "y_col", label = "Y-axis:", choices = names(data)),
      selectInput(inputId = "z_col", label = "Z-axis:", choices = names(data))
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
    # Get selected columns from dropdown menu
    x_col <- input$x_col
    y_col <- input$y_col
    z_col <- input$z_col
    
    # Create scatter plot
    scatterplot <- plot_ly(data, x = ~get(x_col), y = ~get(y_col), z = ~get(z_col),
                           type = "scatter3d", mode = "markers")
    
    # Add best fitting line
    model <- lm(as.formula(paste(z_col, "~", x_col, "+", y_col)), data = data)
    model_data <- data.frame(x = seq(min(data[,x_col]), max(data[,x_col]), length.out = 30),
                             y = seq(min(data[,y_col]), max(data[,y_col]), length.out = 30))
    model_data$z <- outer(model_data$x, model_data$y, function(x, y) {predict(model, newdata = data.frame(x = x, y = y))})
    
    # Add surface plot of best fitting line
    surface <- plot_ly(model_data, x = ~x, y = ~y, z = ~z, type = "surface",
                       colorscale = "Viridis", opacity = 0.8, showscale = FALSE)
    
    # Set layout options for scatter plot
    scatterplot <- scatterplot %>% layout(scene = list(xaxis = list(title = x_col),
                                                       yaxis = list(title = y_col),
                                                       zaxis = list(title = z_col)))
    
    # Combine scatter plot and surface plot
    plot <- subplot(scatterplot, surface, nrows = 1, heights = c(0.7, 0.3))
    
    plot
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
