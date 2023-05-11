library(shiny)
library(mvtnorm)
library(MASS)

# Define UI ----
ui <- fluidPage(
  titlePanel("Bivariate Normal Distribution"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mu1", "Mean of X:", 0, min = -10, max = 10),
      numericInput("mu2", "Mean of Y:", 0, min = -10, max = 10),
      numericInput("sd1", "Standard Deviation of X:", 1, min = 0.1, max = 10),
      numericInput("sd2", "Standard Deviation of Y:", 1, min = 0.1, max = 10),
      numericInput("rho", "Correlation Coefficient:", 0, min = -1, max = 1),
      selectInput("color", "Color Scheme:",
                  choices = c("Red", "Blue", "Green", "Rainbow", "Heat", "Terrain")),
      plotOutput("plot")
    ),
    mainPanel(
      tableOutput("summary")
    )
  )
)

# Define server ----
server <- function(input, output) {
  # Generate data based on user input
  data <- reactive({
    mu <- c(input$mu1, input$mu2)
    sigma <- matrix(c(input$sd1^2, input$rho * input$sd1 * input$sd2, 
                      input$rho * input$sd1 * input$sd2, input$sd2^2), 
                    nrow = 2, ncol = 2)
    x <- seq(-10, 10, length.out = 100)
    y <- seq(-10, 10, length.out = 100)
    z <- matrix(0, nrow = length(x), ncol = length(y))
    for (i in 1:length(x)) {
      for (j in 1:length(y)) {
        z[i, j] <- dmvnorm(c(x[i], y[j]), mean = mu, sigma = sigma)
      }
    }
    list(x = x, y = y, z = z)
  })
  
  # Create plot based on user input
  output$plot <- renderPlot({
  
    heat.colors.density <- function(n, max_density) {
      # Create a sequence of values from 0 to 1
      x <- seq(0, 1, length.out = n)
      # Define a custom color palette that increases towards the highest density
      colors <- cbind(x, sqrt(x), x^3)
      # Normalize the colors so that they range from 0 to 1
      colors <- apply(colors, 2, function(x) (x - min(x)) / (max(x) - min(x)))
      # Scale the colors to the maximum density value
      colors <- apply(colors, 2, function(x) x * max_density)
      # Convert the colors to RGB format
      colors <- apply(colors, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
      # Return the color palette
      colors
    }
    # Estimate the 2-dimensional kernel density using kde2d
    dens <- kde2d(seq(-10, 10, length.out = 100), seq(-10, 10, length.out = 100), 100)
    
    # Find the maximum density value in the grid
    max_density <- max(dens$z)
    
    color_scheme <- switch(input$color,
                           "Red" = heat.colors.density(100, max_density),
                           "Blue" = topo.colors(10)[10:1],
                           "Green" = terrain.colors(10)[10:1],
                           "Rainbow" = rainbow(10),
                           "Heat" = heat.colors.density(10, max_density),
                           "Terrain" = terrain.colors(10))
    
    
  persp(data()$x, data()$y, data()$z, theta = 30, phi = 30, 
        xlab = "X", ylab = "Y", zlab = "Density",
        col = color_scheme)
   })
  # Crte table summary based on user input
  output$summary <- renderTable({
    mu <- c(input$mu1, input$mu2)
    sigma <- matrix(c(input$sd1^2, input$rho * input$sd1 * input$sd2, 
                      input$rho * input$sd1 * input$sd2, input$sd2^2), 
                    nrow = 2, ncol = 2)
    cov <- sigma * input$rho
    colnames(cov) <- c("X", "Y")
    rownames(cov) <- c("X", "Y")
    data.frame("Mean" = mu, "Covariance Matrix" = cov)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
