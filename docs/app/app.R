library(shiny)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Input for sample size
      numericInput(inputId = "sample_size", label = "Sample size:", value = 50),
      # Input for number of iterations
      numericInput(inputId = "n_iterations", label = "Number of iterations:", value = 100),
      # Input for p-value threshold
      numericInput(inputId = "p_value", label = "P-value threshold:", value = 0.05)
    ),
    mainPanel(
      # Output plot
      plotOutput(outputId = "plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Function to simulate t-test
  simulate_t_test <- function(sample_size, n_iterations, p_value) {
    # Create empty vectors to store results
    iteration <- numeric()
    p_value_result <- numeric()
    significant_result <- logical()
    
    # Loop over number of iterations
    for (i in 1:n_iterations) {
      # Generate random sample data
      data <- rnorm(sample_size)
      
      # Perform t-test
      t_test_result <- t.test(data)
      
      # Store iteration number, p-value, and significant result
      iteration[i] <- i
      p_value_result[i] <- t_test_result$p.value
      significant_result[i] <- t_test_result$p.value < p_value
      
      # Stop simulation if significant result is obtained
      if (any(significant_result)) {
        stop("Significant result obtained at iteration", which(significant_result)[1])
      }
    }
    
    # Return results as data frame
    data.frame(iteration = iteration, p_value = p_value_result, significant = significant_result)
  }
  
  # Render plot
  output$plot <- renderPlot({
    # Simulate t-test
    t_test_sim <- simulate_t_test(input$sample_size, input$n_iterations, input$p_value)
    
    # Plot p-values
    plot(t_test_sim$iteration, t_test_sim$p_value, type = "l", ylim = c(0, 1), xlab = "Iteration", ylab = "P-value")
    
    # Add line for p-value threshold
    abline(h = input$p_value, col = "red")
    
    # Add points for significant results
    points(t_test_sim$iteration[t_test_sim$significant], t_test_sim$p_value[t_test_sim$significant], col = "green", pch = 20)
    
    # Add text for iteration number of significant result
    if (any(t_test_sim$significant)) {
      text(x = which(t_test_sim$significant)[1], y = t_test_sim$p_value[which(t_test_sim$significant)[1]], label = paste0("Iteration ", which(t_test_sim$significant)[1]), pos = 3)
    }
  })
}

# Run app
shinyApp(ui, server)
