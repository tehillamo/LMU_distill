library(shiny)
library(ggplot2)
library(pwr)

ui <- fluidPage(
  titlePanel("Sample size simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sample_size", "Starting sample size:", value = 10, min = 1, max = 1000, step = 10),
      sliderInput("effect_size", "Effect size:", value = 0.5, min = 0, max = 1, step = 0.1),
      sliderInput("p_value", "p-value threshold:", value = 0.1, min = 0.01, max = 0.5, step = 0.01),
      sliderInput("stat_power", "Statistical power threshold:", value = 0.8, min = 0.5, max = 0.99, step = 0.01)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  # Function to calculate p-value and statistical power
  calc_stats <- function(n) {
    # Generate random data
    set.seed(123)
    group1 <- rnorm(n, mean = 0, sd = 1)
    group2 <- rnorm(n, mean = input$effect_size, sd = 1)
    # Perform t-test
    t_test <- t.test(group1, group2)
    # Calculate p-value and statistical power
    p_value <- t_test$p.value
    power <- pwr.t.test(n = n, d = input$effect_size, sig.level = input$p_value, power = NULL)$power
    return(c(p_value, power))
  }
  
  # Reactive expression to generate results data frame
  results_df <- reactive({
    df <- data.frame(num_observations = numeric(),
                     value = numeric(),
                     variable = character(),
                     stringsAsFactors = FALSE)
    last_sig_n <- NA
    last_power_n <- NA
    # Iterate over sample sizes until significant p-value or statistical power is reached
    for (n in seq(from = input$sample_size, to = 1000, by = 10)) {
      stats <- calc_stats(n)
      # Stop iterating if either threshold is reached
      if (stats[1] <= input$p_value & is.na(last_sig_n)) {
        last_sig_n <- n
      }
      if (stats[2] >= input$stat_power & is.na(last_power_n)) {
        last_power_n <- n
      }
      if (!is.na(last_sig_n) & !is.na(last_power_n)) {
        break
      }
      # Add results to data frame
      df <- rbind(df, data.frame(num_observations = n,
                                 value = stats[1],
                                 variable = "p-value",
                                 stringsAsFactors = FALSE))
      df <- rbind(df, data.frame(num_observations = n,
                                 value = stats[2],
                                 variable = "power",
                                 stringsAsFactors = FALSE))
    }
    df$last_sig_n <- last_sig_n
    df$last_power_n <- last_power_n
    return(df)
  })
  
  # Plot results
  output$plot <- renderPlot({
    ggplot(results_df(), aes(x = num_observations, y = value, group = variable, color = variable)) +
      geom_line() +
      scale_color_manual(values = c("p-value" = "red", "power" = "blue")) +
      xlab("Sample size") +
      ylab("p-value / power")
    