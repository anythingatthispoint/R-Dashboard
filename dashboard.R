# Installs all the needed libraries
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("caTools", quietly = TRUE)) install.packages("caTools")

#Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(tidyverse)
library(GGally)
library(gridExtra)
library(caTools)

# The dashboard
ui <- dashboardPage( 
  dashboardHeader(title = "Final Project Dashboard"),
  dashboardSidebar(
    # custom CSS File
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Optionally, link Google Fonts for 'Open Sans'
      tags$link(href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600;700&display=swap", rel = "stylesheet")
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Raw Data", tabName = "raw_data", icon = icon("table"))
    ),
    # Wrap inputs in a div for consistent styling
    div(class = "input-section", # Custom class for styling
        selectInput("plot_column", "Select Column for Univariate Plot:",
                    choices = c("")),
        sliderInput("bins",
                    "Number of Bins (for numeric data):",
                    min = 1,
                    max = 50,
                    value = 30)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Exploratory Data Analysis Findings"),
              fluidRow(
                box(
                  title = "Univariate Distribution Plot", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("univariate_plot")
                ),
                box(
                  title = "Univariate Summary Statistics", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  verbatimTextOutput("univariate_summary_stats")
                )
              ),
              fluidRow(
                box(
                  title = "Correlation Matrix of Numerical Variables", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("correlation_matrix_plot")
                ),
                box(
                  title = "Scatterplot Matrix", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  plotOutput("scatterplot_matrix_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Discovery 1: Like Count vs. Buzz (Near-Perfect Correlation)", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("discovery1_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Discovery 2: Follower Counts by State", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("discovery2_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Discovery 3: Skewed Distribution of 'i'", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("discovery3_plot")
                )
              ),
              h3("Model Development and Evaluation"),
              fluidRow(
                box(
                  title = "Regression Model Summary", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  verbatimTextOutput("model_summary_output")
                ),
                box(
                  title = "Model Evaluation Metrics", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  verbatimTextOutput("model_metrics_output")
                )
              ),
              fluidRow(
                box(
                  title = "Prediction Accuracy: Actual vs. Predicted Buzz (Split View)", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("prediction_accuracy_plot")
                )
              )
      ),
      tabItem(tabName = "raw_data",
              h2("Raw Data Table"),
              fluidRow(
                box(
                  title = "Dataset Preview", status = "success", solidHeader = TRUE,
                  width = 12,
                  DTOutput("data_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(file.exists("EDA_table_processed.csv"))
    read.csv("EDA_table_processed.csv")
  })
  
  var_types <- reactive({
    df <- data()
    numeric_vars <- df %>% select(where(is.numeric)) %>% names()
    categorical_vars <- df %>% select_if(function(col) !is.numeric(col) | is.factor(col) | is.character(col)) %>% names()
    list(numeric = numeric_vars, categorical = categorical_vars)
  })
  
  observe({
    df <- data()
    if (!is.null(df)) {
      updateSelectInput(session, "plot_column",
                        choices = names(df))
    }
  })
  
  output$univariate_plot <- renderPlot({
    df <- data()
    req(input$plot_column)
    
    selected_col <- df[[input$plot_column]]
    
    if (is.numeric(selected_col)) {
      ggplot(df, aes(x = .data[[input$plot_column]])) +
        geom_histogram(aes(y = ..density..), bins = input$bins, fill = "#3498db", color = "white", alpha = 0.8) +
        geom_density(color = "#e74c3c", size = 1) +
        labs(title = paste("Distribution of", input$plot_column),
             x = input$plot_column,
             y = "Density") +
        theme_minimal()
    } else {
      ggplot(df, aes(x = fct_infreq(.data[[input$plot_column]]))) +
        geom_bar(fill = "#2ecc71", color = "white", alpha = 0.9) +
        geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 4) +
        labs(title = paste("Frequency Distribution of", input$plot_column),
             x = input$plot_column,
             y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$univariate_summary_stats <- renderPrint({
    df <- data()
    req(input$plot_column)
    summary(df[[input$plot_column]])
  })
  
  output$correlation_matrix_plot <- renderPlot({
    df <- data()
    numeric_vars <- var_types()$numeric
    req(length(numeric_vars) > 1)
    
    ggcorr(df[, numeric_vars],
           label = TRUE,
           label_size = 3.5,
           label_round = 2,
           hjust = 0.8,
           size = 4,
           layout.exp = 2,
           name = "Correlation") +
      labs(title = "Correlation Matrix of Numerical Variables") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$scatterplot_matrix_plot <- renderPlot({
    df <- data()
    numeric_vars <- var_types()$numeric
    categorical_vars <- var_types()$categorical
    req(length(numeric_vars) > 1)
    
    selected_vars_for_pairs <- head(numeric_vars, 5)
    
    if (length(categorical_vars) > 0) {
      ggpairs(df, columns = selected_vars_for_pairs, aes(color = .data[[categorical_vars[1]]], alpha = 0.7)) +
        labs(title = paste("Scatterplot Matrix Colored by", categorical_vars[1])) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      ggpairs(df, columns = selected_vars_for_pairs) +
        labs(title = "Scatterplot Matrix") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$discovery1_plot <- renderPlot({
    df <- data()
    ggplot(df, aes(x = like_count, y = Buzz)) +
      geom_point(alpha = 0.6, color = "#2980b9") +
      geom_smooth(method = "lm", color = "#c0392b", se = FALSE) +
      labs(title = "Near-Perfect Correlation Between Like Count and Buzz",
           subtitle = "The relationship is almost perfectly linear, indicating redundancy.",
           x = "Like Count",
           y = "Buzz Score") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$discovery2_plot <- renderPlot({
    df <- data()
    ggplot(df, aes(x = reorder(State, Followers, FUN = median), y = Followers)) +
      geom_boxplot(fill = "#1abc9c", alpha = 0.8) +
      labs(title = "Follower Counts Show Significant Variation by State",
           subtitle = "Certain states have a notably higher median and spread of followers.",
           x = "State",
           y = "Number of Followers") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$discovery3_plot <- renderPlot({
    df <- data()
    ggplot(df, aes(x = i)) +
      geom_histogram(bins = 50, fill = "#f1c40f", alpha = 0.9) +
      labs(title = "The Distribution of Variable 'i' is Highly Skewed",
           subtitle = "Most values are low, with a long tail of high-value outliers.",
           x = "Value of 'i'",
           y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })
  
  model_results <- reactive({
    df <- data()
    req(df)
    
    set.seed(123)
    sample <- sample.split(df$Buzz, SplitRatio = 0.8)
    train_df <- subset(df, sample == TRUE)
    test_df <- subset(df, sample == FALSE)
    
    model <- lm(Buzz ~ log1p(Followers) + log1p(reply_count), data = train_df)
    
    predictions <- predict(model, newdata = test_df)
    results_df <- data.frame(
      Actual = test_df$Buzz,
      Predicted = predictions
    )
    mae <- mean(abs(results_df$Actual - results_df$Predicted))
    mse <- mean((results_df$Actual - results_df$Predicted)^2)
    rmse <- sqrt(mse)
    
    list(
      model_summary = summary(model),
      metrics = list(mae = mae, mse = mse, rmse = rmse),
      predictions_df = results_df
    )
  })
  
  output$model_summary_output <- renderPrint({
    model_results()$model_summary
  })
  
  output$model_metrics_output <- renderPrint({
    metrics <- model_results()$metrics
    cat("Mean Absolute Error (MAE):", round(metrics$mae, 2), "\n")
    cat("Mean Squared Error (MSE):", round(metrics$mse, 2), "\n")
    cat("Root Mean Squared Error (RMSE):", round(metrics$rmse, 2), "\n")
  })
  
  output$prediction_accuracy_plot <- renderPlot({
    results_df <- model_results()$predictions_df
    req(results_df)
    
    results_low <- results_df %>% filter(Actual < 0.5)
    results_high <- results_df %>% filter(Actual >= 0.5)
    
    plot1 <- ggplot(data = results_low, mapping = aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.6, color = "#2980b9") +
      geom_abline(intercept = 0, slope = 1, color = "#c0392b", linetype = "dashed", size = 1) +
      labs(
        title = "Focus on Low Buzz Scores",
        subtitle = "Actual < 0.5",
        x = "Actual Buzz Score",
        y = "Predicted Buzz Score"
      ) +
      theme_minimal() +
      coord_fixed(ratio = 1, xlim = c(0, 0.5), ylim = c(0, 1)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
    
    plot2 <- ggplot(data = results_high, mapping = aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      geom_abline(intercept = 0, slope = 1, color = "#c0392b", linetype = "dashed", size = 1) +
      labs(
        title = "Focus on High Buzz Scores",
        subtitle = "Actual >= 0.5",
        x = "Actual Buzz Score",
        y = "Predicted Buzz Score"
      ) +
      theme_minimal() +
      coord_fixed(ratio = 1, xlim = c(0.5, 1), ylim = c(0, 1)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
    grid.arrange(plot1, plot2, ncol = 2)
  })
  
  output$data_table <- renderDT({
    datatable(data(),
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100),
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                searchHighlight = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().container()).css({'overflow-x': 'auto'});",
                  "$(this.api().table().header()).css({'background-color': '#4CAF50', 'color': 'white'});",
                  "}"
                ),
                columnDefs = list(list(className = 'dt-center', targets = '_all')),
                scrollX = TRUE,
                scrollY = "400px",
                paging = TRUE,
                autoWidth = TRUE,
                rowCallback = JS(
                  "function(row, data) {",
                  "$(row).css('background-color', data[0] % 2 === 0 ? '#f2f2f2' : 'white');",
                  "}"
                )
              ),
              filter = 'top',
              class = 'cell-border stripe hover'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)