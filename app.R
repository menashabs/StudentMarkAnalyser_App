# ------------------------------------------------------------------------------

# StudentMarkAnalyser: Students' Exam Mark Analysing App

# ------------------------------------------------------------------------------

# Libraries
library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(gridExtra)
library(shinythemes)
library(bslib)
library(patchwork)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# UI
ui <- fluidPage(
  
  # Custom CSS for light brown background and dark brown text
  
  tags$head(
    tags$style(HTML("
    
     /* Full page background */
     
      body {
        background-color: #F0E4D7;  
      }
    
      /* Title Panel */
      
      .shiny-title-panel {
        background-color: #D2B48C;  
        padding: 20px;
        border-radius: 0px;
        text-align: center;
        margin-bottom: 20px;
      }
      .shiny-title-panel h1 {
        color: #5C4033 !important;           
        margin: 0;
      }

      /* Sidebar panel */
      
      .well, .sidebar {
        background-color: #D2B48C !important;  
        color: #5C4033 !important;             
      }

      /* Main panel */
      
      .main-panel {
        background-color: #D2B48C;  
        color: #5C4033;            
        padding: 20px;
        border-radius: 10px;
      }

      /* Tab panels */
      
      .tab-content, .nav-tabs {
        background-color: #F0E4D7;  
        color: #5C4033; 
        margin-top: 10px;
      }

      /* Tab titles */
      
      .nav-tabs > li > a {
        color: #5C4033;             
        background-color: #D2B48C;  
      }
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        background-color: #C19A6B;  
        color: #5C4033;             
      }

      /* Buttons */
      
      .btn {
        background-color: #C19A6B;  
        color: #5C4033;             
        border: 1px solid #5C4033;
      }
      .btn:hover {
        background-color: #D2B48C;
      }
      
      /* White background for data preview, grade table, and summary table */
      
      #data_preview, #grade_table, #summary_table, 
      .dataTable, table.dataTable {
        background-color: white !important;
        color: #5C4033 !important;  /* dark brown text */
        border-radius: 10px;
        padding: 10px;
      }

      /* Make table headers stand out slightly */
      
      table.dataTable th {
        background-color: #F9F9F9 !important;
        color: #5C4033 !important;
        font-weight: bold;
      }
      
    "))
  ),
  
  # ----------------------------------------------------------------------------
  
  # Title panel 
  
  div(class = "shiny-title-panel",
      tags$h1("StudentMarkAnalyser: Students' Exam Mark Analysing App")
  ),
  
  # ----------------------------------------------------------------------------
  
  # Sidebar panel 
  
  sidebarLayout(
    sidebarPanel(
      
      # CSV Upload
      fileInput("file", "Upload CSV", accept = ".csv"),
      
      # Distribution variable selection (histogram + density + boxplot)
      uiOutput("dist_var"),
      
      # Scatterplot variable selections
      uiOutput("scatter_x"),
      uiOutput("scatter_y"),
      
      width = 3
      ),
    
    # --------------------------------------------------------------------------
    
    # Main panel 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Preview & Grades",
                 h4("The Dataset"),
                 DTOutput("data_preview"),
                 h4("Grade Distribution"),
                 DTOutput("grade_table")
        ),
        tabPanel("Summary Statistics", 
                 h4("Summary Statistics of Marks"),
                 DTOutput("summary_table")),
        tabPanel("Distributions", 
                 uiOutput("dist_tabs")),
        tabPanel("Relationships", 
                 plotOutput("scatter_plot"))
      ),
      
      width = 9
    )
  )
)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Server
server <- function(input, output, session) {
  
  # ------------------------------------------------------------------------------
  
  # Load data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, sep = ",")
  })
  
  # ------------------------------------------------------------------------------
  
  # Filter only quantitative variables
  numeric_data <- reactive({
    req(data())
    df <- data()
    df %>% 
      select(where(is.numeric))
  })
  
  # Show first 5 rows of dataset
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), 
              options = list(pageLength = 5),
              rownames = FALSE)
  })
  
  # Grade distribution table 
  output$grade_table <- renderDT({
    req(data())
    df <- data()
    
    grades <- df %>%
      count(Grade, name = "Frequency")
    
    datatable(
      grades[order(factor(grades$Grade, levels = c("A+", "A", "A-", "B+", "B", "B-", 
                                                   "C+", "C", "C-", "D+", "D", "E", "AB"))), ],
      options = list(pageLength = 7),
      rownames = FALSE
    )
  })
  
  # ------------------------------------------------------------------------------
  
  # Summary statistics
  output$summary_table <- renderDT({
    
    df <- req(numeric_data()) 
    
    # Calculate summary statistics
    numeric_cols <- df %>% 
      select(where(is.numeric))
    
    # Function to compute modes
    get_modes <- function(x){
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      modes <- ux[tab == max(tab)]
      if(is.numeric(modes)) {
        modes <- round(modes, 2)
      }
      paste(modes, collapse = ", ")
    }
    
    summary_df <- data.frame(
      Statistic = c("Min", "Max", "Mean", "Median", "Mode", "Std_Dev", "IQR"),
      stringsAsFactors = FALSE
    )
    
    # Compute statistics for each numeric column
    for (col in names(numeric_cols)) {
      x <- numeric_cols[[col]]
      summary_df[[col]] <- c(
        round(min(x, na.rm = TRUE), 2),
        round(max(x, na.rm = TRUE), 2),
        round(mean(x, na.rm = TRUE), 2),
        round(median(x, na.rm = TRUE), 2),
        get_modes(x),               
        round(sd(x, na.rm = TRUE), 2),
        round(IQR(x, na.rm = TRUE), 2)
      )
    }
    
    # Reshape to have variables as rows and statistics as columns
    summary_df_tidy <- summary_df %>%
      pivot_longer(cols = -Statistic, names_to = "Variable", values_to = "Value") %>%
      pivot_wider(names_from = Statistic, values_from = Value)
    
    datatable(summary_df_tidy, 
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # ----------------------------------------------------------------------------
  
  # Distributions
  
  # Dynamic UI for distribution variable(s) selection
  output$dist_var <- renderUI({
    req(data())
    numeric_vars <- names(data() %>% select(where(is.numeric)))
    
    selectInput("dist_vars", "Select Variable(s) for Distribution Plots:", 
                choices = numeric_vars, multiple = TRUE)
  })
  
  # Render distribution plot tabs dynamically
  output$dist_tabs <- renderUI({
    req(input$dist_vars)
    
    tabs <- lapply(input$dist_vars, function(var_name) {
      tabPanel(
        title = var_name,
        plotOutput(outputId = paste0("dist_plot_", var_name))
      )
    })
    
    do.call(tabsetPanel, tabs)
  })
  
  # Render distribution plots for each selected variable
  observe({
    req(input$dist_vars)
    
    for (var_name in input$dist_vars) {
      local({
        v <- var_name
        output[[paste0("dist_plot_", v)]] <- renderPlot({
          x <- data()[[v]]
          
          # Histogram
          histogram <- ggplot(data = data.frame(x), aes(x = x)) +
            geom_histogram(bins = 20, fill = "#D2B48C", color = "#5C4033") +
            geom_density(color = "#5C4033", size = 1) +
            theme_minimal() +
            theme(
              axis.text.x  = element_text(color = "#5C4033", size = 10),  
              axis.text.y  = element_text(color = "#5C4033", size = 10),  
              axis.ticks.x = element_line(color = "#5C4033", size = 1),             
              axis.ticks.y = element_line(color = "#5C4033", size = 1),         
              axis.title.x = element_text(color = "#5C4033", size = 12), 
              axis.title.y = element_text(color = "#5C4033", size = 12),   
              plot.title   = element_text(color = "#5C4033", size = 18)
            ) +
            labs(title = paste("Distribution of", v), y = "Frequency", x = "")
          
          # Boxplot
          boxplot <- ggplot(data = data.frame(x), aes(x = x, y = "")) +
            geom_boxplot(fill = "#D2B48C", color = "#5C4033") +
            theme_minimal() +
            theme(
              axis.text.x  = element_text(color = "#5C4033", size = 10),  
              axis.text.y  = element_blank(),  
              axis.ticks.x = element_line(color = "#5C4033", size = 1),             
              axis.ticks.y = element_blank(),          
              axis.title.x = element_text(color = "#5C4033", size = 12), 
              axis.title.y = element_blank(),  
              plot.title   = element_text(color = "#5C4033", size = 18)
            ) +
            labs(title = "", x = "Values")
          
          histogram / boxplot + patchwork::plot_layout(heights = c(3,1))
        })
      })
    }
  })
  
  # ----------------------------------------------------------------------------
  
  # Generate UI for selecting variables for scatter plot
  output$scatter_x <- renderUI({
    req(numeric_data())
    selectInput("scatter_x_var", "Select X Variable for Scatter Plot", 
                choices = names(numeric_data()))
  })
  
  output$scatter_y <- renderUI({
    req(numeric_data())
    selectInput("scatter_y_var", "Select Y Variable for Scatter Plot", 
                choices = names(numeric_data()))
  })
  
  # Generate scatter plot
  output$scatter_plot <- renderPlot({
    req(numeric_data(), input$scatter_x_var, input$scatter_y_var)
    df <- numeric_data()
    
    ggplot(df, aes_string(x = input$scatter_x_var, y = input$scatter_y_var)) +
      geom_point(color = "#D2B48C") +
      geom_smooth(method = "lm", se = FALSE, color = "#5C4033") +
      theme_minimal() +
      theme(
        axis.text.x  = element_text(color = "#5C4033", size = 10),  
        axis.text.y  = element_text(color = "#5C4033", size = 10),  
        axis.ticks.x = element_line(color = "#5C4033", size = 1),             
        axis.ticks.y = element_line(color = "#5C4033", size = 1),          
        axis.title.x = element_text(color = "#5C4033", size = 12), 
        axis.title.y = element_text(color = "#5C4033", size = 12),  
        plot.title   = element_text(color = "#5C4033", size = 18)
      ) +
      labs(
        title = paste("Scatter Plot of", input$scatter_x_var, "vs", input$scatter_y_var),
        x = input$scatter_x_var,
        y = input$scatter_y_var
      )
  })
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Run the App
shinyApp(ui, server)

# ------------------------------------------------------------------------------

