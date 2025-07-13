# ===================================================================
# DESCRIPTIVE STATISTICS MODULE: modules/descriptive_stats_module.R
# Handles descriptive statistics and frequency tables
# ===================================================================

# UI Function
descriptive_stats_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "descriptives",
          fluidRow(
            box(title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 4,
                conditionalPanel(condition = "output.fileUploaded",
                                 selectInput(ns("desc_vars"), "Select Variables for Descriptive Statistics:", 
                                             choices = NULL, multiple = TRUE),
                                 checkboxInput(ns("include_categorical"), "Include Categorical Variables", value = TRUE),
                                 actionButton(ns("generate_desc"), "Generate Descriptives", class = "btn-primary"),
                                 br(), br(),
                                 h5("R Code for descriptives:"),
                                 div(class = "r-code-box", verbatimTextOutput(ns("desc_r_code")))
                )
            ),
            box(title = "Descriptive Statistics", status = "info", solidHeader = TRUE, width = 8,
                DT::dataTableOutput(ns("descriptives_table")),
                br(), div(class = "interpretation-box", h4("📊 Interpretation Guide:"), 
                          htmlOutput(ns("desc_interpretation")))
            )
          ),
          fluidRow(
            box(title = "Categorical Variable Frequencies", status = "info", solidHeader = TRUE, width = 12,
                htmlOutput(ns("categorical_tables"))
            )
          ),
          fluidRow(
            box(title = "Distribution Plots", status = "info", solidHeader = TRUE, width = 12,
                plotlyOutput(ns("distribution_plots"), height = "400px")
            )
          )
  )
}

# Server Function
descriptive_stats_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable choices when data changes
    observe({
      req(shared_data$working_data)
      choices <- get_variable_choices(shared_data$working_data, "all")
      updateSelectInput(session, "desc_vars", choices = choices)
    })
    
    # Generate descriptives
    observeEvent(input$generate_desc, {
      req(shared_data$working_data, input$desc_vars)
      
      tryCatch({
        df <- shared_data$working_data
        selected_vars <- input$desc_vars[input$desc_vars != "" & !is.na(input$desc_vars)]
        selected_vars <- selected_vars[selected_vars %in% names(df)]
        
        if(length(selected_vars) == 0) {
          showNotification("No valid variables selected.", type = "warning")
          return()
        }
        
        # Generate R code for descriptives
        vars_str <- paste0("c('", paste(selected_vars, collapse = "', '"), "')")
        desc_r_code <- paste0("# Descriptive statistics\n")
        desc_r_code <- paste0(desc_r_code, "selected_vars <- ", vars_str, "\n")
        desc_r_code <- paste0(desc_r_code, "summary(data[, selected_vars])")
        
        add_r_code(shared_data, desc_r_code, "Descriptive Statistics")
        
        desc_list <- list()
        categorical_tables <- list()
        
        for(var in selected_vars) {
          var_data <- df[[var]]
          
          if(is.numeric(var_data)) {
            desc_list[[var]] <- data.frame(
              Variable = var, Type = "Numeric", N = sum(!is.na(var_data)),
              Mean = round(mean(var_data, na.rm = TRUE), 3),
              Median = round(median(var_data, na.rm = TRUE), 3),
              SD = round(sd(var_data, na.rm = TRUE), 3),
              Min = round(min(var_data, na.rm = TRUE), 3),
              Max = round(max(var_data, na.rm = TRUE), 3),
              stringsAsFactors = FALSE
            )
          } else {
            freq_table <- table(var_data, useNA = "ifany")
            prop_table <- prop.table(freq_table) * 100
            
            categorical_tables[[var]] <- data.frame(
              Category = names(freq_table), Frequency = as.numeric(freq_table),
              Percentage = round(as.numeric(prop_table), 1), stringsAsFactors = FALSE
            )
            
            desc_list[[var]] <- data.frame(
              Variable = var, Type = "Categorical", N = sum(!is.na(var_data)),
              Mean = NA, Median = NA, SD = NA, Min = NA, Max = NA, stringsAsFactors = FALSE
            )
          }
        }
        
        desc_stats <- do.call(rbind, desc_list)
        rownames(desc_stats) <- NULL
        
        output$descriptives_table <- DT::renderDataTable({
          DT::datatable(desc_stats, options = list(scrollX = TRUE, dom = 'lrtip')) %>%
            formatRound(columns = c("Mean", "Median", "SD", "Min", "Max"), digits = 3)
        })
        
        # Create categorical frequency tables
        if(length(categorical_tables) > 0) {
          output$categorical_tables <- renderUI({
            table_list <- lapply(names(categorical_tables), function(var_name) {
              cat_data <- categorical_tables[[var_name]]
              div(
                h5(paste("Frequency Table:", var_name), style = "color: #2c3e50; margin-top: 20px;"),
                DT::renderDataTable({
                  DT::datatable(cat_data, options = list(dom = 't', pageLength = -1), rownames = FALSE) %>%
                    formatRound(columns = "Percentage", digits = 1)
                }, outputArgs = list())
              )
            })
            div(h4("📊 Categorical Variable Breakdowns:", style = "color: #2c3e50;"), table_list)
          })
        } else {
          output$categorical_tables <- renderUI({ div() })
        }
        
        # Generate interpretation
        output$desc_interpretation <- renderUI({
          n_vars <- length(selected_vars)
          numeric_vars <- sum(sapply(df[selected_vars], is.numeric))
          categorical_vars <- n_vars - numeric_vars
          
          HTML(paste0(
            "<strong>Summary:</strong> You've analyzed ", n_vars, " variables (", 
            numeric_vars, " numeric, ", categorical_vars, " categorical).<br/>",
            "<strong>Key Points:</strong><br/>",
            "• Check the 'N' column for missing data patterns<br/>",
            "• For numeric variables: Compare mean and median<br/>",
            "• For categorical variables: Review frequency tables below<br/>",
            "• Standard deviation shows variability"
          ))
        })
        
      }, error = function(e) {
        showNotification(paste("Error in descriptive statistics:", e$message), type = "error")
      })
    })
    
    # R code output
    output$desc_r_code <- renderText({
      if(is.null(input$desc_vars) || length(input$desc_vars) == 0) return("# Select variables for descriptives")
      
      vars_str <- paste0("c('", paste(input$desc_vars, collapse = "', '"), "')")
      r_code <- paste0("# Descriptive statistics\n")
      r_code <- paste0(r_code, "selected_vars <- ", vars_str, "\n")
      r_code <- paste0(r_code, "summary(data[, selected_vars])\n\n")
      r_code <- paste0(r_code, "# For categorical variables\n")
      r_code <- paste0(r_code, "lapply(data[, selected_vars], function(x) {\n")
      r_code <- paste0(r_code, "  if(is.numeric(x)) summary(x) else table(x, useNA = 'ifany')\n")
      r_code <- paste0(r_code, "})")
      
      return(r_code)
    })
    
    # Distribution plots
    output$distribution_plots <- renderPlotly({
      req(shared_data$working_data, input$desc_vars)
      
      df <- shared_data$working_data
      selected_vars <- input$desc_vars[input$desc_vars != ""]
      selected_vars <- selected_vars[1:min(4, length(selected_vars))]
      
      if(length(selected_vars) > 0) {
        plots <- list()
        
        for(i in seq_along(selected_vars)) {
          var_name <- selected_vars[i]
          if(is.numeric(df[[var_name]])) {
            # For numeric variables, use density histogram to show percentages
            p <- ggplot(df, aes_string(x = var_name)) +
              geom_histogram(aes(y = after_stat(count/sum(count)*100)), 
                             bins = 30, fill = "skyblue", alpha = 0.7) +
              labs(title = "", 
                   x = var_name, y = "Percentage (%)") +
              theme_minimal() +
              theme(plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)))
          } else {
            # For categorical variables, calculate percentages
            var_data <- df[[var_name]]
            var_data <- var_data[!is.na(var_data)]  # Remove NAs for percentage calculation
            
            # Create a data frame with percentages
            freq_table <- table(var_data)
            perc_df <- data.frame(
              category = names(freq_table),
              percentage = as.numeric(freq_table) / length(var_data) * 100,
              stringsAsFactors = FALSE
            )
            
            p <- ggplot(perc_df, aes(x = category, y = percentage)) +
              geom_bar(stat = "identity", fill = "lightcoral", alpha = 0.7) + 
              labs(title = "", 
                   x = var_name, y = "Percentage (%)") +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
                axis.text.x = element_text(angle = 45, hjust = 1)
              )
          }
          
          # Convert to plotly and ensure title is preserved
          plotly_obj <- ggplotly(p) %>%
            layout(
              title = list(
                text = "",
                font = list(size = 14),
                x = 0.5,
                xanchor = 'center'
              ),
              margin = list(t = 50),  # Add top margin for title
              yaxis = list(title = "Percentage (%)")  # Ensure y-axis label is clear
            )
          
          plots[[i]] <- plotly_obj
        }
        
        # Handle single vs multiple plots
        if(length(plots) == 1) {
          plots[[1]]
        } else {
          # Use subplot with proper spacing and margins
          subplot(
            plots, 
            nrows = ceiling(length(plots)/2), 
            margin = 0.08,  # Add margin between subplots
            titleX = TRUE, 
            titleY = TRUE
          ) %>%
            layout(
              showlegend = FALSE,
              margin = list(t = 60, b = 40, l = 40, r = 40)  # Overall margins
            )
        }
      }
    })
  })
}

# ===================================================================
# END OF DESCRIPTIVE STATISTICS MODULE
# ===================================================================