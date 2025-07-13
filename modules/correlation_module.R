# ===================================================================
# CORRELATION MODULE: modules/correlation_module.R
# Handles correlation analysis and visualization
# ===================================================================

# Required packages installation and loading
required_packages <- c("corrplot", "psych", "ppcor", "qgraph", "pheatmap", "DT", "dplyr")

# Function to install and load packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
      cat("Package", pkg, "installed and loaded successfully.\n")
    } else {
      cat("Package", pkg, "already installed and loaded.\n")
    }
  }
}

# Install and load required packages
cat("=== Loading Required Packages for Correlation Module ===\n")
install_and_load(required_packages)
cat("=== All packages loaded successfully ===\n")

# UI Function
correlation_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "correlations",
          fluidRow(
            box(title = "Correlation Analysis", status = "primary", solidHeader = TRUE, width = 4,
                conditionalPanel(condition = "output.fileUploaded",
                                 selectInput(ns("corr_vars"), "Select Variables for Correlation:", 
                                             choices = NULL, multiple = TRUE),
                                 selectInput(ns("corr_method"), "Correlation Method:",
                                             choices = list("Pearson" = "pearson", 
                                                            "Spearman" = "spearman",
                                                            "Kendall" = "kendall"), 
                                             selected = "pearson"),
                                 
                                 # Partial Correlation Section
                                 checkboxInput(ns("use_partial"), "Use Partial Correlation", value = FALSE),
                                 conditionalPanel(condition = paste0("input['", ns("use_partial"), "']"),
                                                  selectInput(ns("control_vars"), "Control Variables:", 
                                                              choices = NULL, multiple = TRUE),
                                                  selectInput(ns("partial_method"), "Partial Correlation Method:",
                                                              choices = list("Partial" = "partial",
                                                                             "Semi-partial" = "semipartial"),
                                                              selected = "partial")
                                 ),
                                 
                                 # Statistical Options
                                 checkboxInput(ns("show_pvalues"), "Show P-values", value = FALSE),
                                 checkboxInput(ns("show_ci"), "Show Confidence Intervals", value = FALSE),
                                 conditionalPanel(condition = paste0("input['", ns("show_ci"), "']"),
                                                  numericInput(ns("ci_level"), "Confidence Level:", 
                                                               value = 0.95, min = 0.01, max = 0.99, step = 0.01)
                                 ),
                                 
                                 # Visualization Options
                                 selectInput(ns("plot_type"), "Plot Type:",
                                             choices = list("Correlation Matrix" = "corrplot", 
                                                            "Network Plot" = "network",
                                                            "Heatmap with Clustering" = "heatmap"),
                                             selected = "corrplot"),
                                 
                                 actionButton(ns("generate_corr"), "Generate Correlation Analysis", class = "btn-primary"),
                                 br(),
                                 actionButton(ns("reset_all"), "Reset All Fields", class = "btn-warning"),
                                 br(), br(),
                                 
                                 # Export Options
                                 h5("Export Options:"),
                                 downloadButton(ns("download_corr"), "Download Results", class = "btn-success"),
                                 br(), br(),
                                 
                                 h5("R Code for correlation:"),
                                 div(class = "r-code-box", verbatimTextOutput(ns("corr_r_code")))
                )
            ),
            box(title = "Correlation Visualization", status = "info", solidHeader = TRUE, width = 8,
                plotOutput(ns("correlation_plot"), height = "500px"), 
                br(), 
                conditionalPanel(condition = paste0("input['", ns("plot_type"), "'] == 'network'"),
                                 h5("Network Plot Legend:"),
                                 p("• Blue edges: Positive correlations"),
                                 p("• Red edges: Negative correlations"),
                                 p("• Edge thickness: Correlation strength")
                )
            )
          ),
          fluidRow(
            box(title = "Correlation Results", status = "info", solidHeader = TRUE, width = 12,
                tabsetPanel(
                  tabPanel("Correlation Matrix", DT::dataTableOutput(ns("correlation_table"))),
                  tabPanel("Significance Tests", 
                           conditionalPanel(condition = paste0("input['", ns("show_pvalues"), "']"),
                                            div(
                                              h5("Statistical Significance of Correlations"),
                                              p(strong("P-values"), " indicate the probability that the observed correlation occurred by chance:"),
                                              tags$ul(
                                                tags$li(HTML("<strong>p < 0.001:</strong> Very strong evidence against null hypothesis (***)")),
                                                tags$li(HTML("<strong>p < 0.01:</strong> Strong evidence (marked with **)")),
                                                tags$li(HTML("<strong>p < 0.05:</strong> Moderate evidence (marked with *)")),
                                                tags$li(HTML("<strong>p ≥ 0.05:</strong> Insufficient evidence (not significant)"))
                                              ),
                                              p(em("Note: Colors indicate significance levels - green (p<0.01), yellow (p<0.05), red (p≥0.05)")),
                                              br()
                                            ),
                                            DT::dataTableOutput(ns("pvalue_table"))
                           ),
                           conditionalPanel(condition = paste0("!input['", ns("show_pvalues"), "']"),
                                            div(
                                              h5("About Significance Tests"),
                                              p("Significance tests help determine if correlations are statistically meaningful or could have occurred by random chance."),
                                              p(strong("To view significance tests:")),
                                              tags$ol(
                                                tags$li("Check the 'Show P-values' option in the analysis panel"),
                                                tags$li("Generate your correlation analysis"),
                                                tags$li("Return to this tab to view detailed p-values")
                                              ),
                                              br(),
                                              div(class = "alert alert-info",
                                                  HTML("<strong>What you'll see:</strong><br/>
                                                     • P-values for each correlation coefficient<br/>
                                                     • Color-coded significance levels<br/>
                                                     • Statistical interpretation guidance"))
                                            )
                           )
                  ),
                  tabPanel("Confidence Intervals",
                           conditionalPanel(condition = paste0("input['", ns("show_ci"), "']"),
                                            DT::dataTableOutput(ns("ci_table"))
                           ),
                           conditionalPanel(condition = paste0("!input['", ns("show_ci"), "']"),
                                            p("Enable 'Show Confidence Intervals' option to see confidence intervals.")
                           )
                  )
                )
            )
          ),
          fluidRow(
            box(title = "Correlation Interpretation", status = "info", solidHeader = TRUE, width = 8,
                div(class = "interpretation-box", htmlOutput(ns("corr_interpretation")))
            ),
            box(title = "R Log Output", status = "warning", solidHeader = TRUE, width = 4,
                div(class = "r-log-box", 
                    verbatimTextOutput(ns("r_log_output")),
                    style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 10px;"
                )
            )
          )
  )
}

# Server Function
correlation_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for storing results
    corr_results <- reactiveValues(
      matrix = NULL,
      pvalues = NULL,
      ci_lower = NULL,
      ci_upper = NULL,
      r_log = ""
    )
    
    # Reset all fields
    observeEvent(input$reset_all, {
      updateSelectInput(session, "corr_vars", selected = character(0))
      updateSelectInput(session, "corr_method", selected = "pearson")
      updateCheckboxInput(session, "use_partial", value = FALSE)
      updateSelectInput(session, "control_vars", selected = character(0))
      updateSelectInput(session, "partial_method", selected = "partial")
      updateCheckboxInput(session, "show_pvalues", value = FALSE)
      updateCheckboxInput(session, "show_ci", value = FALSE)
      updateNumericInput(session, "ci_level", value = 0.95)
      updateSelectInput(session, "plot_type", selected = "corrplot")
      
      corr_results$matrix <- NULL
      corr_results$pvalues <- NULL
      corr_results$ci_lower <- NULL
      corr_results$ci_upper <- NULL
      corr_results$r_log <- ""
      
      output$correlation_plot <- renderPlot({ 
        plot.new()
        text(0.5, 0.5, "Select variables and generate analysis", cex = 1.2, col = "gray")
      })
      output$correlation_table <- DT::renderDataTable({ 
        data.frame(Message = "No data to display") 
      }, options = list(dom = 't'))
      output$pvalue_table <- DT::renderDataTable({ 
        data.frame(Message = "No data to display") 
      }, options = list(dom = 't'))
      output$ci_table <- DT::renderDataTable({ 
        data.frame(Message = "No data to display") 
      }, options = list(dom = 't'))
      output$corr_interpretation <- renderUI({ 
        HTML("<p>Select variables and click 'Generate Correlation Analysis' to begin.</p>") 
      })
      output$r_log_output <- renderText({
        "R log will appear here after running correlation analysis..."
      })
      
      showNotification("All fields have been reset to default values.", type = "message")
    })
    
    # Update variable choices when data changes
    observe({
      req(shared_data$working_data)
      choices <- get_variable_choices(shared_data$working_data, "numeric")
      updateSelectInput(session, "corr_vars", choices = choices)
      updateSelectInput(session, "control_vars", choices = choices)
    })
    
    # Generate correlation analysis
    observeEvent(input$generate_corr, {
      req(shared_data$working_data, input$corr_vars)
      
      tryCatch({
        df <- shared_data$working_data
        selected_vars <- input$corr_vars[input$corr_vars != ""]
        available_vars <- selected_vars[selected_vars %in% names(df)]
        numeric_vars <- available_vars[sapply(df[available_vars], is.numeric)]
        
        if(length(numeric_vars) < 2) {
          showNotification("Please select at least 2 numeric variables for correlation analysis.", type = "warning")
          return()
        }
        
        # Initialize R log
        r_log <- "=== R Correlation Analysis Log ===\n"
        r_log <- paste0(r_log, "Started at: ", Sys.time(), "\n")
        r_log <- paste0(r_log, "Variables selected: ", paste(numeric_vars, collapse = ", "), "\n")
        r_log <- paste0(r_log, "Method: ", input$corr_method, "\n")
        
        # Generate R code for correlation
        vars_str <- paste0("c('", paste(numeric_vars, collapse = "', '"), "')")
        corr_r_code <- paste0("# Enhanced Correlation Analysis\n")
        corr_r_code <- paste0(corr_r_code, "# Load required packages\n")
        corr_r_code <- paste0(corr_r_code, "required_packages <- c('corrplot', 'psych', 'ppcor', 'qgraph', 'pheatmap')\n")
        corr_r_code <- paste0(corr_r_code, "for(pkg in required_packages) {\n")
        corr_r_code <- paste0(corr_r_code, "  if(!require(pkg, character.only = TRUE)) {\n")
        corr_r_code <- paste0(corr_r_code, "    install.packages(pkg)\n")
        corr_r_code <- paste0(corr_r_code, "    library(pkg, character.only = TRUE)\n")
        corr_r_code <- paste0(corr_r_code, "  }\n")
        corr_r_code <- paste0(corr_r_code, "}\n\n")
        
        if(input$use_partial) {
          corr_r_code <- paste0(corr_r_code, "# Additional packages for partial correlation\n")
          corr_r_code <- paste0(corr_r_code, "library(ppcor)\n")
        }
        if(input$plot_type == "network") {
          corr_r_code <- paste0(corr_r_code, "# Additional packages for network plots\n")
          corr_r_code <- paste0(corr_r_code, "library(qgraph)\n")
        }
        if(input$plot_type == "heatmap") {
          corr_r_code <- paste0(corr_r_code, "# Additional packages for heatmaps\n")
          corr_r_code <- paste0(corr_r_code, "library(pheatmap)\n")
        }
        
        corr_r_code <- paste0(corr_r_code, "\n# Data preparation\n")
        corr_r_code <- paste0(corr_r_code, "selected_vars <- ", vars_str, "\n")
        corr_r_code <- paste0(corr_r_code, "corr_data <- data[, selected_vars, drop = FALSE]\n")
        corr_r_code <- paste0(corr_r_code, "corr_data <- na.omit(corr_data)\n")
        
        corr_data <- df[, numeric_vars, drop = FALSE]
        corr_data <- na.omit(corr_data)
        
        if(nrow(corr_data) < 10) {
          showNotification("Not enough valid observations for correlation analysis.", type = "warning")
          return()
        }
        
        r_log <- paste0(r_log, "Valid observations: ", nrow(corr_data), "\n")
        
        # Perform correlation analysis
        if(input$use_partial && !is.null(input$control_vars) && length(input$control_vars) > 0) {
          # Partial correlation analysis
          control_vars <- input$control_vars[input$control_vars %in% names(df)]
          control_vars <- control_vars[sapply(df[control_vars], is.numeric)]
          
          if(length(control_vars) > 0) {
            r_log <- paste0(r_log, "Control variables: ", paste(control_vars, collapse = ", "), "\n")
            
            all_vars <- c(numeric_vars, control_vars)
            all_data <- df[, all_vars, drop = FALSE]
            all_data <- na.omit(all_data)
            
            if(input$partial_method == "partial") {
              # Full partial correlation
              library(ppcor)
              pcor_result <- pcor(all_data, method = input$corr_method)
              corr_matrix <- pcor_result$estimate[1:length(numeric_vars), 1:length(numeric_vars)]
              
              if(input$show_pvalues) {
                corr_results$pvalues <- pcor_result$p.value[1:length(numeric_vars), 1:length(numeric_vars)]
              }
              
              corr_r_code <- paste0(corr_r_code, "\n# Partial correlation\n")
              corr_r_code <- paste0(corr_r_code, "control_vars <- c('", paste(control_vars, collapse = "', '"), "')\n")
              corr_r_code <- paste0(corr_r_code, "all_vars <- c(selected_vars, control_vars)\n")
              corr_r_code <- paste0(corr_r_code, "all_data <- data[, all_vars, drop = FALSE]\n")
              corr_r_code <- paste0(corr_r_code, "all_data <- na.omit(all_data)\n")
              corr_r_code <- paste0(corr_r_code, "pcor_result <- pcor(all_data, method = '", input$corr_method, "')\n")
              corr_r_code <- paste0(corr_r_code, "corr_matrix <- pcor_result$estimate[1:length(selected_vars), 1:length(selected_vars)]\n")
              
              r_log <- paste0(r_log, "Partial correlation computed successfully\n")
            } else {
              # Semi-partial correlation
              library(ppcor)
              spcor_result <- spcor(all_data, method = input$corr_method)
              corr_matrix <- spcor_result$estimate[1:length(numeric_vars), 1:length(numeric_vars)]
              
              if(input$show_pvalues) {
                corr_results$pvalues <- spcor_result$p.value[1:length(numeric_vars), 1:length(numeric_vars)]
              }
              
              corr_r_code <- paste0(corr_r_code, "\n# Semi-partial correlation\n")
              corr_r_code <- paste0(corr_r_code, "spcor_result <- spcor(all_data, method = '", input$corr_method, "')\n")
              corr_r_code <- paste0(corr_r_code, "corr_matrix <- spcor_result$estimate[1:length(selected_vars), 1:length(selected_vars)]\n")
              
              r_log <- paste0(r_log, "Semi-partial correlation computed successfully\n")
            }
          } else {
            showNotification("No valid control variables selected for partial correlation.", type = "warning")
            return()
          }
        } else {
          # Regular correlation
          corr_matrix <- cor(corr_data, method = input$corr_method)
          
          if(input$show_pvalues) {
            library(psych)
            corr_test <- corr.test(corr_data, method = input$corr_method)
            corr_results$pvalues <- corr_test$p
          }
          
          corr_r_code <- paste0(corr_r_code, "\n# Standard correlation\n")
          corr_r_code <- paste0(corr_r_code, "corr_matrix <- cor(corr_data, method = '", input$corr_method, "')\n")
          
          r_log <- paste0(r_log, "Standard correlation computed successfully\n")
        }
        
        # Confidence intervals
        if(input$show_ci) {
          n <- nrow(corr_data)
          z_crit <- qnorm(1 - (1 - input$ci_level) / 2)
          
          # Fisher's z-transformation for confidence intervals
          z_corr <- 0.5 * log((1 + corr_matrix) / (1 - corr_matrix))
          se_z <- 1 / sqrt(n - 3)
          
          z_lower <- z_corr - z_crit * se_z
          z_upper <- z_corr + z_crit * se_z
          
          corr_results$ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
          corr_results$ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
          
          r_log <- paste0(r_log, "Confidence intervals calculated (", input$ci_level * 100, "% level)\n")
        }
        
        rownames(corr_matrix) <- colnames(corr_matrix) <- numeric_vars
        corr_results$matrix <- corr_matrix
        
        add_r_code(shared_data, corr_r_code, "Enhanced Correlation Analysis")
        
        # Generate visualization
        output$correlation_plot <- renderPlot({
          if(input$plot_type == "corrplot") {
            corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", 
                     tl.cex = 0.8, tl.col = "black", addCoef.col = "black", number.cex = 0.7)
          } else if(input$plot_type == "network") {
            if(requireNamespace("qgraph", quietly = TRUE)) {
              qgraph::qgraph(corr_matrix, graph = "cor", layout = "spring", 
                             posCol = "blue", negCol = "red", threshold = 0.1)
            } else {
              plot.new()
              text(0.5, 0.5, "qgraph package required for network plots", cex = 1.2, col = "red")
            }
          } else if(input$plot_type == "heatmap") {
            if(requireNamespace("pheatmap", quietly = TRUE)) {
              pheatmap::pheatmap(corr_matrix, cluster_rows = TRUE, cluster_cols = TRUE,
                                 display_numbers = TRUE, fontsize_number = 8)
            } else {
              plot.new()
              text(0.5, 0.5, "pheatmap package required for heatmap plots", cex = 1.2, col = "red")
            }
          }
        })
        
        # Correlation table
        corr_df <- as.data.frame(round(corr_matrix, 3))
        corr_df$Variable <- rownames(corr_df)
        corr_df <- corr_df[, c("Variable", names(corr_df)[names(corr_df) != "Variable"])]
        
        output$correlation_table <- DT::renderDataTable({
          DT::datatable(corr_df, options = list(scrollX = TRUE, dom = 'lrtip')) %>%
            formatRound(columns = 2:ncol(corr_df), digits = 3)
        })
        
        # P-values table
        if(input$show_pvalues && !is.null(corr_results$pvalues)) {
          pval_df <- as.data.frame(round(corr_results$pvalues, 4))
          pval_df$Variable <- rownames(pval_df)
          pval_df <- pval_df[, c("Variable", names(pval_df)[names(pval_df) != "Variable"])]
          
          output$pvalue_table <- DT::renderDataTable({
            DT::datatable(pval_df, options = list(scrollX = TRUE, dom = 'lrtip')) %>%
              formatRound(columns = 2:ncol(pval_df), digits = 4) %>%
              formatStyle(columns = 2:ncol(pval_df), 
                          backgroundColor = styleInterval(c(0.01, 0.05), c("lightgreen", "lightyellow", "lightcoral")))
          })
        }
        
        # Confidence intervals table
        if(input$show_ci && !is.null(corr_results$ci_lower)) {
          ci_data <- data.frame(
            Variable1 = rep(rownames(corr_matrix), each = ncol(corr_matrix)),
            Variable2 = rep(colnames(corr_matrix), nrow(corr_matrix)),
            Correlation = as.vector(corr_matrix),
            CI_Lower = as.vector(corr_results$ci_lower),
            CI_Upper = as.vector(corr_results$ci_upper),
            stringsAsFactors = FALSE
          )
          ci_data <- ci_data[ci_data$Variable1 != ci_data$Variable2, ]
          ci_data <- ci_data[!duplicated(t(apply(ci_data[, 1:2], 1, sort))), ]
          
          output$ci_table <- DT::renderDataTable({
            DT::datatable(ci_data, options = list(scrollX = TRUE, dom = 'lrtip')) %>%
              formatRound(columns = 3:5, digits = 3)
          })
        }
        
        # Generate interpretation
        strong_corrs <- which(abs(corr_matrix) > 0.5 & corr_matrix != 1, arr.ind = TRUE)
        moderate_corrs <- which(abs(corr_matrix) >= 0.3 & abs(corr_matrix) <= 0.5, arr.ind = TRUE)
        
        output$corr_interpretation <- renderUI({
          interp_text <- "<strong>Correlation Analysis Results:</strong><br/>"
          
          if(input$use_partial) {
            interp_text <- paste0(interp_text, "Type: ", tools::toTitleCase(input$partial_method), " correlation<br/>")
            if(!is.null(input$control_vars) && length(input$control_vars) > 0) {
              interp_text <- paste0(interp_text, "Controlling for: ", paste(input$control_vars, collapse = ", "), "<br/>")
            }
          } else {
            interp_text <- paste0(interp_text, "Type: Standard correlation<br/>")
          }
          
          interp_text <- paste0(interp_text, "Method: ", tools::toTitleCase(input$corr_method), "<br/>")
          interp_text <- paste0(interp_text, "Sample size: ", nrow(corr_data), " observations<br/><br/>")
          
          if(nrow(strong_corrs) > 0) {
            strong_text <- paste(apply(strong_corrs, 1, function(x) 
              paste0(rownames(corr_matrix)[x[1]], " & ", colnames(corr_matrix)[x[2]], 
                     " (r = ", round(corr_matrix[x[1], x[2]], 3), ")")), collapse = "<br/>• ")
            interp_text <- paste0(interp_text, "<strong>Strong correlations (|r| > 0.5):</strong><br/>• ", strong_text, "<br/><br/>")
          }
          
          if(nrow(moderate_corrs) > 0) {
            mod_text <- paste(apply(moderate_corrs, 1, function(x) 
              paste0(rownames(corr_matrix)[x[1]], " & ", colnames(corr_matrix)[x[2]], 
                     " (r = ", round(corr_matrix[x[1], x[2]], 3), ")")), collapse = "<br/>• ")
            interp_text <- paste0(interp_text, "<strong>Moderate correlations (0.3 ≤ |r| ≤ 0.5):</strong><br/>• ", mod_text, "<br/><br/>")
          }
          
          if(nrow(strong_corrs) == 0 && nrow(moderate_corrs) == 0) {
            interp_text <- paste0(interp_text, "No strong or moderate correlations detected.<br/><br/>")
          }
          
          interp_text <- paste0(interp_text, 
                                "<strong>Interpretation Guide:</strong><br/>",
                                "• |r| < 0.3: Weak relationship<br/>",
                                "• |r| 0.3-0.5: Moderate relationship<br/>",
                                "• |r| > 0.5: Strong relationship<br/>",
                                "• p < 0.05: Statistically significant (if p-values shown)")
          
          HTML(interp_text)
        })
        
        # Update R log
        r_log <- paste0(r_log, "Analysis completed at: ", Sys.time(), "\n")
        r_log <- paste0(r_log, "Strong correlations found: ", nrow(strong_corrs), "\n")
        r_log <- paste0(r_log, "Moderate correlations found: ", nrow(moderate_corrs), "\n")
        r_log <- paste0(r_log, "=== End of Log ===")
        
        corr_results$r_log <- r_log
        
        showNotification("Correlation analysis completed successfully!", type = "message")
        
      }, error = function(e) {
        error_msg <- paste("Error in correlation analysis:", e$message)
        showNotification(error_msg, type = "error")
        corr_results$r_log <- paste0(corr_results$r_log, "\nERROR: ", error_msg)
      })
    })
    
    # R code output
    output$corr_r_code <- renderText({
      if(is.null(input$corr_vars) || length(input$corr_vars) < 2) return("# Select at least 2 variables for correlation")
      
      vars_str <- paste0("c('", paste(input$corr_vars, collapse = "', '"), "')")
      r_code <- paste0("# Enhanced Correlation Analysis\n")
      r_code <- paste0(r_code, "# Load required packages\n")
      r_code <- paste0(r_code, "required_packages <- c('corrplot', 'psych'")
      
      if(input$use_partial) {
        r_code <- paste0(r_code, ", 'ppcor'")
      }
      if(input$plot_type == "network") {
        r_code <- paste0(r_code, ", 'qgraph'")
      }
      if(input$plot_type == "heatmap") {
        r_code <- paste0(r_code, ", 'pheatmap'")
      }
      
      r_code <- paste0(r_code, ")\n")
      r_code <- paste0(r_code, "for(pkg in required_packages) {\n")
      r_code <- paste0(r_code, "  if(!require(pkg, character.only = TRUE)) {\n")
      r_code <- paste0(r_code, "    install.packages(pkg)\n")
      r_code <- paste0(r_code, "    library(pkg, character.only = TRUE)\n")
      r_code <- paste0(r_code, "  }\n")
      r_code <- paste0(r_code, "}\n\n")
      
      r_code <- paste0(r_code, "selected_vars <- ", vars_str, "\n")
      r_code <- paste0(r_code, "corr_data <- data[, selected_vars, drop = FALSE]\n")
      r_code <- paste0(r_code, "corr_data <- na.omit(corr_data)\n\n")
      
      if(input$use_partial && !is.null(input$control_vars) && length(input$control_vars) > 0) {
        control_str <- paste0("c('", paste(input$control_vars, collapse = "', '"), "')")
        r_code <- paste0(r_code, "# Partial correlation\n")
        r_code <- paste0(r_code, "control_vars <- ", control_str, "\n")
        r_code <- paste0(r_code, "all_vars <- c(selected_vars, control_vars)\n")
        r_code <- paste0(r_code, "all_data <- data[, all_vars, drop = FALSE]\n")
        r_code <- paste0(r_code, "all_data <- na.omit(all_data)\n")
        
        if(input$partial_method == "partial") {
          r_code <- paste0(r_code, "pcor_result <- pcor(all_data, method = '", input$corr_method, "')\n")
          r_code <- paste0(r_code, "corr_matrix <- pcor_result$estimate[1:length(selected_vars), 1:length(selected_vars)]\n")
        } else {
          r_code <- paste0(r_code, "spcor_result <- spcor(all_data, method = '", input$corr_method, "')\n")
          r_code <- paste0(r_code, "corr_matrix <- spcor_result$estimate[1:length(selected_vars), 1:length(selected_vars)]\n")
        }
      } else {
        r_code <- paste0(r_code, "# Standard correlation\n")
        r_code <- paste0(r_code, "corr_matrix <- cor(corr_data, method = '", input$corr_method, "')\n")
      }
      
      r_code <- paste0(r_code, "\n# Visualization\n")
      if(input$plot_type == "corrplot") {
        r_code <- paste0(r_code, "corrplot(corr_matrix, method = 'color', type = 'upper')")
      } else if(input$plot_type == "network") {
        r_code <- paste0(r_code, "library(qgraph)\n")
        r_code <- paste0(r_code, "qgraph(corr_matrix, graph = 'cor', layout = 'spring')")
      } else {
        r_code <- paste0(r_code, "library(pheatmap)\n")
        r_code <- paste0(r_code, "pheatmap(corr_matrix, cluster_rows = TRUE, cluster_cols = TRUE)")
      }
      
      return(r_code)
    })
    
    # R log output
    output$r_log_output <- renderText({
      if(is.null(corr_results$r_log) || corr_results$r_log == "") {
        return("R log will appear here after running correlation analysis...")
      }
      return(corr_results$r_log)
    })
    
    # Download handler
    output$download_corr <- downloadHandler(
      filename = function() {
        paste0("correlation_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if(!is.null(corr_results$matrix)) {
          write.csv(corr_results$matrix, file, row.names = TRUE)
        }
      }
    )
  })
}

# ===================================================================
# END OF CORRELATION MODULE
# ===================================================================