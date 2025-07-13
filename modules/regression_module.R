# ===================================================================
# REGRESSION MODULE: modules/regression_module.R
# ===================================================================

library(sandwich)
library(lmtest)
library(broom)
library(performance)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT) 
library(plotly) 
library(htmltools)
library(stats)  

apply_multiple_comparisons_robust <- function(model, method, alpha, use_robust = FALSE) {
  tryCatch({
    if(use_robust) {
      robust_vcov <- vcovHC(model, type = "HC1")
      robust_summary <- coeftest(model, vcov = robust_vcov)
      
      if(nrow(robust_summary) <= 1) {
        return(NULL)
      }
      
      p_values <- robust_summary[-1, 4, drop = FALSE]
      
      coef_df <- data.frame(
        term = rownames(robust_summary)[-1],
        estimate = robust_summary[-1, 1],
        std.error = robust_summary[-1, 2],
        statistic = robust_summary[-1, 3],
        p.value = robust_summary[-1, 4]
      )
    } else {
      coef_summary <- summary(model)$coefficients
      
      if(nrow(coef_summary) <= 1) {
        return(NULL)
      }
      
      if(ncol(coef_summary) < 4 || !("Pr(>|t|)" %in% colnames(coef_summary))) {
        return(NULL)
      }
      
      p_values <- coef_summary[-1, "Pr(>|t|)", drop = FALSE]
      
      coef_df <- tidy(model, conf.int = TRUE)
      coef_df <- coef_df[coef_df$term != "(Intercept)", ]
    }
    
    if(length(p_values) == 0 || all(is.na(p_values))) {
      return(NULL)
    }
    
    corrected_p <- p.adjust(as.vector(p_values), method = method)
    
    if(nrow(coef_df) == 0) {
      return(NULL)
    }
    
    if(method == "bonferroni") {
      corrected_alpha <- alpha / length(p_values)
    } else {
      corrected_alpha <- alpha
    }
    
    t_critical <- qt(1 - corrected_alpha/2, df = model$df.residual)
    
    coef_df$conf.low_corrected <- coef_df$estimate - t_critical * coef_df$std.error
    coef_df$conf.high_corrected <- coef_df$estimate + t_critical * coef_df$std.error
    coef_df$p.value_corrected <- corrected_p
    
    return(coef_df)
  }, error = function(e) {
    return(NULL)
  })
}

compare_models <- function(models, model_names = NULL, type = "ols") {
  tryCatch({
    if(length(models) < 2) {
      return(NULL)
    }
    
    if(is.null(model_names)) {
      model_names <- paste("Model", 1:length(models))
    }
    
    if(type == "ols") {
      
      comparison_df <- data.frame(
        Model = model_names,
        R_squared = sapply(models, function(m) summary(m)$r.squared),
        Adj_R_squared = sapply(models, function(m) summary(m)$adj.r.squared),
        F_statistic = sapply(models, function(m) summary(m)$fstatistic[1]),
        p_value = sapply(models, function(m) {
          f_stat <- summary(m)$fstatistic
          pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        }),
        AIC = sapply(models, AIC),
        BIC = sapply(models, BIC),
        RMSE = sapply(models, function(m) sqrt(mean(residuals(m)^2))),
        stringsAsFactors = FALSE
      )
    } else {
      
      comparison_df <- data.frame(
        Model = model_names,
        McFadden_R2 = sapply(models, function(m) 1 - (m$deviance / m$null.deviance)),
        AIC = sapply(models, AIC),
        BIC = sapply(models, BIC),
        Log_Likelihood = sapply(models, logLik),
        Deviance = sapply(models, function(m) m$deviance),
        Null_Deviance = sapply(models, function(m) m$null.deviance),
        stringsAsFactors = FALSE
      )
    }
    
    return(comparison_df)
  }, error = function(e) {
    return(NULL)
  })
}

anova_comparison <- function(models, model_names = NULL, type = "ols") {
  tryCatch({
    if(length(models) < 2) {
      return(NULL)
    }
    
    if(is.null(model_names)) {
      model_names <- paste("Model", 1:length(models))
    }
    
    if(type == "ols") {
      
      anova_result <- do.call(anova, models)
      
      if(nrow(anova_result) == length(models)) {
        anova_result$Model <- model_names
        anova_result <- anova_result[, c("Model", setdiff(names(anova_result), "Model"))]
      }
      
      return(anova_result)
    } else {
     
      lrt_result <- do.call(anova, c(models, list(test = "LRT")))
      
      if(nrow(lrt_result) == length(models)) {
        lrt_result$Model <- model_names
        lrt_result <- lrt_result[, c("Model", setdiff(names(lrt_result), "Model"))]
      }
      
      return(lrt_result)
    }
  }, error = function(e) {
    return(NULL)
  })
}

regression_UI <- function(id, title) {
  ns <- NS(id)
  
  if(id == "ols") {
    tabItem(tabName = "ols",
            fluidRow(
              box(title = "OLS Regression Setup", status = "primary", solidHeader = TRUE, width = 4,
                  conditionalPanel(condition = "output.fileUploaded",
                                   selectInput(ns("dv"), "Dependent Variable:", choices = NULL),
                                   selectInput(ns("iv"), "Independent Variables:", choices = NULL, multiple = TRUE),
                                   
                                   div(style = "text-align: center; margin: 10px 0;",
                                       actionButton(ns("reset_vars"), "Reset Variables", 
                                                    class = "btn-secondary btn-sm", 
                                                    style = "margin-right: 10px;")),
                                   
                                   checkboxInput(ns("robust"), "Robust Standard Errors", value = FALSE),
                                   
                                   h5("Multiple Comparisons Correction:"),
                                   checkboxInput(ns("apply_correction"), "Apply Multiple Comparisons Correction", value = FALSE),
                                   conditionalPanel(condition = paste0("input['", ns("apply_correction"), "']"),
                                                    selectInput(ns("correction_method"), "Correction Method:", 
                                                                choices = list("Bonferroni" = "bonferroni",
                                                                               "Holm" = "holm", 
                                                                               "Benjamini-Hochberg (FDR)" = "BH",
                                                                               "Benjamini-Yekutieli" = "BY"),
                                                                selected = "bonferroni"),
                                                    numericInput(ns("alpha_level"), "Alpha Level:", 
                                                                 value = 0.05, min = 0.001, max = 0.2, step = 0.001),
                                                    p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                                                      "Note: Confidence intervals will be adjusted based on the correction method and alpha level.")),
                                   
                                   h5("Model Comparison:"),
                                   checkboxInput(ns("enable_comparison"), "Enable Model Comparison", value = FALSE),
                                   conditionalPanel(condition = paste0("input['", ns("enable_comparison"), "']"),
                                                    p(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                                      "Build multiple models to compare their performance."),
                                                    actionButton(ns("add_model"), "Add Current Model", class = "btn-info btn-sm"),
                                                    br(), br(),
                                                    verbatimTextOutput(ns("stored_models")),
                                                    actionButton(ns("clear_models"), "Clear All Models", class = "btn-warning btn-sm")),
                                   
                                   actionButton(ns("run_regression"), "Run OLS Regression", class = "btn-primary"),
                                   br(), br(),
                                   h5("R Code for OLS:"),
                                   div(class = "r-code-box", verbatimTextOutput(ns("r_code")))
                  )
              ),
              box(title = "Regression Results", status = "info", solidHeader = TRUE, width = 8,
                  verbatimTextOutput(ns("summary")), br(),
                  h4("Model Fit Statistics:"), DT::dataTableOutput(ns("fit_stats"))
              )
            ),
            fluidRow(
              box(title = "Coefficient Plot", status = "info", solidHeader = TRUE, width = 6,
                  htmlOutput(ns("ci_note")),
                  plotlyOutput(ns("coef_plot"), height = "400px")
              ),
              box(title = "Corrected Confidence Intervals", status = "info", solidHeader = TRUE, width = 6,
                  conditionalPanel(condition = paste0("input['", ns("apply_correction"), "']"),
                                   p(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                     "Confidence intervals adjusted for multiple comparisons."),
                                   DT::dataTableOutput(ns("corrected_ci"))),
                  conditionalPanel(condition = paste0("!input['", ns("apply_correction"), "']"),
                                   p("Enable multiple comparisons correction to view corrected confidence intervals."))
              )
            ),
            fluidRow(
              box(title = "Model Comparison", status = "info", solidHeader = TRUE, width = 12,
                  conditionalPanel(condition = paste0("input['", ns("enable_comparison"), "']"),
                                   h5("Model Performance Comparison:"),
                                   DT::dataTableOutput(ns("model_comparison")),
                                   br(),
                                   h5("ANOVA Comparison (Nested Models):"),
                                   verbatimTextOutput(ns("anova_comparison")),
                                   br(),
                                   h5("Model Comparison Plot:"),
                                   plotlyOutput(ns("comparison_plot"), height = "400px")),
                  conditionalPanel(condition = paste0("!input['", ns("enable_comparison"), "']"),
                                   p("Enable model comparison to compare multiple models."))
              )
            ),
            fluidRow(
              box(title = "OLS Interpretation", status = "info", solidHeader = TRUE, width = 12,
                  div(class = "interpretation-box", htmlOutput(ns("interpretation")))
              )
            ),
            fluidRow(
              box(title = "Diagnostic Plots", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput(ns("diagnostics"), height = "500px"),
                  br(),
                  div(class = "interpretation-box", 
                      h5("Diagnostic Plot Interpretation:"),
                      HTML("<strong>Residuals vs Fitted:</strong> Check for linearity and homoscedasticity. Points should be randomly scattered around the horizontal line at 0.<br/>
                           <strong>Q-Q Plot:</strong> Check for normality of residuals. Points should follow the diagonal line closely.<br/>
                           <strong>Scale-Location:</strong> Check for homoscedasticity. The red line should be roughly horizontal.<br/>
                           <strong>Residuals vs Leverage:</strong> Identify influential observations. Look for points outside Cook's distance lines."))
              )
            )
    )
  } else {
    tabItem(tabName = "logit",
            fluidRow(
              box(title = "Logistic Regression Setup", status = "primary", solidHeader = TRUE, width = 4,
                  conditionalPanel(condition = "output.fileUploaded",
                                   selectInput(ns("dv"), "Dependent Variable (Binary):", choices = NULL),
                                   p(style = "font-size: 12px; color: #666; margin-top: -5px; margin-bottom: 10px;",
                                     "Note: Variable must contain only 0 and 1 values."),
                                   selectInput(ns("iv"), "Independent Variables:", choices = NULL, multiple = TRUE),
                                   
                                   div(style = "text-align: center; margin: 10px 0;",
                                       actionButton(ns("reset_vars"), "Reset Variables", 
                                                    class = "btn-secondary btn-sm", 
                                                    style = "margin-right: 10px;")),
                                   
                                   h5("Multiple Comparisons Correction:"),
                                   checkboxInput(ns("apply_correction"), "Apply Multiple Comparisons Correction", value = FALSE),
                                   conditionalPanel(condition = paste0("input['", ns("apply_correction"), "']"),
                                                    selectInput(ns("correction_method"), "Correction Method:", 
                                                                choices = list("Bonferroni" = "bonferroni",
                                                                               "Holm" = "holm", 
                                                                               "Benjamini-Hochberg (FDR)" = "BH",
                                                                               "Benjamini-Yekutieli" = "BY"),
                                                                selected = "bonferroni"),
                                                    numericInput(ns("alpha_level"), "Alpha Level:", 
                                                                 value = 0.05, min = 0.001, max = 0.2, step = 0.001),
                                                    p(style = "font-size: 12px; color: #666; margin-top: 5px;",
                                                      "Note: Confidence intervals will be adjusted based on the correction method and alpha level.")),
                                   
                                   h5("Model Comparison:"),
                                   checkboxInput(ns("enable_comparison"), "Enable Model Comparison", value = FALSE),
                                   conditionalPanel(condition = paste0("input['", ns("enable_comparison"), "']"),
                                                    p(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                                      "Build multiple models to compare their performance."),
                                                    actionButton(ns("add_model"), "Add Current Model", class = "btn-info btn-sm"),
                                                    br(), br(),
                                                    verbatimTextOutput(ns("stored_models")),
                                                    actionButton(ns("clear_models"), "Clear All Models", class = "btn-warning btn-sm")),
                                   
                                   actionButton(ns("run_regression"), "Run Logistic Regression", class = "btn-primary"),
                                   br(), br(),
                                   h5("R Code for Logistic Regression:"),
                                   div(class = "r-code-box", verbatimTextOutput(ns("r_code")))
                  )
              ),
              box(title = "Logistic Regression Results", status = "info", solidHeader = TRUE, width = 8,
                  verbatimTextOutput(ns("summary")), br(),
                  h4("Model Fit Statistics:"), DT::dataTableOutput(ns("fit_stats"))
              )
            ),
            fluidRow(
              box(title = "Coefficient Plot (Log-Odds)", status = "info", solidHeader = TRUE, width = 6,
                  htmlOutput(ns("ci_note")),
                  plotlyOutput(ns("coef_plot"), height = "400px")
              ),
              box(title = "Odds Ratios", status = "info", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput(ns("odds_ratios"))
              )
            ),
            fluidRow(
              box(title = "Corrected Confidence Intervals", status = "info", solidHeader = TRUE, width = 6,
                  conditionalPanel(condition = paste0("input['", ns("apply_correction"), "']"),
                                   p(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                     "Confidence intervals adjusted for multiple comparisons (log-odds scale)."),
                                   DT::dataTableOutput(ns("corrected_ci"))),
                  conditionalPanel(condition = paste0("!input['", ns("apply_correction"), "']"),
                                   p("Enable multiple comparisons correction to view corrected confidence intervals."))
              ),
              box(title = "Predicted Probabilities", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput(ns("predicted_probs"))
              )
            ),
            fluidRow(
              box(title = "Model Comparison", status = "info", solidHeader = TRUE, width = 12,
                  conditionalPanel(condition = paste0("input['", ns("enable_comparison"), "']"),
                                   h5("Model Performance Comparison:"),
                                   DT::dataTableOutput(ns("model_comparison")),
                                   br(),
                                   h5("ANOVA Comparison (Nested Models):"),
                                   verbatimTextOutput(ns("anova_comparison")),
                                   br(),
                                   h5("Model Comparison Plot:"),
                                   plotlyOutput(ns("comparison_plot"), height = "400px")),
                  conditionalPanel(condition = paste0("!input['", ns("enable_comparison"), "']"),
                                   p("Enable model comparison to compare multiple models."))
              )
            ),
            fluidRow(
              box(title = "Logistic Regression Interpretation", status = "info", solidHeader = TRUE, width = 12,
                  div(class = "interpretation-box", htmlOutput(ns("interpretation")))
              )
            )
    )
  }
}

regression_server <- function(id, shared_data, type = "ols") {
  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues(
      model = NULL,
      model_summary = NULL,
      coef_data = NULL,
      corrected_coef_data = NULL,
      mcfadden_r2 = NULL,
      current_dv = NULL,
      current_iv = NULL,
      apply_correction_val = FALSE,
      correction_method_val = "bonferroni",
      alpha_level_val = 0.05,
      robust_summary = NULL,
      robust_vcov = NULL,
      cached_predictions = NULL,
      cached_fitted_values = NULL,
      cached_residuals = NULL,
      cached_standardized_residuals = NULL,
      cached_leverage = NULL,
      cached_cooks_distance = NULL,
      stored_models = list(),
      model_names = character(0),
      comparison_data = NULL,
      anova_result = NULL
    )
    
    output$ci_note <- renderUI({
      if(!is.null(input$apply_correction) && input$apply_correction && 
         !is.null(input$correction_method) && !is.null(input$alpha_level)) {
        
        if(!is.null(values$coef_data) && nrow(values$coef_data) > 0) {
          n_tests <- nrow(values$coef_data)
          
          if(input$correction_method == "bonferroni") {
            effective_alpha <- input$alpha_level / n_tests
            ci_level <- round((1 - effective_alpha) * 100, 1)
            correction_text <- paste0("Bonferroni corrected (α = ", input$alpha_level, 
                                      " ÷ ", n_tests, " tests = ", 
                                      round(effective_alpha, 4), ")")
          } else {
            ci_level <- round((1 - input$alpha_level) * 100, 1)
            correction_text <- paste0(input$correction_method, " corrected (α = ", input$alpha_level, ")")
          }
          
          sig_note <- "Red = significant (CI excludes 0), Gray = not significant (CI includes 0)"
          
          div(
            p(style = "font-size: 11px; color: #666; margin-bottom: 3px; text-align: center;",
              paste0("Error bars show ", ci_level, "% confidence intervals")),
            p(style = "font-size: 10px; color: #777; margin-bottom: 3px; text-align: center;",
              correction_text),
            p(style = "font-size: 10px; color: #888; margin-bottom: 5px; text-align: center;",
              sig_note)
          )
        } else {
          if(input$correction_method == "bonferroni") {
            correction_text <- paste0("Bonferroni corrected (α = ", input$alpha_level, ")")
          } else {
            correction_text <- paste0(input$correction_method, " corrected (α = ", input$alpha_level, ")")
          }
          
          ci_level <- round((1 - input$alpha_level) * 100, 1)
          sig_note <- "Red = significant (CI excludes 0), Gray = not significant (CI includes 0)"
          
          div(
            p(style = "font-size: 11px; color: #666; margin-bottom: 3px; text-align: center;",
              paste0("Error bars will show confidence intervals")),
            p(style = "font-size: 10px; color: #777; margin-bottom: 3px; text-align: center;",
              correction_text),
            p(style = "font-size: 10px; color: #888; margin-bottom: 5px; text-align: center;",
              sig_note)
          )
        }
      } else {
        div(
          p(style = "font-size: 11px; color: #666; margin-bottom: 3px; text-align: center;",
            "Error bars show 95% confidence intervals"),
          p(style = "font-size: 10px; color: #888; margin-bottom: 5px; text-align: center;",
            "Red = significant (CI excludes 0), Gray = not significant (CI includes 0)")
        )
      }
    })
    
    observe({
      req(shared_data$working_data)
      
      if(type == "ols") {
        dv_choices <- get_variable_choices(shared_data$working_data, "numeric")
        iv_choices <- get_variable_choices(shared_data$working_data, "all")
      } else {
        dv_choices <- get_variable_choices(shared_data$working_data, "binary")
        iv_choices <- get_variable_choices(shared_data$working_data, "all")
      }
      
      if(length(dv_choices) > 0) {
        dv_choices <- c("Select variable" = "", dv_choices)
      }
      
      updateSelectInput(session, "dv", choices = dv_choices, selected = "")
      updateSelectInput(session, "iv", choices = iv_choices, selected = character(0))
    })
    
    observeEvent(input$reset_vars, {
      updateSelectInput(session, "dv", selected = "")
      updateSelectInput(session, "iv", selected = character(0))
      showNotification("Variables reset successfully.", type = "message")
    })
    
    output$stored_models <- renderText({
      if(length(values$stored_models) == 0) {
        return("No models stored yet.")
      } else {
        model_info <- character(0)
        for(i in 1:length(values$stored_models)) {
          formula_str <- deparse(formula(values$stored_models[[i]]))
          model_info <- c(model_info, paste0(values$model_names[i], ": ", formula_str))
        }
        return(paste(model_info, collapse = "\n"))
      }
    })
    
    observeEvent(input$add_model, {
      req(values$model, values$current_dv, values$current_iv)
      
      model_name <- paste0("Model_", length(values$stored_models) + 1, "_",
                           paste(values$current_iv, collapse = "_"))
      
      values$stored_models <- append(values$stored_models, list(values$model))
      values$model_names <- c(values$model_names, model_name)
      
      if(length(values$stored_models) >= 2) {
        values$comparison_data <- compare_models(values$stored_models, values$model_names, type)
        
        if(type == "ols") {
          values$anova_result <- tryCatch({
            anova_comparison(values$stored_models, values$model_names, type)
          }, error = function(e) {
            "Models are not nested or ANOVA comparison failed."
          })
        } else {
          values$anova_result <- tryCatch({
            anova_comparison(values$stored_models, values$model_names, type)
          }, error = function(e) {
            "Models are not nested or Likelihood Ratio Test failed."
          })
        }
      }
      
      showNotification(paste("Model added:", model_name), type = "message")
      
      if(length(values$stored_models) >= 2) {
        comparison_code <- generate_comparison_code(values$stored_models, values$model_names, type)
        add_r_code(shared_data, comparison_code, "Model Comparison")
      }
    })
    
    observeEvent(input$clear_models, {
      values$stored_models <- list()
      values$model_names <- character(0)
      values$comparison_data <- NULL
      values$anova_result <- NULL
      showNotification("All stored models cleared.", type = "message")
    })
    
    output$model_comparison <- DT::renderDataTable({
      if(!is.null(values$comparison_data)) {
        DT::datatable(values$comparison_data, options = list(scrollX = TRUE)) %>%
          formatRound(columns = setdiff(names(values$comparison_data), "Model"), digits = 4)
      } else {
        empty_df <- data.frame(Message = "Add at least 2 models to compare")
        DT::datatable(empty_df, options = list(dom = 't'))
      }
    })
    
    output$anova_comparison <- renderPrint({
      if(!is.null(values$anova_result)) {
        if(is.character(values$anova_result)) {
          cat(values$anova_result)
        } else {
          if(type == "ols") {
            cat("=== ANOVA Comparison ===\n")
          } else {
            cat("=== Likelihood Ratio Test ===\n")
          }
          print(values$anova_result)
        }
      } else {
        if(type == "ols") {
          cat("Add at least 2 models to perform ANOVA comparison.")
        } else {
          cat("Add at least 2 models to perform Likelihood Ratio Test.")
        }
      }
    })
    
    output$comparison_plot <- renderPlotly({
      if(!is.null(values$comparison_data) && nrow(values$comparison_data) >= 2) {
        if(type == "ols") {
          # Create comparison plot for OLS models
          plot_data <- values$comparison_data
          
          p <- ggplot(plot_data, aes(x = Model)) +
            geom_col(aes(y = Adj_R_squared), fill = "#3498db", alpha = 0.7) +
            geom_text(aes(y = Adj_R_squared, label = round(Adj_R_squared, 3)), 
                      vjust = -0.5, size = 3) +
            labs(title = "Model Comparison: Adjusted R-squared",
                 x = "Models", y = "Adjusted R-squared") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          ggplotly(p)
        } else {
          
          plot_data <- values$comparison_data
          
          p <- ggplot(plot_data, aes(x = Model)) +
            geom_col(aes(y = McFadden_R2), fill = "#e74c3c", alpha = 0.7) +
            geom_text(aes(y = McFadden_R2, label = round(McFadden_R2, 3)), 
                      vjust = -0.5, size = 3) +
            labs(title = "Model Comparison: McFadden R-squared",
                 x = "Models", y = "McFadden R-squared") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          ggplotly(p)
        }
      } else {
        p <- ggplot() + 
          labs(title = "Add at least 2 models to view comparison plot") +
          theme_minimal()
        ggplotly(p)
      }
    })
    
    check_binary_variable <- function(data, var_name) {
      var_data <- data[[var_name]]
      unique_vals <- unique(var_data[!is.na(var_data)])
      return(all(unique_vals %in% c(0, 1)) && length(unique_vals) == 2)
    }
    
    observeEvent(input$run_regression, {
      req(shared_data$working_data, input$dv, input$iv)
      
      if(input$dv == "" || length(input$iv) == 0 || any(input$iv == "")) {
        showNotification("Please select dependent and independent variables.", type = "warning")
        return()
      }
      
      df <- shared_data$working_data
      
      if(type == "logit") {
        if(!check_binary_variable(df, input$dv)) {
          showNotification(
            HTML("Error: Dependent variable must be binary (0/1 values only).<br/>Please create a binary variable or use the Data Transformation tab."), 
            type = "error", 
            duration = 8000
          )
          return()
        }
      }
      
      formula_str <- paste(input$dv, "~", paste(input$iv, collapse = " + "))
      
      notification_id <- showNotification(
        "Running regression analysis... Please wait.", 
        type = "message", 
        duration = NULL
      )
      
      values$apply_correction_val <- input$apply_correction
      values$correction_method_val <- input$correction_method
      values$alpha_level_val <- input$alpha_level
      values$current_dv <- input$dv
      values$current_iv <- input$iv
      
      tryCatch({
        if(type == "ols") {
          model <- lm(as.formula(formula_str), data = df)
          values$model <- model
          values$model_summary <- summary(model)
          
          values$cached_fitted_values <- fitted(model)
          values$cached_residuals <- residuals(model)
          values$cached_standardized_residuals <- rstandard(model)
          values$cached_leverage <- hatvalues(model)
          values$cached_cooks_distance <- cooks.distance(model)
          
          if(input$robust) {
            robust_vcov <- vcovHC(model, type = "HC1")
            robust_summary <- coeftest(model, vcov = robust_vcov)
            values$robust_summary <- robust_summary
            values$robust_vcov <- robust_vcov
            
            r_code_text <- "# OLS Regression with Robust Standard Errors\n"
            r_code_text <- paste0(r_code_text, "library(sandwich)\n")
            r_code_text <- paste0(r_code_text, "library(lmtest)\n\n")
            r_code_text <- paste0(r_code_text, "model <- lm(", formula_str, ", data = data)\n")
            r_code_text <- paste0(r_code_text, "summary(model)\n\n")
            r_code_text <- paste0(r_code_text, "robust_se <- coeftest(model, vcov = vcovHC(model, type = 'HC1'))\n")
            r_code_text <- paste0(r_code_text, "print(robust_se)\n\n")
          } else {
            values$robust_summary <- NULL
            values$robust_vcov <- NULL
            
            r_code_text <- "# OLS Regression\n"
            r_code_text <- paste0(r_code_text, "model <- lm(", formula_str, ", data = data)\n")
            r_code_text <- paste0(r_code_text, "summary(model)\n\n")
          }
          
          if(values$apply_correction_val) {
            r_code_text <- paste0(r_code_text, "# Multiple Comparisons Correction\n")
            r_code_text <- paste0(r_code_text, "library(broom)\n")
            
            if(input$robust) {
              r_code_text <- paste0(r_code_text, "robust_se <- coeftest(model, vcov = vcovHC(model, type = 'HC1'))\n")
              r_code_text <- paste0(r_code_text, "p_values <- robust_se[-1, 4]\n")
            } else {
              r_code_text <- paste0(r_code_text, "coef_df <- tidy(model, conf.int = TRUE)\n")
              r_code_text <- paste0(r_code_text, "p_values <- coef_df$p.value[-1]\n")
            }
            
            r_code_text <- paste0(r_code_text, "corrected_p <- p.adjust(p_values, method = '", values$correction_method_val, "')\n")
            r_code_text <- paste0(r_code_text, "print(corrected_p)\n\n")
          }
          
          add_r_code(shared_data, r_code_text, "OLS Regression")
        } else {
          model <- glm(as.formula(formula_str), data = df, family = binomial(),
                       control = glm.control(epsilon = 1e-6, maxit = 20, trace = FALSE))
          values$model <- model
          values$model_summary <- summary(model)
          
          values$cached_predictions <- predict(model, type = "response")
          values$cached_linear_predictions <- predict(model, type = "link")
          values$cached_residuals <- residuals(model)
          
          null_deviance <- model$null.deviance
          residual_deviance <- model$deviance
          mcfadden_r2 <- 1 - (residual_deviance / null_deviance)
          values$mcfadden_r2 <- mcfadden_r2
          
          r_code_text <- "# Optimized Logistic Regression\n"
          r_code_text <- paste0(r_code_text, "model <- glm(", formula_str, ", data = data, family = binomial(),\n")
          r_code_text <- paste0(r_code_text, "              control = glm.control(epsilon = 1e-6, maxit = 20, trace = FALSE))\n")
          r_code_text <- paste0(r_code_text, "summary(model)\n\n")
          r_code_text <- paste0(r_code_text, "# Fast odds ratios calculation\n")
          r_code_text <- paste0(r_code_text, "coef_est <- coef(model)\n")
          r_code_text <- paste0(r_code_text, "coef_se <- sqrt(diag(vcov(model)))\n")
          r_code_text <- paste0(r_code_text, "z_critical <- qnorm(0.975)\n")
          r_code_text <- paste0(r_code_text, "odds_ratios <- data.frame(\n")
          r_code_text <- paste0(r_code_text, "  estimate = exp(coef_est),\n")
          r_code_text <- paste0(r_code_text, "  conf.low = exp(coef_est - z_critical * coef_se),\n")
          r_code_text <- paste0(r_code_text, "  conf.high = exp(coef_est + z_critical * coef_se)\n")
          r_code_text <- paste0(r_code_text, ")\n")
          r_code_text <- paste0(r_code_text, "print(odds_ratios)\n\n")
          
          if(values$apply_correction_val) {
            r_code_text <- paste0(r_code_text, "# Multiple Comparisons Correction\n")
            r_code_text <- paste0(r_code_text, "library(broom)\n")
            r_code_text <- paste0(r_code_text, "coef_df <- tidy(model, conf.int = TRUE)\n")
            r_code_text <- paste0(r_code_text, "p_values <- coef_df$p.value[-1]\n")
            r_code_text <- paste0(r_code_text, "corrected_p <- p.adjust(p_values, method = '", values$correction_method_val, "')\n")
            r_code_text <- paste0(r_code_text, "print(corrected_p)\n\n")
          }
          
          add_r_code(shared_data, r_code_text, "Logistic Regression")
        }
        
        if(type == "ols") {
          output$summary <- renderPrint({ 
            if(input$robust && !is.null(values$robust_summary)) {
              cat("=== OLS Regression with Robust Standard Errors (HC1) ===\n\n")
              print(values$robust_summary)
              cat("\n=== Regular Summary (for R-squared, etc.) ===\n")
              print(values$model_summary)
            } else {
              values$model_summary
            }
          })
          
          model_stats <- data.frame(
            Statistic = c("R-squared", "Adjusted R-squared", "F-statistic", "p-value", "AIC", "BIC"),
            Value = c(
              round(values$model_summary$r.squared, 4), 
              round(values$model_summary$adj.r.squared, 4),
              round(values$model_summary$fstatistic[1], 2),
              round(pf(values$model_summary$fstatistic[1], values$model_summary$fstatistic[2], 
                       values$model_summary$fstatistic[3], lower.tail = FALSE), 4),
              round(AIC(model), 2), 
              round(BIC(model), 2)
            )
          )
          
          output$fit_stats <- DT::renderDataTable({
            DT::datatable(model_stats, options = list(dom = 't'))
          })
          
          if(input$robust && !is.null(values$robust_summary)) {
            coef_df <- data.frame(
              term = rownames(values$robust_summary)[-1],
              estimate = values$robust_summary[-1, 1],
              std.error = values$robust_summary[-1, 2],
              statistic = values$robust_summary[-1, 3],
              p.value = values$robust_summary[-1, 4]
            )
            
            t_critical <- qt(0.975, df = model$df.residual)
            coef_df$conf.low <- coef_df$estimate - t_critical * coef_df$std.error
            coef_df$conf.high <- coef_df$estimate + t_critical * coef_df$std.error
            
            values$coef_data <- coef_df
          } else {
            coef_df <- tidy(model, conf.int = TRUE)
            coef_df <- coef_df[coef_df$term != "(Intercept)", ]
            values$coef_data <- coef_df
          }
          
          if(values$apply_correction_val) {
            corrected_df <- apply_multiple_comparisons_robust(model, values$correction_method_val, 
                                                              values$alpha_level_val, input$robust)
            values$corrected_coef_data <- corrected_df
          } else {
            values$corrected_coef_data <- NULL
          }
          
          output$corrected_ci <- DT::renderDataTable({
            if(values$apply_correction_val && !is.null(values$corrected_coef_data)) {
              display_df <- values$corrected_coef_data[, c("term", "estimate", "conf.low_corrected", "conf.high_corrected", "p.value_corrected")]
              names(display_df) <- c("Variable", "Coefficient", "Lower CI", "Upper CI", "Corrected p-value")
              DT::datatable(display_df, options = list(scrollX = TRUE)) %>%
                formatRound(columns = c("Coefficient", "Lower CI", "Upper CI"), digits = 4) %>%
                formatRound(columns = "Corrected p-value", digits = 6)
            } else {
              empty_df <- data.frame(Message = "Enable multiple comparisons correction to view results")
              DT::datatable(empty_df, options = list(dom = 't'))
            }
          })
        } else {
          output$summary <- renderPrint({ values$model_summary })
          
          model_stats <- data.frame(
            Statistic = c("McFadden R-squared", "AIC", "BIC", "Null Deviance", "Residual Deviance"),
            Value = c(round(mcfadden_r2, 4), round(AIC(model), 2), round(BIC(model), 2),
                      round(null_deviance, 2), round(residual_deviance, 2))
          )
          
          output$fit_stats <- DT::renderDataTable({
            DT::datatable(model_stats, options = list(dom = 't'))
          })
          
          coef_df <- tidy(model, conf.int = TRUE)
          coef_df <- coef_df[coef_df$term != "(Intercept)", ]
          values$coef_data <- coef_df
          
          if(values$apply_correction_val) {
            corrected_df <- apply_multiple_comparisons(model, values$correction_method_val, values$alpha_level_val)
            values$corrected_coef_data <- corrected_df
          } else {
            values$corrected_coef_data <- NULL
          }
          
          output$corrected_ci <- DT::renderDataTable({
            if(values$apply_correction_val && !is.null(values$corrected_coef_data)) {
              display_df <- values$corrected_coef_data[, c("term", "estimate", "conf.low_corrected", "conf.high_corrected", "p.value_corrected")]
              names(display_df) <- c("Variable", "Log-Odds", "Lower CI", "Upper CI", "Corrected p-value")
              DT::datatable(display_df, options = list(scrollX = TRUE)) %>%
                formatRound(columns = c("Log-Odds", "Lower CI", "Upper CI"), digits = 4) %>%
                formatRound(columns = "Corrected p-value", digits = 6)
            } else {
              empty_df <- data.frame(Message = "Enable multiple comparisons correction to view results")
              DT::datatable(empty_df, options = list(dom = 't'))
            }
          })
        }
        
        output$coef_plot <- renderPlotly({
          req(values$coef_data)
          
          if(nrow(values$coef_data) == 0) {
            p <- ggplot() + 
              labs(title = "No coefficients to display") +
              theme_minimal()
            return(ggplotly(p))
          }
          
          plot_data <- values$coef_data
          if(values$apply_correction_val && !is.null(values$corrected_coef_data)) {
            plot_data$conf.low <- values$corrected_coef_data$conf.low_corrected
            plot_data$conf.high <- values$corrected_coef_data$conf.high_corrected
            title_suffix <- paste0(" (", values$correction_method_val, " corrected)")
          } else {
            title_suffix <- ""
          }
          
          plot_data$significant <- !(plot_data$conf.low <= 0 & plot_data$conf.high >= 0)
          
          plot_title <- if(type == "ols") {
            paste0("Coefficient Plot", title_suffix)
          } else {
            paste0("Coefficient Plot (Log-Odds)", title_suffix)
          }
          
          y_label <- if(type == "ols") "Coefficient Estimate" else "Log-Odds Estimate"
          
          p <- ggplot(plot_data, aes(x = reorder(term, estimate), y = estimate)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "#666666", size = 0.8) +
            geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                          width = 0, size = 1.2, color = "#2c3e50") +
            geom_point(aes(color = significant, fill = significant), 
                       size = 4, shape = 21, stroke = 1.5) +
            scale_color_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#e74c3c")) +
            scale_fill_manual(values = c("FALSE" = "#ecf0f1", "TRUE" = "#e74c3c")) +
            coord_flip() +
            labs(title = plot_title, x = "Variables", y = y_label) +
            theme_minimal() +
            theme(
              panel.grid.major.y = element_line(color = "#f8f9fa", size = 0.5),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line(color = "#e9ecef", size = 0.3),
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 11),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = c("x", "y", "ymin", "ymax")) %>%
            layout(showlegend = FALSE)
        })
        
        if(type == "logit") {
          output$odds_ratios <- renderDataTable({
            req(values$model)
            
            coef_est <- coef(values$model)
            coef_se <- sqrt(diag(vcov(values$model)))
            z_critical <- qnorm(0.975)
            
            coef_df_or <- data.frame(
              term = names(coef_est),
              estimate = exp(coef_est),
              conf.low = exp(coef_est - z_critical * coef_se),
              conf.high = exp(coef_est + z_critical * coef_se),
              p.value = values$model_summary$coefficients[, 4]
            )
            
            coef_df_or <- coef_df_or[coef_df_or$term != "(Intercept)", ]
            
            if(nrow(coef_df_or) == 0) {
              empty_df <- data.frame(Message = "No coefficients available")
              return(DT::datatable(empty_df, options = list(dom = 't')))
            }
            
            DT::datatable(coef_df_or, options = list(scrollX = TRUE)) %>%
              formatRound(columns = c("estimate", "conf.low", "conf.high"), digits = 3) %>%
              formatRound(columns = "p.value", digits = 4)
          })
          
          output$predicted_probs <- renderPlotly({
            req(values$cached_predictions)
            
            predictions <- values$cached_predictions
            actual_values <- values$model$y
            
            complete_cases_idx <- complete.cases(df[c(values$current_dv, values$current_iv)])
            df_complete <- df[complete_cases_idx, ]
            
            if(length(values$current_iv) == 1) {
              pred_data <- data.frame(
                x = df_complete[[values$current_iv[1]]], 
                predicted = predictions
              )
              
              p <- ggplot(pred_data, aes(x = x, y = predicted)) + 
                geom_point(alpha = 0.6, size = 1.5) +
                geom_smooth(method = "loess", color = "red", se = FALSE) +
                labs(title = "Predicted Probabilities", 
                     x = values$current_iv[1], 
                     y = paste0("P(", values$current_dv, " = 1)")) + 
                theme_minimal() +
                ylim(0, 1)
              ggplotly(p)
            } else {
              pred_data <- data.frame(
                predicted = predictions, 
                actual = actual_values
              )
              
              p <- ggplot(pred_data, aes(x = predicted, fill = factor(actual))) +
                geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
                labs(title = "Distribution of Predicted Probabilities", 
                     x = "Predicted Probability", 
                     y = "Count", 
                     fill = values$current_dv) + 
                theme_minimal() +
                scale_fill_manual(values = c("0" = "#3498db", "1" = "#e74c3c"))
              ggplotly(p)
            }
          })
        }
        
        if(type == "ols") {
          output$diagnostics <- renderPlot({
            req(values$cached_fitted_values, values$cached_residuals, 
                values$cached_standardized_residuals, values$cached_leverage)
            
            df_resid <- data.frame(
              fitted = values$cached_fitted_values, 
              residuals = values$cached_residuals, 
              standardized = values$cached_standardized_residuals,
              sqrt_abs_resid = sqrt(abs(values$cached_standardized_residuals)),
              leverage = values$cached_leverage,
              cooks_distance = values$cached_cooks_distance
            )
            
            par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
            
            plot(df_resid$fitted, df_resid$residuals,
                 main = "Residuals vs Fitted",
                 xlab = "Fitted Values", 
                 ylab = "Residuals",
                 pch = 16, col = rgb(0, 0, 0, 0.6), cex = 0.8)
            abline(h = 0, col = "red", lty = 2, lwd = 2)
            smooth_line <- lowess(df_resid$fitted, df_resid$residuals)
            lines(smooth_line, col = "blue", lwd = 2)
            
            qqnorm(df_resid$standardized, 
                   main = "Normal Q-Q Plot",
                   xlab = "Theoretical Quantiles",
                   ylab = "Standardized Residuals",
                   pch = 16, col = rgb(0, 0, 0, 0.6), cex = 0.8)
            qqline(df_resid$standardized, col = "red", lwd = 2)
            
            plot(df_resid$fitted, df_resid$sqrt_abs_resid,
                 main = "Scale-Location",
                 xlab = "Fitted Values",
                 ylab = expression(sqrt("|Standardized Residuals|")),
                 pch = 16, col = rgb(0, 0, 0, 0.6), cex = 0.8)
            smooth_line2 <- lowess(df_resid$fitted, df_resid$sqrt_abs_resid)
            lines(smooth_line2, col = "red", lwd = 2)
            
            plot(df_resid$leverage, df_resid$standardized,
                 main = "Residuals vs Leverage",
                 xlab = "Leverage",
                 ylab = "Standardized Residuals",
                 pch = 16, col = rgb(0, 0, 0, 0.6), cex = 0.8)
            abline(h = 0, col = "gray", lty = 2)
            smooth_line3 <- lowess(df_resid$leverage, df_resid$standardized)
            lines(smooth_line3, col = "red", lwd = 2)
            
            cook_cutoff <- 4 / length(df_resid$leverage)
            abline(v = cook_cutoff, col = "red", lty = 3, alpha = 0.5)
            
            par(mfrow = c(1, 1))
          })
        }
        
        output$interpretation <- renderUI({
          plot_data <- values$coef_data
          if(values$apply_correction_val && !is.null(values$corrected_coef_data)) {
            plot_data$conf.low <- values$corrected_coef_data$conf.low_corrected
            plot_data$conf.high <- values$corrected_coef_data$conf.high_corrected
          }
          sig_vars <- plot_data$term[!(plot_data$conf.low <= 0 & plot_data$conf.high >= 0)]
          
          if(type == "ols") {
            r_sq <- round(values$model_summary$r.squared * 100, 1)
            
            sig_text <- if(length(sig_vars) > 0) {
              if(values$apply_correction_val) {
                paste0("Statistically significant predictors (", values$correction_method_val, 
                       " corrected CIs exclude 0): ", paste(sig_vars, collapse = ", "))
              } else {
                paste0("Statistically significant predictors (95% CIs exclude 0): ", paste(sig_vars, collapse = ", "))
              }
            } else {
              if(values$apply_correction_val) {
                paste0("No statistically significant predictors found (", values$correction_method_val, 
                       " corrected CIs all include 0).")
              } else {
                "No statistically significant predictors found (95% CIs all include 0)."
              }
            }
            
            correction_text <- if(values$apply_correction_val) {
              paste0("<br/><strong>Multiple Comparisons Correction:</strong><br/>",
                     "Method: ", values$correction_method_val, "<br/>",
                     "Significance threshold: α = ", values$alpha_level_val, "<br/>",
                     "This correction adjusts confidence intervals for testing multiple coefficients simultaneously.")
            } else {
              ""
            }
            
            robust_text <- if(input$robust) {
              "<br/><strong>Robust Standard Errors:</strong><br/>Used HC1 robust standard errors to account for heteroscedasticity."
            } else {
              ""
            }
            
            comparison_text <- if(length(values$stored_models) >= 2) {
              best_model_idx <- which.max(values$comparison_data$Adj_R_squared)
              best_model <- values$comparison_data$Model[best_model_idx]
              paste0("<br/><strong>Model Comparison:</strong><br/>",
                     "Best performing model (highest Adj. R²): ", best_model, "<br/>",
                     "Current model performance ranking: ", which(values$comparison_data$Model == paste0("Model_", length(values$stored_models), "_", paste(values$current_iv, collapse = "_"))), 
                     " out of ", nrow(values$comparison_data), " models")
            } else {
              ""
            }
            
            HTML(paste0(
              "<strong>OLS Regression Results:</strong><br/>",
              "Model explains ", r_sq, "% of variance in ", values$current_dv, "<br/>",
              sig_text, correction_text, robust_text, comparison_text, "<br/><br/>",
              "<strong>Coefficient Interpretation:</strong><br/>",
              "• Each coefficient shows the expected change in ", values$current_dv, " for a 1-unit increase in that predictor<br/>",
              "• Confidence intervals show the range of plausible values for each coefficient<br/>",
              "• <span style='color: #e74c3c;'>Red points</span> = significant (CI excludes 0), <span style='color: #95a5a6;'>Gray points</span> = not significant (CI includes 0)"
            ))
          } else {
            req(values$mcfadden_r2)
            mcfadden_r2_pct <- round(values$mcfadden_r2 * 100, 1)
            
            sig_text <- if(length(sig_vars) > 0) {
              if(values$apply_correction_val) {
                paste0("Statistically significant predictors (", values$correction_method_val, 
                       " corrected CIs exclude 0): ", paste(sig_vars, collapse = ", "))
              } else {
                paste0("Statistically significant predictors (95% CIs exclude 0): ", paste(sig_vars, collapse = ", "))
              }
            } else {
              if(values$apply_correction_val) {
                paste0("No statistically significant predictors found (", values$correction_method_val, 
                       " corrected CIs all include 0).")
              } else {
                "No statistically significant predictors found (95% CIs all include 0)."
              }
            }
            
            correction_text <- if(values$apply_correction_val) {
              paste0("<br/><strong>Multiple Comparisons Correction:</strong><br/>",
                     "Method: ", values$correction_method_val, "<br/>",
                     "Significance threshold: α = ", values$alpha_level_val, "<br/>",
                     "This correction adjusts confidence intervals for testing multiple coefficients simultaneously.")
            } else {
              ""
            }
            
            comparison_text <- if(length(values$stored_models) >= 2) {
              best_model_idx <- which.max(values$comparison_data$McFadden_R2)
              best_model <- values$comparison_data$Model[best_model_idx]
              paste0("<br/><strong>Model Comparison:</strong><br/>",
                     "Best performing model (highest McFadden R²): ", best_model, "<br/>",
                     "Current model performance ranking: ", which(values$comparison_data$Model == paste0("Model_", length(values$stored_models), "_", paste(values$current_iv, collapse = "_"))), 
                     " out of ", nrow(values$comparison_data), " models")
            } else {
              ""
            }
            
            HTML(paste0(
              "<strong>Logistic Regression Results:</strong><br/>",
              "Model pseudo R-squared (McFadden): ", mcfadden_r2_pct, "%<br/>",
              sig_text, correction_text, comparison_text, "<br/><br/>",
              "<strong>Odds Ratio Interpretation:</strong><br/>",
              "• Odds Ratio = 1: No effect on odds<br/>",
              "• Odds Ratio > 1: Increases odds of outcome<br/>",
              "• Odds Ratio < 1: Decreases odds of outcome<br/>",
              "• For example, OR = 2.0 means doubling the odds<br/>",
              "• OR = 0.5 means halving the odds<br/>",
              "• <span style='color: #e74c3c;'>Red points</span> = significant (CI excludes 0), <span style='color: #95a5a6;'>Gray points</span> = not significant (CI includes 0)"
            ))
          }
        })
        
        removeNotification(notification_id)
        showNotification("Regression analysis completed successfully!", type = "message", duration = 3000)
        
      }, error = function(e) {
        removeNotification(notification_id)
        showNotification(paste("Error in regression analysis:", e$message), type = "error", duration = 5000)
      })
    })
    
    output$r_code <- renderText({
      if(is.null(input$dv) || input$dv == "" || is.null(input$iv) || length(input$iv) == 0) {
        return("# Select dependent and independent variables")
      }
      
      formula_str <- paste(input$dv, "~", paste(input$iv, collapse = " + "))
      
      if(type == "ols") {
        r_code_text <- "# OLS Regression Analysis\n"
        r_code_text <- paste0(r_code_text, "library(broom)\n")
        r_code_text <- paste0(r_code_text, "library(ggplot2)\n")
        
        if(!is.null(input$robust) && input$robust) {
          r_code_text <- paste0(r_code_text, "library(sandwich)\n")
          r_code_text <- paste0(r_code_text, "library(lmtest)\n")
        }
        
        if(!is.null(input$enable_comparison) && input$enable_comparison) {
          r_code_text <- paste0(r_code_text, "library(performance)\n")
        }
        
        r_code_text <- paste0(r_code_text, "\nmodel <- lm(", formula_str, ", data = data)\n")
        r_code_text <- paste0(r_code_text, "summary(model)\n\n")
        
        if(!is.null(input$robust) && input$robust) {
          r_code_text <- paste0(r_code_text, "robust_se <- coeftest(model, vcov = vcovHC(model, type = 'HC1'))\n")
          r_code_text <- paste0(r_code_text, "print(robust_se)\n\n")
        }
        
        if(!is.null(input$apply_correction) && input$apply_correction) {
          r_code_text <- paste0(r_code_text, "# Multiple Comparisons Correction\n")
          
          if(!is.null(input$robust) && input$robust) {
            r_code_text <- paste0(r_code_text, "robust_se <- coeftest(model, vcov = vcovHC(model, type = 'HC1'))\n")
            r_code_text <- paste0(r_code_text, "p_values <- robust_se[-1, 4]\n")
          } else {
            r_code_text <- paste0(r_code_text, "coef_df <- tidy(model, conf.int = TRUE)\n")
            r_code_text <- paste0(r_code_text, "p_values <- coef_df$p.value[-1]\n")
          }
          
          if(!is.null(input$correction_method)) {
            r_code_text <- paste0(r_code_text, "corrected_p <- p.adjust(p_values, method = '", input$correction_method, "')\n")
            if(!is.null(input$alpha_level)) {
              r_code_text <- paste0(r_code_text, "# Using alpha level: ", input$alpha_level, "\n")
            }
            r_code_text <- paste0(r_code_text, "print(corrected_p)\n\n")
          } else {
            r_code_text <- paste0(r_code_text, "corrected_p <- p.adjust(p_values, method = 'bonferroni')\n")
            r_code_text <- paste0(r_code_text, "print(corrected_p)\n\n")
          }
        }
        
        if(!is.null(input$enable_comparison) && input$enable_comparison) {
          r_code_text <- paste0(r_code_text, "# Model Comparison Setup\n")
          r_code_text <- paste0(r_code_text, "# Store multiple models for comparison\n")
          r_code_text <- paste0(r_code_text, "models_list <- list(model1 = model)\n")
          r_code_text <- paste0(r_code_text, "# Add more models: models_list$model2 <- lm(formula2, data)\n")
          r_code_text <- paste0(r_code_text, "# compare_performance(models_list)\n\n")
        }
        
        r_code_text <- paste0(r_code_text, "par(mfrow = c(2, 2))\n")
        r_code_text <- paste0(r_code_text, "plot(model)\n")
        r_code_text <- paste0(r_code_text, "par(mfrow = c(1, 1))\n\n")
        r_code_text <- paste0(r_code_text, "cat('AIC:', AIC(model), '\\n')\n")
        r_code_text <- paste0(r_code_text, "cat('BIC:', BIC(model), '\\n')\n")
        r_code_text <- paste0(r_code_text, "cat('Adjusted R-squared:', summary(model)$adj.r.squared, '\\n')")
      } else {
        r_code_text <- "# Optimized Logistic Regression Analysis\n"
        r_code_text <- paste0(r_code_text, "library(broom)\n")
        r_code_text <- paste0(r_code_text, "library(ggplot2)\n")
        
        if(!is.null(input$enable_comparison) && input$enable_comparison) {
          r_code_text <- paste0(r_code_text, "library(performance)\n")
        }
        
        r_code_text <- paste0(r_code_text, "\nmodel <- glm(", formula_str, ", data = data, family = binomial(),\n")
        r_code_text <- paste0(r_code_text, "              control = glm.control(epsilon = 1e-6, maxit = 20, trace = FALSE))\n")
        r_code_text <- paste0(r_code_text, "summary(model)\n\n")
        r_code_text <- paste0(r_code_text, "# Fast odds ratios calculation\n")
        r_code_text <- paste0(r_code_text, "coef_est <- coef(model)\n")
        r_code_text <- paste0(r_code_text, "coef_se <- sqrt(diag(vcov(model)))\n")
        r_code_text <- paste0(r_code_text, "z_critical <- qnorm(0.975)\n")
        r_code_text <- paste0(r_code_text, "odds_ratios <- data.frame(\n")
        r_code_text <- paste0(r_code_text, "  estimate = exp(coef_est),\n")
        r_code_text <- paste0(r_code_text, "  conf.low = exp(coef_est - z_critical * coef_se),\n")
        r_code_text <- paste0(r_code_text, "  conf.high = exp(coef_est + z_critical * coef_se)\n")
        r_code_text <- paste0(r_code_text, ")\n")
        r_code_text <- paste0(r_code_text, "print(odds_ratios)\n\n")
        
        if(!is.null(input$apply_correction) && input$apply_correction) {
          r_code_text <- paste0(r_code_text, "# Multiple Comparisons Correction\n")
          r_code_text <- paste0(r_code_text, "coef_df <- tidy(model, conf.int = TRUE)\n")
          r_code_text <- paste0(r_code_text, "p_values <- coef_df$p.value[-1]\n")
          if(!is.null(input$correction_method)) {
            r_code_text <- paste0(r_code_text, "corrected_p <- p.adjust(p_values, method = '", input$correction_method, "')\n")
            if(!is.null(input$alpha_level)) {
              r_code_text <- paste0(r_code_text, "# Using alpha level: ", input$alpha_level, "\n")
            }
            r_code_text <- paste0(r_code_text, "print(corrected_p)\n\n")
          } else {
            r_code_text <- paste0(r_code_text, "corrected_p <- p.adjust(p_values, method = 'bonferroni')\n")
            r_code_text <- paste0(r_code_text, "print(corrected_p)\n\n")
          }
        }
        
        if(!is.null(input$enable_comparison) && input$enable_comparison) {
          r_code_text <- paste0(r_code_text, "# Model Comparison Setup\n")
          r_code_text <- paste0(r_code_text, "# Store multiple models for comparison\n")
          r_code_text <- paste0(r_code_text, "models_list <- list(model1 = model)\n")
          r_code_text <- paste0(r_code_text, "# Add more models: models_list$model2 <- glm(formula2, data, family = binomial())\n")
          r_code_text <- paste0(r_code_text, "# compare_performance(models_list)\n\n")
        }
        
        r_code_text <- paste0(r_code_text, "# Model fit statistics\n")
        r_code_text <- paste0(r_code_text, "mcfadden_r2 <- 1 - (model$deviance / model$null.deviance)\n")
        r_code_text <- paste0(r_code_text, "cat('McFadden R-squared:', mcfadden_r2, '\\n')\n")
        r_code_text <- paste0(r_code_text, "cat('AIC:', AIC(model), '\\n')\n")
        r_code_text <- paste0(r_code_text, "cat('BIC:', BIC(model), '\\n')\n\n")
        r_code_text <- paste0(r_code_text, "predicted_probs <- predict(model, type = 'response')\n")
        r_code_text <- paste0(r_code_text, "summary(predicted_probs)")
      }
      
      return(r_code_text)
    })
  })
}

generate_comparison_code <- function(models, model_names, type) {
  if(length(models) < 2) {
    return(NULL)
  }
  
  r_code_text <- "# Model Comparison Analysis\n"
  r_code_text <- paste0(r_code_text, "library(performance)\n")
  r_code_text <- paste0(r_code_text, "library(broom)\n\n")
  
  for(i in 1:length(models)) {
    formula_str <- deparse(formula(models[[i]]))
    if(type == "ols") {
      r_code_text <- paste0(r_code_text, model_names[i], " <- lm(", formula_str, ", data = data)\n")
    } else {
      r_code_text <- paste0(r_code_text, model_names[i], " <- glm(", formula_str, ", data = data, family = binomial())\n")
    }
  }
  
  r_code_text <- paste0(r_code_text, "\n# Compare model performance\n")
  r_code_text <- paste0(r_code_text, "models_list <- list(", paste(paste0(model_names, " = ", model_names), collapse = ", "), ")\n")
  r_code_text <- paste0(r_code_text, "comparison_result <- compare_performance(models_list)\n")
  r_code_text <- paste0(r_code_text, "print(comparison_result)\n\n")
  
  r_code_text <- paste0(r_code_text, "# ANOVA comparison (for nested models)\n")
  if(type == "ols") {
    r_code_text <- paste0(r_code_text, "anova_result <- anova(", paste(model_names, collapse = ", "), ")\n")
  } else {
    r_code_text <- paste0(r_code_text, "# Likelihood Ratio Test for logistic models\n")
    r_code_text <- paste0(r_code_text, "lrt_result <- anova(", paste(model_names, collapse = ", "), ", test = 'LRT')\n")
  }
  r_code_text <- paste0(r_code_text, "print(", if(type == "ols") "anova_result" else "lrt_result", ")\n\n")
  
  if(type == "ols") {
    r_code_text <- paste0(r_code_text, "# Extract key metrics\n")
    r_code_text <- paste0(r_code_text, "adj_r_squared <- sapply(models_list, function(m) summary(m)$adj.r.squared)\n")
    r_code_text <- paste0(r_code_text, "aic_values <- sapply(models_list, AIC)\n")
    r_code_text <- paste0(r_code_text, "bic_values <- sapply(models_list, BIC)\n")
    r_code_text <- paste0(r_code_text, "comparison_df <- data.frame(\n")
    r_code_text <- paste0(r_code_text, "  Model = names(models_list),\n")
    r_code_text <- paste0(r_code_text, "  Adj_R_squared = adj_r_squared,\n")
    r_code_text <- paste0(r_code_text, "  AIC = aic_values,\n")
    r_code_text <- paste0(r_code_text, "  BIC = bic_values\n")
    r_code_text <- paste0(r_code_text, ")\n")
    r_code_text <- paste0(r_code_text, "print(comparison_df)\n")
  } else {
    r_code_text <- paste0(r_code_text, "# Extract key metrics\n")
    r_code_text <- paste0(r_code_text, "mcfadden_r2 <- sapply(models_list, function(m) 1 - (m$deviance / m$null.deviance))\n")
    r_code_text <- paste0(r_code_text, "aic_values <- sapply(models_list, AIC)\n")
    r_code_text <- paste0(r_code_text, "bic_values <- sapply(models_list, BIC)\n")
    r_code_text <- paste0(r_code_text, "comparison_df <- data.frame(\n")
    r_code_text <- paste0(r_code_text, "  Model = names(models_list),\n")
    r_code_text <- paste0(r_code_text, "  McFadden_R2 = mcfadden_r2,\n")
    r_code_text <- paste0(r_code_text, "  AIC = aic_values,\n")
    r_code_text <- paste0(r_code_text, "  BIC = bic_values\n")
    r_code_text <- paste0(r_code_text, ")\n")
    r_code_text <- paste0(r_code_text, "print(comparison_df)\n")
  }
  
  return(r_code_text)
}

apply_multiple_comparisons <- function(model, method, alpha) {
  tryCatch({
    coef_summary <- summary(model)$coefficients
    
    if(nrow(coef_summary) <= 1) {
      return(NULL)
    }
    
    if(ncol(coef_summary) < 4 || !("Pr(>|z|)" %in% colnames(coef_summary))) {
      return(NULL)
    }
    p_values <- coef_summary[-1, "Pr(>|z|)", drop = FALSE]
    
    if(length(p_values) == 0 || all(is.na(p_values))) {
      return(NULL)
    }
    
    corrected_p <- p.adjust(as.vector(p_values), method = method)
    
    coef_df <- tidy(model, conf.int = TRUE)
    coef_df <- coef_df[coef_df$term != "(Intercept)", ]
    
    if(nrow(coef_df) == 0) {
      return(NULL)
    }
    
    if(method == "bonferroni") {
      corrected_alpha <- alpha / length(p_values)
    } else {
      corrected_alpha <- alpha
    }
    
    z_critical <- qnorm(1 - corrected_alpha/2)
    
    coef_df$conf.low_corrected <- coef_df$estimate - z_critical * coef_df$std.error
    coef_df$conf.high_corrected <- coef_df$estimate + z_critical * coef_df$std.error
    coef_df$p.value_corrected <- corrected_p
    
    return(coef_df)
  }, error = function(e) {
    return(NULL)
  })
}

# ===================================================================
# END OF REGRESSION MODULE
# ===================================================================