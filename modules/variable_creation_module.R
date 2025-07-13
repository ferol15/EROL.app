# ===================================================================
# COMPLETE WORKING VARIABLE CREATION MODULE
# ===================================================================

# Required libraries
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard") 
if (!require(DT)) install.packages("DT")
if (!require(dplyr)) install.packages("dplyr")

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# ===================================================================
# UI FUNCTION
# ===================================================================

variable_creation_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "variables",
          fluidRow(
            box(title = "Variable Operations", status = "primary", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = "output.fileUploaded",
                                 h4("Choose Operation Type"),
                                 selectInput(ns("operation_type"), "Operation Type:",
                                             choices = list(
                                               "Simple Recoding" = "recode",
                                               "Indexing/Summing/Averaging" = "compute",
                                               "Variable Renaming" = "rename",
                                               "Missing Value Handling" = "missing",
                                               "Categorical → Continuous" = "cat_to_cont",
                                               "Continuous → Categorical" = "cont_to_cat"
                                             )),
                                 
                                 # Simple Recoding Section
                                 conditionalPanel(condition = paste0("input['", ns("operation_type"), "'] == 'recode'"),
                                                  h5("Variable Recoding"),
                                                  selectInput(ns("source_var"), "Select Source Variable:", choices = NULL),
                                                  actionButton(ns("reset_source"), "Reset Selection", class = "btn-sm btn-secondary"),
                                                  textInput(ns("new_var_name"), "New Variable Name:", placeholder = "e.g., age_category"),
                                                  helpText("Syntax: 1=0, <=0=NA, >=999=NA or 1->0, <=0->NA"),
                                                  textAreaInput(ns("numeric_rules"), "Recoding Rules:", placeholder = "1=1\n2=0\n<=0=NA\n>=999=NA", rows = 4),
                                                  verbatimTextOutput(ns("debug_parsing"))
                                 ),
                                 
                                 # Computing Section
                                 conditionalPanel(condition = paste0("input['", ns("operation_type"), "'] == 'compute'"),
                                                  h5("Create Computed Variables"),
                                                  textInput(ns("computed_var_name"), "New Variable Name:", placeholder = "e.g., total_score"),
                                                  selectInput(ns("computation_type"), "Computation Type:",
                                                              choices = list(
                                                                "Sum of variables" = "sum",
                                                                "Average of variables" = "mean", 
                                                                "Index (standardized sum)" = "index",
                                                                "Weighted sum" = "weighted_sum",
                                                                "Count non-missing" = "count",
                                                                "Custom formula" = "custom"
                                                              )),
                                                  selectInput(ns("compute_variables"), "Select Variables:", choices = NULL, multiple = TRUE),
                                                  actionButton(ns("reset_compute_vars"), "Reset Selection", class = "btn-sm btn-secondary"),
                                                  conditionalPanel(condition = paste0("input['", ns("computation_type"), "'] == 'weighted_sum'"),
                                                                   textInput(ns("weights"), "Weights (comma-separated):", placeholder = "1, 2, 1.5, 0.5")
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("computation_type"), "'] == 'index'"),
                                                                   fluidRow(
                                                                     column(6, numericInput(ns("min_value"), "Min value per variable:", value = 1)),
                                                                     column(6, numericInput(ns("max_value"), "Max value per variable:", value = 5))
                                                                   )
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("computation_type"), "'] == 'custom'"),
                                                                   textAreaInput(ns("custom_formula"), "Custom Formula:", placeholder = "(var1 + var2) / 2", rows = 2)
                                                  ),
                                                  checkboxInput(ns("require_all"), "Require all variables non-missing", value = FALSE)
                                 ),
                                 
                                 # Categorical to Continuous Section
                                 conditionalPanel(condition = paste0("input['", ns("operation_type"), "'] == 'cat_to_cont'"),
                                                  h5("Convert Categorical → Continuous"),
                                                  selectInput(ns("cat_source_var"), "Select Categorical Variable:", choices = NULL),
                                                  actionButton(ns("reset_cat_source"), "Reset Selection", class = "btn-sm btn-secondary"),
                                                  textInput(ns("cat_new_name"), "New Variable Name:", placeholder = "e.g., education_numeric"),
                                                  selectInput(ns("cat_conversion_type"), "Conversion Method:",
                                                              choices = list(
                                                                "Manual Assignment" = "manual",
                                                                "Ordinal Ranking (1, 2, 3...)" = "ordinal",
                                                                "Dummy Variables (0/1)" = "dummy",
                                                                "Effect Coding (-1/0/1)" = "effect"
                                                              )),
                                                  conditionalPanel(condition = paste0("input['", ns("cat_conversion_type"), "'] == 'manual'"),
                                                                   helpText("💡 Assign numeric values to each category"),
                                                                   uiOutput(ns("manual_mapping_ui")),
                                                                   div(style = "margin-top: 10px;",
                                                                       actionButton(ns("reset_mapping"), "Sequential (1,2,3...)", class = "btn-sm btn-outline-secondary"),
                                                                       actionButton(ns("set_binary"), "Binary (0,1)", class = "btn-sm btn-outline-info")
                                                                   )
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("cat_conversion_type"), "'] == 'dummy'"),
                                                                   helpText("📋 Creates 0/1 variables for each category"),
                                                                   uiOutput(ns("base_category_ui")),
                                                                   checkboxInput(ns("include_all_dummies"), "Include all categories (no reference)", value = FALSE)
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("cat_conversion_type"), "'] == 'effect'"),
                                                                   helpText("📋 Reference = -1, Target = 1, Others = 0 (for regression analysis)"),
                                                                   uiOutput(ns("effect_base_category_ui"))
                                                  )
                                 ),
                                 
                                 # Continuous to Categorical Section
                                 conditionalPanel(condition = paste0("input['", ns("operation_type"), "'] == 'cont_to_cat'"),
                                                  h5("Convert Continuous → Categorical"),
                                                  selectInput(ns("cont_source_var"), "Select Continuous Variable:", choices = NULL),
                                                  actionButton(ns("reset_cont_source"), "Reset Selection", class = "btn-sm btn-secondary"),
                                                  textInput(ns("cont_new_name"), "New Variable Name:", placeholder = "e.g., age_groups"),
                                                  selectInput(ns("cont_conversion_type"), "Conversion Method:",
                                                              choices = list(
                                                                "Custom Cutpoints" = "custom_cuts",
                                                                "Equal Width Intervals" = "equal_width",
                                                                "Quantile-based" = "quantiles",
                                                                "Standard Deviations" = "sd_cuts"
                                                              )),
                                                  conditionalPanel(condition = paste0("input['", ns("cont_conversion_type"), "'] == 'custom_cuts'"),
                                                                   textInput(ns("custom_cutpoints"), "Cutpoints:", placeholder = "18, 35, 55, 75"),
                                                                   textInput(ns("custom_labels"), "Labels (optional):", placeholder = "Young, Middle, Older, Senior")
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("cont_conversion_type"), "'] == 'equal_width'"),
                                                                   numericInput(ns("n_bins"), "Number of bins:", value = 3, min = 2, max = 10)
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("cont_conversion_type"), "'] == 'quantiles'"),
                                                                   numericInput(ns("n_quantiles"), "Number of quantiles:", value = 4, min = 2, max = 10)
                                                  ),
                                                  conditionalPanel(condition = paste0("input['", ns("cont_conversion_type"), "'] == 'sd_cuts'"),
                                                                   selectInput(ns("sd_method"), "Method:",
                                                                               choices = list(
                                                                                 "±1 SD (3 groups)" = "1sd",
                                                                                 "±0.5, ±1 SD (5 groups)" = "half_sd",
                                                                                 "±1, ±2 SD (5 groups)" = "2sd"
                                                                               ))
                                                  )
                                 ),
                                 
                                 # Renaming Section
                                 conditionalPanel(condition = paste0("input['", ns("operation_type"), "'] == 'rename'"),
                                                  h5("Rename Variables"),
                                                  selectInput(ns("rename_source"), "Variable to Rename:", choices = NULL),
                                                  actionButton(ns("reset_rename"), "Reset Selection", class = "btn-sm btn-secondary"),
                                                  textInput(ns("rename_new_name"), "New Name:", placeholder = "Enter new variable name")
                                 ),
                                 
                                 # Missing Value Section
                                 conditionalPanel(condition = paste0("input['", ns("operation_type"), "'] == 'missing'"),
                                                  h5("Missing Value Treatment"),
                                                  selectInput(ns("missing_source_var"), "Select Variable:", choices = NULL),
                                                  actionButton(ns("reset_missing"), "Reset Selection", class = "btn-sm btn-secondary"),
                                                  textInput(ns("missing_new_name"), "New Variable Name:", placeholder = "e.g., clean_variable"),
                                                  textInput(ns("missing_values"), "Missing Values (comma-separated):", placeholder = "-99, -98, 999"),
                                                  selectInput(ns("missing_action"), "Action:",
                                                              choices = list("Set to NA" = "na", "Set to specific value" = "value")),
                                                  conditionalPanel(condition = paste0("input['", ns("missing_action"), "'] == 'value'"),
                                                                   numericInput(ns("replacement_value"), "Replacement Value:", value = 0)
                                                  )
                                 ),
                                 
                                 hr(),
                                 div(style = "text-align: center;",
                                     actionButton(ns("create_variable"), "Create Variable", class = "btn-primary btn-lg"),
                                     br(), br(),
                                     downloadButton(ns("download_data"), "Download Dataset", class = "btn-success")
                                 )
                )
            ),
            
            box(title = "Variable Information", status = "info", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = "output.fileUploaded",
                                 tabsetPanel(id = ns("info_tabs"),
                                             tabPanel("R Code", 
                                                      h5("Generated R Code:"),
                                                      div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                                                          verbatimTextOutput(ns("current_r_code"))
                                                      )
                                             ),
                                             tabPanel("Variable Summary", 
                                                      h5("Variable Details:"),
                                                      verbatimTextOutput(ns("variable_summary"))
                                             ),
                                             tabPanel("Created Variables",
                                                      h5("Created Variables Log:"),
                                                      DT::dataTableOutput(ns("created_variables_table"))
                                             )
                                 )
                )
            )
          ),
          
          fluidRow(
            box(title = "Data Preview", status = "info", solidHeader = TRUE, width = 12,
                helpText("📊 Green = newly created variables, Yellow = source variables"),
                DT::dataTableOutput(ns("variable_preview"))
            )
          )
  )
}

# ===================================================================
# VARIABLE CREATION MODULE
# ===================================================================

# Enhanced rule parsing function
parse_recode_rule <- function(rule) {
  rule <- trimws(rule)
  
  if(grepl("->", rule)) {
    parts <- strsplit(rule, "->")[[1]]
    if(length(parts) != 2) return(list(error = paste("Invalid arrow syntax:", rule)))
    old_val <- trimws(parts[1])
    new_val <- trimws(parts[2])
  } else if(grepl("=", rule)) {
    equals_positions <- gregexpr("=", rule)[[1]]
    if(length(equals_positions) == 0) return(list(error = paste("No equals sign found:", rule)))
    last_equals <- equals_positions[length(equals_positions)]
    old_val <- trimws(substr(rule, 1, last_equals - 1))
    new_val <- trimws(substr(rule, last_equals + 1, nchar(rule)))
  } else {
    return(list(error = paste("Cannot parse rule:", rule)))
  }
  
  if(new_val == "" || toupper(new_val) == "NA") new_val <- "NA"
  
  if(grepl("^(<=|>=|<|>|==|!=)", old_val)) {
    if(grepl("^<=", old_val)) {
      value_part <- trimws(gsub("^<=", "", old_val))
      operator <- "<="
    } else if(grepl("^>=", old_val)) {
      value_part <- trimws(gsub("^>=", "", old_val))
      operator <- ">="
    } else if(grepl("^<", old_val)) {
      value_part <- trimws(gsub("^<", "", old_val))
      operator <- "<"
    } else if(grepl("^>", old_val)) {
      value_part <- trimws(gsub("^>", "", old_val))
      operator <- ">"
    } else if(grepl("^==", old_val)) {
      value_part <- trimws(gsub("^==", "", old_val))
      operator <- "=="
    } else if(grepl("^!=", old_val)) {
      value_part <- trimws(gsub("^!=", "", old_val))
      operator <- "!="
    }
    
    if(value_part == "") return(list(error = paste("Missing numeric value:", old_val)))
    numeric_value <- suppressWarnings(as.numeric(value_part))
    if(is.na(numeric_value)) return(list(error = paste("Invalid numeric value:", old_val)))
    
    return(list(type = "condition", operator = operator, value = value_part, new_value = new_val))
  } else {
    return(list(type = "simple", old_value = old_val, new_value = new_val))
  }
}

# Helper function for condition application
apply_condition <- function(data, condition_str) {
  if(grepl("^<=", condition_str)) {
    value <- as.numeric(gsub("^<=", "", condition_str))
    return(data <= value & !is.na(data))
  } else if(grepl("^>=", condition_str)) {
    value <- as.numeric(gsub("^>=", "", condition_str))
    return(data >= value & !is.na(data))
  } else if(grepl("^<", condition_str)) {
    value <- as.numeric(gsub("^<", "", condition_str))
    return(data < value & !is.na(data))
  } else if(grepl("^>", condition_str)) {
    value <- as.numeric(gsub("^>", "", condition_str))
    return(data > value & !is.na(data))
  } else if(grepl("^==", condition_str)) {
    value <- as.numeric(gsub("^==", "", condition_str))
    return(data == value & !is.na(data))
  } else if(grepl("^!=", condition_str)) {
    value <- as.numeric(gsub("^!=", "", condition_str))
    return(data != value & !is.na(data))
  }
  return(rep(FALSE, length(data)))
}

# Helper function to get variable choices
get_variable_choices <- function(data, type = "all") {
  if(is.null(data) || ncol(data) == 0) return(list())
  
  if(type == "numeric") {
    numeric_vars <- sapply(data, is.numeric, USE.NAMES = FALSE)
    vars <- names(data)[numeric_vars]
    return(setNames(vars, vars))
  } else if(type == "categorical") {
    categorical_vars <- sapply(data, function(x) {
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x[!is.na(x)])) <= 10)
    }, USE.NAMES = FALSE)
    vars <- names(data)[categorical_vars]
    return(setNames(vars, vars))
  } else {
    vars <- names(data)
    return(setNames(vars, vars))
  }
}

# FIXED: Helper function to add R code to shared data with proper logging
add_r_code <- function(shared_data, code, description) {
  if(is.null(shared_data$r_code_log)) {
    shared_data$r_code_log <- data.frame(
      Step = character(0),
      Description = character(0), 
      Code = character(0),
      Timestamp = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    )
  }
  
  new_row <- data.frame(
    Step = paste("Step", nrow(shared_data$r_code_log) + 1),
    Description = description,
    Code = code,
    Timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  shared_data$r_code_log <- rbind(shared_data$r_code_log, new_row)
}

# FIXED: Generate complete R code based on operation type and inputs
generate_complete_r_code <- function(input, shared_data) {
  if(is.null(shared_data$working_data)) return("# No data loaded")
  
  switch(input$operation_type,
         "recode" = {
           if(is.null(input$source_var) || is.null(input$new_var_name) || 
              input$source_var == "" || input$new_var_name == "") {
             return("# Select source variable and enter new variable name")
           }
           
           r_code <- paste0("# Recode variable: ", input$new_var_name, "\n")
           r_code <- paste0(r_code, "data$", input$new_var_name, " <- data$", input$source_var, "\n")
           
           if(!is.null(input$numeric_rules) && input$numeric_rules != "") {
             r_code <- paste0(r_code, "\n# Apply recoding rules:\n")
             rules <- strsplit(input$numeric_rules, "\n")[[1]]
             rules <- trimws(rules[rules != ""])
             
             for(rule in rules) {
               parsed <- parse_recode_rule(rule)
               if(is.null(parsed$error)) {
                 if(parsed$type == "condition") {
                   if(parsed$new_value == "NA") {
                     r_code <- paste0(r_code, "data$", input$new_var_name, "[data$", input$source_var, " ", parsed$operator, " ", parsed$value, " & !is.na(data$", input$source_var, ")] <- NA\n")
                   } else {
                     r_code <- paste0(r_code, "data$", input$new_var_name, "[data$", input$source_var, " ", parsed$operator, " ", parsed$value, " & !is.na(data$", input$source_var, ")] <- ", parsed$new_value, "\n")
                   }
                 } else {
                   if(parsed$new_value == "NA") {
                     r_code <- paste0(r_code, "data$", input$new_var_name, "[data$", input$source_var, " == ", parsed$old_value, " & !is.na(data$", input$source_var, ")] <- NA\n")
                   } else {
                     r_code <- paste0(r_code, "data$", input$new_var_name, "[data$", input$source_var, " == ", parsed$old_value, "] <- ", parsed$new_value, "\n")
                   }
                 }
               }
             }
           } else {
             r_code <- paste0(r_code, "# Add recoding rules above")
           }
           
           return(r_code)
         },
         
         "compute" = {
           if(is.null(input$computed_var_name) || is.null(input$compute_variables) ||
              input$computed_var_name == "" || length(input$compute_variables) == 0) {
             return("# Select variables and enter variable name")
           }
           
           vars_str <- paste0("c('", paste(input$compute_variables, collapse = "', '"), "')")
           r_code <- paste0("# Compute variable: ", input$computed_var_name, "\n")
           
           switch(input$computation_type,
                  "sum" = {
                    na_rm <- if(input$require_all) "FALSE" else "TRUE"
                    r_code <- paste0(r_code, "data$", input$computed_var_name, " <- rowSums(data[, ", vars_str, "], na.rm = ", na_rm, ")")
                  },
                  "mean" = {
                    na_rm <- if(input$require_all) "FALSE" else "TRUE"
                    r_code <- paste0(r_code, "data$", input$computed_var_name, " <- rowMeans(data[, ", vars_str, "], na.rm = ", na_rm, ")")
                  },
                  "index" = {
                    if(!is.null(input$min_value) && !is.null(input$max_value)) {
                      r_code <- paste0(r_code, "# Standardized index (0-1 scale)\n")
                      r_code <- paste0(r_code, "sum_scores <- rowSums(data[, ", vars_str, "], na.rm = ", !input$require_all, ")\n")
                      r_code <- paste0(r_code, "min_possible <- ", input$min_value, " * ", length(input$compute_variables), "\n")
                      r_code <- paste0(r_code, "max_possible <- ", input$max_value, " * ", length(input$compute_variables), "\n")
                      r_code <- paste0(r_code, "data$", input$computed_var_name, " <- (sum_scores - min_possible) / (max_possible - min_possible)")
                    } else {
                      r_code <- paste0(r_code, "# Set min and max values for index calculation")
                    }
                  },
                  "weighted_sum" = {
                    if(!is.null(input$weights) && input$weights != "") {
                      r_code <- paste0(r_code, "weights <- c(", input$weights, ")\n")
                      r_code <- paste0(r_code, "data$", input$computed_var_name, " <- rowSums(data[, ", vars_str, "] * weights, na.rm = ", !input$require_all, ")")
                    } else {
                      r_code <- paste0(r_code, "# Enter weights for weighted sum")
                    }
                  },
                  "count" = {
                    r_code <- paste0(r_code, "data$", input$computed_var_name, " <- rowSums(!is.na(data[, ", vars_str, "]))")
                  },
                  "custom" = {
                    if(!is.null(input$custom_formula) && input$custom_formula != "") {
                      r_code <- paste0(r_code, "data$", input$computed_var_name, " <- with(data, ", input$custom_formula, ")")
                    } else {
                      r_code <- paste0(r_code, "# Enter custom formula")
                    }
                  }
           )
           
           return(r_code)
         },
         
         "cat_to_cont" = {
           if(is.null(input$cat_source_var) || is.null(input$cat_new_name) ||
              input$cat_source_var == "" || input$cat_new_name == "") {
             return("# Select categorical variable and enter new name")
           }
           
           r_code <- paste0("# Convert categorical to continuous: ", input$cat_new_name, "\n")
           
           switch(input$cat_conversion_type,
                  "manual" = {
                    r_code <- paste0(r_code, "# Manual value assignment\n")
                    r_code <- paste0(r_code, "data$", input$cat_new_name, " <- data$", input$cat_source_var, "\n")
                    
                    if(!is.null(input$cat_source_var) && input$cat_source_var != "" && 
                       input$cat_source_var %in% names(shared_data$working_data)) {
                      var_data <- shared_data$working_data[[input$cat_source_var]]
                      unique_values <- sort(unique(var_data[!is.na(var_data)]))
                      
                      for(i in seq_along(unique_values)) {
                        val <- unique_values[i]
                        map_input <- input[[paste0("map_", i)]]
                        if(!is.null(map_input)) {
                          r_code <- paste0(r_code, "data$", input$cat_new_name, "[data$", input$cat_source_var, " == '", val, "'] <- ", map_input, "\n")
                        }
                      }
                    } else {
                      r_code <- paste0(r_code, "# Manual recoding rules will be added here")
                    }
                  },
                  "ordinal" = {
                    r_code <- paste0(r_code, "data$", input$cat_new_name, " <- as.numeric(as.factor(data$", input$cat_source_var, "))")
                  },
                  "dummy" = {
                    if(input$include_all_dummies) {
                      r_code <- paste0(r_code, "# Create dummy variables (all categories)\n")
                      
                      if(!is.null(input$cat_source_var) && input$cat_source_var != "" && 
                         input$cat_source_var %in% names(shared_data$working_data)) {
                        var_data <- shared_data$working_data[[input$cat_source_var]]
                        unique_values <- sort(unique(var_data[!is.na(var_data)]))
                        
                        for(val in unique_values) {
                          dummy_name <- paste0(input$cat_new_name, "_", make.names(val))
                          r_code <- paste0(r_code, "data$", dummy_name, " <- as.numeric(data$", input$cat_source_var, " == '", val, "')\n")
                        }
                      } else {
                        r_code <- paste0(r_code, "# Dummy variable code will be generated here")
                      }
                    } else {
                      if(!is.null(input$base_category) && input$base_category != "") {
                        r_code <- paste0(r_code, "# Create dummy variables (reference: ", input$base_category, ")\n")
                        
                        if(!is.null(input$cat_source_var) && input$cat_source_var != "" && 
                           input$cat_source_var %in% names(shared_data$working_data)) {
                          var_data <- shared_data$working_data[[input$cat_source_var]]
                          unique_values <- sort(unique(var_data[!is.na(var_data)]))
                          non_base_cats <- unique_values[unique_values != input$base_category]
                          
                          for(val in non_base_cats) {
                            dummy_name <- paste0(input$cat_new_name, "_", make.names(val))
                            r_code <- paste0(r_code, "data$", dummy_name, " <- as.numeric(data$", input$cat_source_var, " == '", val, "')\n")
                          }
                          r_code <- paste0(r_code, "# Reference category '", input$base_category, "' coded as 0 for all dummies")
                        }
                      } else {
                        r_code <- paste0(r_code, "# Select base category to generate dummy variable code")
                      }
                    }
                  },
                  "effect" = {
                    if(!is.null(input$effect_base_category) && input$effect_base_category != "") {
                      r_code <- paste0(r_code, "# Create effect coding variables (reference: ", input$effect_base_category, ")\n")
                      
                      if(!is.null(input$cat_source_var) && input$cat_source_var != "" && 
                         input$cat_source_var %in% names(shared_data$working_data)) {
                        var_data <- shared_data$working_data[[input$cat_source_var]]
                        unique_values <- sort(unique(var_data[!is.na(var_data)]))
                        non_base_cats <- unique_values[unique_values != input$effect_base_category]
                        
                        for(val in non_base_cats) {
                          effect_name <- paste0(input$cat_new_name, "_", make.names(val))
                          r_code <- paste0(r_code, "data$", effect_name, " <- ifelse(data$", input$cat_source_var, " == '", val, "', 1, ifelse(data$", input$cat_source_var, " == '", input$effect_base_category, "', -1, 0))\n")
                        }
                        r_code <- paste0(r_code, "# Reference category '", input$effect_base_category, "' coded as -1")
                      }
                    } else {
                      r_code <- paste0(r_code, "# Select base category to generate effect coding")
                    }
                  }
           )
           
           return(r_code)
         },
         
         "cont_to_cat" = {
           if(is.null(input$cont_source_var) || is.null(input$cont_new_name) ||
              input$cont_source_var == "" || input$cont_new_name == "") {
             return("# Select continuous variable and enter new name")
           }
           
           r_code <- paste0("# Convert continuous to categorical: ", input$cont_new_name, "\n")
           
           switch(input$cont_conversion_type,
                  "custom_cuts" = {
                    if(!is.null(input$custom_cutpoints) && input$custom_cutpoints != "") {
                      r_code <- paste0(r_code, "cutpoints <- c(-Inf, ", input$custom_cutpoints, ", Inf)\n")
                      if(!is.null(input$custom_labels) && input$custom_labels != "") {
                        labels <- trimws(strsplit(input$custom_labels, ",")[[1]])
                        labels_str <- paste0("c('", paste(labels, collapse = "', '"), "')")
                        r_code <- paste0(r_code, "data$", input$cont_new_name, " <- cut(data$", input$cont_source_var, ", breaks = cutpoints, labels = ", labels_str, ", include.lowest = TRUE)")
                      } else {
                        r_code <- paste0(r_code, "data$", input$cont_new_name, " <- cut(data$", input$cont_source_var, ", breaks = cutpoints, include.lowest = TRUE)")
                      }
                    } else {
                      r_code <- paste0(r_code, "# Enter cutpoints to generate code")
                    }
                  },
                  "equal_width" = {
                    if(!is.null(input$n_bins)) {
                      r_code <- paste0(r_code, "data$", input$cont_new_name, " <- cut(data$", input$cont_source_var, ", breaks = ", input$n_bins, ", include.lowest = TRUE)")
                    }
                  },
                  "quantiles" = {
                    if(!is.null(input$n_quantiles)) {
                      r_code <- paste0(r_code, "quantile_breaks <- quantile(data$", input$cont_source_var, ", probs = seq(0, 1, length.out = ", input$n_quantiles + 1, "), na.rm = TRUE)\n")
                      r_code <- paste0(r_code, "data$", input$cont_new_name, " <- cut(data$", input$cont_source_var, ", breaks = quantile_breaks, include.lowest = TRUE)")
                    }
                  },
                  "sd_cuts" = {
                    if(!is.null(input$sd_method)) {
                      r_code <- paste0(r_code, "mean_val <- mean(data$", input$cont_source_var, ", na.rm = TRUE)\n")
                      r_code <- paste0(r_code, "sd_val <- sd(data$", input$cont_source_var, ", na.rm = TRUE)\n")
                      
                      if(input$sd_method == "1sd") {
                        r_code <- paste0(r_code, "breaks <- c(-Inf, mean_val - sd_val, mean_val + sd_val, Inf)\n")
                        r_code <- paste0(r_code, "labels <- c('Low', 'Medium', 'High')\n")
                      } else if(input$sd_method == "half_sd") {
                        r_code <- paste0(r_code, "breaks <- c(-Inf, mean_val - sd_val, mean_val - 0.5*sd_val, mean_val + 0.5*sd_val, mean_val + sd_val, Inf)\n")
                        r_code <- paste0(r_code, "labels <- c('Very Low', 'Low', 'Medium', 'High', 'Very High')\n")
                      } else if(input$sd_method == "2sd") {
                        r_code <- paste0(r_code, "breaks <- c(-Inf, mean_val - 2*sd_val, mean_val - sd_val, mean_val + sd_val, mean_val + 2*sd_val, Inf)\n")
                        r_code <- paste0(r_code, "labels <- c('Very Low', 'Low', 'Medium', 'High', 'Very High')\n")
                      }
                      
                      r_code <- paste0(r_code, "data$", input$cont_new_name, " <- cut(data$", input$cont_source_var, ", breaks = breaks, labels = labels, include.lowest = TRUE)")
                    }
                  }
           )
           
           return(r_code)
         },
         
         "rename" = {
           if(is.null(input$rename_source) || is.null(input$rename_new_name) ||
              input$rename_source == "" || input$rename_new_name == "") {
             return("# Select source variable and enter new name")
           }
           
           paste0("# Rename variable: ", input$rename_source, " -> ", input$rename_new_name, "\n",
                  "data$", input$rename_new_name, " <- data$", input$rename_source)
         },
         
         "missing" = {
           if(is.null(input$missing_source_var) || is.null(input$missing_new_name) ||
              input$missing_source_var == "" || input$missing_new_name == "") {
             return("# Select variable and enter new name")
           }
           
           r_code <- paste0("# Handle missing values: ", input$missing_new_name, "\n")
           r_code <- paste0(r_code, "data$", input$missing_new_name, " <- data$", input$missing_source_var, "\n")
           
           if(!is.null(input$missing_values) && input$missing_values != "") {
             missing_vals <- trimws(strsplit(input$missing_values, ",")[[1]])
             if(input$missing_action == "na") {
               r_code <- paste0(r_code, "data$", input$missing_new_name, "[data$", input$missing_source_var, " %in% c(", paste(missing_vals, collapse = ", "), ")] <- NA")
             } else {
               replacement <- if(!is.null(input$replacement_value)) input$replacement_value else 0
               r_code <- paste0(r_code, "data$", input$missing_new_name, "[data$", input$missing_source_var, " %in% c(", paste(missing_vals, collapse = ", "), ")] <- ", replacement)
             }
           } else {
             r_code <- paste0(r_code, "# Specify missing values to generate code")
           }
           
           return(r_code)
         },
         
         "# Select operation type and configure options"
  )
}

# ===================================================================
# VARIABLE CREATION MODULE
# ===================================================================

# SERVER FUNCTION
variable_creation_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for tracking
    values <- reactiveValues(
      created_vars_df = data.frame(
        Variable = character(0), 
        Source = character(0),
        Type = character(0), 
        Created = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    )
    
    # Update variable choices when data changes
    observe({
      req(shared_data$working_data)
      
      data <- shared_data$working_data
      all_vars <- names(data)
      numeric_vars <- all_vars[sapply(data, is.numeric)]
      categorical_vars <- all_vars[sapply(data, function(x) {
        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x[!is.na(x)])) <= 10)
      })]
      
      updateSelectInput(session, "source_var", choices = setNames(all_vars, all_vars))
      updateSelectInput(session, "compute_variables", choices = setNames(all_vars, all_vars))
      updateSelectInput(session, "rename_source", choices = setNames(all_vars, all_vars))
      updateSelectInput(session, "missing_source_var", choices = setNames(all_vars, all_vars))
      updateSelectInput(session, "cat_source_var", choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "cont_source_var", choices = setNames(numeric_vars, numeric_vars))
    })
    
    # Reset button observers
    observeEvent(input$reset_source, {
      updateSelectInput(session, "source_var", selected = "")
      updateTextInput(session, "new_var_name", value = "")
      updateTextAreaInput(session, "numeric_rules", value = "")
    })
    
    observeEvent(input$reset_compute_vars, {
      updateSelectInput(session, "compute_variables", selected = character(0))
      updateTextInput(session, "computed_var_name", value = "")
    })
    
    observeEvent(input$reset_rename, {
      updateSelectInput(session, "rename_source", selected = "")
      updateTextInput(session, "rename_new_name", value = "")
    })
    
    observeEvent(input$reset_missing, {
      updateSelectInput(session, "missing_source_var", selected = "")
      updateTextInput(session, "missing_new_name", value = "")
      updateTextInput(session, "missing_values", value = "")
    })
    
    observeEvent(input$reset_cat_source, {
      updateSelectInput(session, "cat_source_var", selected = "")
      updateTextInput(session, "cat_new_name", value = "")
    })
    
    observeEvent(input$reset_cont_source, {
      updateSelectInput(session, "cont_source_var", selected = "")
      updateTextInput(session, "cont_new_name", value = "")
    })
    
    # Dynamic UI for manual mapping
    output$manual_mapping_ui <- renderUI({
      req(shared_data$working_data, input$cat_source_var)
      
      if(input$cat_source_var == "" || !input$cat_source_var %in% names(shared_data$working_data)) {
        return(div(class = "alert alert-warning", "Please select a valid categorical variable"))
      }
      
      var_data <- shared_data$working_data[[input$cat_source_var]]
      unique_values <- sort(unique(var_data[!is.na(var_data)]))
      
      if(length(unique_values) == 0) {
        return(div(class = "alert alert-warning", "No categories found"))
      }
      
      if(length(unique_values) > 20) {
        return(div(class = "alert alert-warning", 
                   paste("Too many categories (", length(unique_values), "). Consider using ordinal ranking instead.")))
      }
      
      freq_table <- table(var_data, useNA = "ifany")
      
      mapping_inputs <- lapply(seq_along(unique_values), function(i) {
        val <- unique_values[i]
        input_id <- paste0("map_", i)
        frequency <- freq_table[as.character(val)]
        
        div(style = "margin-bottom: 8px; padding: 6px; background-color: #f8f9fa; border-radius: 3px;",
            fluidRow(
              column(6, 
                     tags$strong(style = "color: #495057;", val),
                     br(),
                     tags$small(style = "color: #6c757d;", paste("n =", frequency))
              ),
              column(6, 
                     numericInput(session$ns(input_id), "Value:", value = i, step = 0.1, width = "80px")
              )
            )
        )
      })
      
      return(tagList(mapping_inputs))
    })
    
    # Base category UI for dummy variables
    output$base_category_ui <- renderUI({
      req(shared_data$working_data, input$cat_source_var)
      
      if(input$cat_source_var == "" || input$include_all_dummies) return(NULL)
      
      var_data <- shared_data$working_data[[input$cat_source_var]]
      unique_values <- sort(unique(var_data[!is.na(var_data)]))
      
      selectInput(session$ns("base_category"), "Reference Category (will be coded as 0):",
                  choices = setNames(unique_values, unique_values),
                  selected = unique_values[1])
    })
    
    # Effect coding UI
    output$effect_base_category_ui <- renderUI({
      req(shared_data$working_data, input$cat_source_var)
      
      if(input$cat_source_var == "") return(NULL)
      
      var_data <- shared_data$working_data[[input$cat_source_var]]
      unique_values <- sort(unique(var_data[!is.na(var_data)]))
      
      selectInput(session$ns("effect_base_category"), "Reference Category (will be coded as -1):",
                  choices = setNames(unique_values, unique_values),
                  selected = unique_values[length(unique_values)])
    })
    
    # Helper buttons for manual mapping
    observeEvent(input$reset_mapping, {
      req(shared_data$working_data, input$cat_source_var)
      
      if(input$cat_source_var == "") return()
      
      var_data <- shared_data$working_data[[input$cat_source_var]]
      unique_values <- sort(unique(var_data[!is.na(var_data)]))
      
      for(i in seq_along(unique_values)) {
        updateNumericInput(session, paste0("map_", i), value = i)
      }
      showNotification("Reset to sequential values", type = "message")
    })
    
    observeEvent(input$set_binary, {
      req(shared_data$working_data, input$cat_source_var)
      
      if(input$cat_source_var == "") return()
      
      var_data <- shared_data$working_data[[input$cat_source_var]]
      unique_values <- sort(unique(var_data[!is.na(var_data)]))
      
      if(length(unique_values) >= 2) {
        updateNumericInput(session, "map_1", value = 0)
        for(i in 2:length(unique_values)) {
          updateNumericInput(session, paste0("map_", i), value = 1)
        }
        showNotification("Set to binary values", type = "message")
      }
    })
    
    # Debug parsing output
    output$debug_parsing <- renderText({
      if(input$operation_type != "recode" || is.null(input$numeric_rules) || input$numeric_rules == "") {
        return("Enter rules to see parsing status")
      }
      
      rules <- strsplit(input$numeric_rules, "\n")[[1]]
      rules <- trimws(rules[rules != ""])
      if(length(rules) == 0) return("No valid rules entered")
      
      debug_text <- ""
      for(i in seq_along(rules)) {
        parsed <- parse_recode_rule(rules[i])
        if(!is.null(parsed$error)) {
          debug_text <- paste0(debug_text, "❌ Rule ", i, ": ", parsed$error, "\n")
        } else if(parsed$type == "condition") {
          debug_text <- paste0(debug_text, "✅ Rule ", i, ": ", parsed$operator, " ", parsed$value, " → ", parsed$new_value, "\n")
        } else {
          debug_text <- paste0(debug_text, "✅ Rule ", i, ": ", parsed$old_value, " → ", parsed$new_value, "\n")
        }
      }
      return(debug_text)
    })
    
    # Variable summary output
    output$variable_summary <- renderPrint({
      req(shared_data$working_data)
      
      switch(input$operation_type,
             "recode" = {
               if(is.null(input$source_var) || input$source_var == "") {
                 return("Please select a source variable")
               }
               
               var_data <- shared_data$working_data[[input$source_var]]
               cat("Source Variable:", input$source_var, "\n")
               cat("Type:", class(var_data)[1], "\n")
               cat("Length:", length(var_data), "\n")
               cat("Missing:", sum(is.na(var_data)), "\n\n")
               
               if(is.numeric(var_data)) {
                 print(summary(var_data))
               } else {
                 print(table(var_data, useNA = "ifany"))
               }
             },
             
             "compute" = {
               if(is.null(input$compute_variables) || length(input$compute_variables) == 0) {
                 return("Please select variables to compute")
               }
               
               cat("Computation Type:", input$computation_type, "\n")
               cat("Selected Variables:", length(input$compute_variables), "\n")
               for(var in input$compute_variables[1:min(5, length(input$compute_variables))]) {
                 var_data <- shared_data$working_data[[var]]
                 if(is.numeric(var_data)) {
                   cat("  ", var, ": Mean =", round(mean(var_data, na.rm = TRUE), 2), "\n")
                 } else {
                   cat("  ", var, ": Non-numeric\n")
                 }
               }
             },
             
             "cat_to_cont" = {
               if(is.null(input$cat_source_var) || input$cat_source_var == "") {
                 return("Please select a categorical variable")
               }
               
               var_data <- shared_data$working_data[[input$cat_source_var]]
               cat("Variable:", input$cat_source_var, "\n")
               cat("Method:", input$cat_conversion_type, "\n")
               cat("Categories:", length(unique(var_data[!is.na(var_data)])), "\n\n")
               print(table(var_data, useNA = "ifany"))
             },
             
             "cont_to_cat" = {
               if(is.null(input$cont_source_var) || input$cont_source_var == "") {
                 return("Please select a continuous variable")
               }
               
               var_data <- shared_data$working_data[[input$cont_source_var]]
               cat("Variable:", input$cont_source_var, "\n")
               cat("Method:", input$cont_conversion_type, "\n")
               print(summary(var_data))
             },
             
             "rename" = {
               if(is.null(input$rename_source) || input$rename_source == "") {
                 return("Please select a variable to rename")
               }
               
               var_data <- shared_data$working_data[[input$rename_source]]
               cat("Variable:", input$rename_source, "\n")
               cat("Type:", class(var_data)[1], "\n")
               cat("Length:", length(var_data), "\n")
             },
             
             "missing" = {
               if(is.null(input$missing_source_var) || input$missing_source_var == "") {
                 return("Please select a variable")
               }
               
               var_data <- shared_data$working_data[[input$missing_source_var]]
               cat("Variable:", input$missing_source_var, "\n")
               cat("Current missing:", sum(is.na(var_data)), "\n")
               
               if(!is.null(input$missing_values) && input$missing_values != "") {
                 missing_vals <- trimws(strsplit(input$missing_values, ",")[[1]])
                 cat("Values to treat as missing:", paste(missing_vals, collapse = ", "), "\n")
               }
             },
             
             "Select an operation type to see variable details"
      )
    })
    
    # FIXED: Complete R code generation using the generate_complete_r_code function
    output$current_r_code <- renderText({
      req(shared_data$working_data)
      generate_complete_r_code(input, shared_data)
    })
    
    # ===================================================================
    # VARIABLE CREATION MODULE
    # ===================================================================
    
    # FIXED: Variable creation logic with comprehensive error handling AND R CODE LOGGING
    observeEvent(input$create_variable, {
      req(shared_data$working_data)
      
      # Validate inputs first
      validation_msg <- switch(input$operation_type,
                               "recode" = if(is.null(input$source_var) || is.null(input$new_var_name) || 
                                             input$source_var == "" || input$new_var_name == "") "Please select source variable and enter new variable name.",
                               "compute" = if(is.null(input$computed_var_name) || is.null(input$compute_variables) || 
                                              input$computed_var_name == "" || length(input$compute_variables) == 0) "Please enter variable name and select variables to compute.",
                               "cat_to_cont" = if(is.null(input$cat_source_var) || is.null(input$cat_new_name) || 
                                                  input$cat_source_var == "" || input$cat_new_name == "") "Please select categorical variable and enter new variable name.",
                               "cont_to_cat" = if(is.null(input$cont_source_var) || is.null(input$cont_new_name) || 
                                                  input$cont_source_var == "" || input$cont_new_name == "") "Please select continuous variable and enter new variable name.",
                               "rename" = if(is.null(input$rename_source) || is.null(input$rename_new_name) || 
                                             input$rename_source == "" || input$rename_new_name == "") "Please select source variable and enter new name.",
                               "missing" = if(is.null(input$missing_source_var) || is.null(input$missing_new_name) || 
                                              input$missing_source_var == "" || input$missing_new_name == "") "Please complete all missing value fields.",
                               NULL
      )
      
      if(!is.null(validation_msg)) {
        showNotification(validation_msg, type = "warning")
        return()
      }
      
      # Execute variable creation with error handling
      tryCatch({
        df <- shared_data$working_data
        new_vars <- list()
        var_name <- ""
        operation_desc <- ""
        
        # FIXED: Generate R code BEFORE creating variables
        generated_r_code <- generate_complete_r_code(input, shared_data)
        
        if(input$operation_type == "recode") {
          var_name <- input$new_var_name
          operation_desc <- "Recoding"
          source_data <- df[[input$source_var]]
          new_var <- source_data
          
          if(!is.null(input$numeric_rules) && input$numeric_rules != "") {
            rules <- strsplit(input$numeric_rules, "\n")[[1]]
            rules <- trimws(rules[rules != ""])
            
            for(rule in rules) {
              parsed <- parse_recode_rule(rule)
              if(!is.null(parsed$error)) next
              
              if(parsed$type == "condition") {
                condition_result <- apply_condition(source_data, paste0(parsed$operator, parsed$value))
                if(parsed$new_value == "NA") {
                  new_var[condition_result] <- NA
                } else {
                  new_numeric <- as.numeric(parsed$new_value)
                  if(!is.na(new_numeric)) new_var[condition_result] <- new_numeric
                }
              } else {
                old_numeric <- suppressWarnings(as.numeric(parsed$old_value))
                if(!is.na(old_numeric)) {
                  if(parsed$new_value == "NA") {
                    new_var[source_data == old_numeric & !is.na(source_data)] <- NA
                  } else {
                    new_numeric <- as.numeric(parsed$new_value)
                    if(!is.na(new_numeric)) {
                      new_var[source_data == old_numeric & !is.na(source_data)] <- new_numeric
                    }
                  }
                }
              }
            }
          }
          new_vars[[var_name]] <- new_var
          
        } else if(input$operation_type == "compute") {
          var_name <- input$computed_var_name
          operation_desc <- paste("Computing -", input$computation_type)
          
          if(input$computation_type == "sum") {
            new_var <- rowSums(df[, input$compute_variables, drop = FALSE], na.rm = !input$require_all)
          } else if(input$computation_type == "mean") {
            new_var <- rowMeans(df[, input$compute_variables, drop = FALSE], na.rm = !input$require_all)
          } else if(input$computation_type == "index") {
            if(is.null(input$min_value) || is.null(input$max_value)) {
              showNotification("Please set min and max values for index calculation.", type = "warning")
              return()
            }
            sum_scores <- rowSums(df[, input$compute_variables, drop = FALSE], na.rm = !input$require_all)
            min_possible <- input$min_value * length(input$compute_variables)
            max_possible <- input$max_value * length(input$compute_variables)
            new_var <- (sum_scores - min_possible) / (max_possible - min_possible)
          } else if(input$computation_type == "weighted_sum") {
            if(is.null(input$weights) || input$weights == "") {
              showNotification("Please enter weights for weighted sum.", type = "warning")
              return()
            }
            weights <- as.numeric(strsplit(input$weights, ",")[[1]])
            if(length(weights) != length(input$compute_variables)) {
              showNotification("Number of weights must match number of variables.", type = "error")
              return()
            }
            weight_matrix <- matrix(rep(weights, nrow(df)), nrow = nrow(df), byrow = TRUE)
            new_var <- rowSums(df[, input$compute_variables, drop = FALSE] * weight_matrix, na.rm = !input$require_all)
          } else if(input$computation_type == "count") {
            new_var <- rowSums(!is.na(df[, input$compute_variables, drop = FALSE]))
          } else if(input$computation_type == "custom") {
            if(is.null(input$custom_formula) || input$custom_formula == "") {
              showNotification("Please enter a custom formula.", type = "warning")
              return()
            }
            new_var <- with(df, eval(parse(text = input$custom_formula)))
          }
          new_vars[[var_name]] <- new_var
          
        } else if(input$operation_type == "cat_to_cont") {
          var_name <- input$cat_new_name
          operation_desc <- paste("Cat→Cont -", input$cat_conversion_type)
          source_data <- df[[input$cat_source_var]]
          
          if(input$cat_conversion_type == "manual") {
            unique_values <- sort(unique(source_data[!is.na(source_data)]))
            new_var <- as.numeric(source_data)
            
            for(i in seq_along(unique_values)) {
              val <- unique_values[i]
              new_val_input <- input[[paste0("map_", i)]]
              if(!is.null(new_val_input)) {
                new_var[source_data == val & !is.na(source_data)] <- new_val_input
              }
            }
            new_vars[[var_name]] <- new_var
            
          } else if(input$cat_conversion_type == "ordinal") {
            new_vars[[var_name]] <- as.numeric(as.factor(source_data))
            
          } else if(input$cat_conversion_type == "dummy") {
            unique_values <- sort(unique(source_data[!is.na(source_data)]))
            
            if(input$include_all_dummies) {
              for(val in unique_values) {
                dummy_name <- paste0(var_name, "_", make.names(val))
                new_vars[[dummy_name]] <- as.numeric(source_data == val)
              }
            } else {
              if(is.null(input$base_category) || input$base_category == "") {
                showNotification("Please select a base category for dummy variables.", type = "warning")
                return()
              }
              non_base_cats <- unique_values[unique_values != input$base_category]
              for(val in non_base_cats) {
                dummy_name <- paste0(var_name, "_", make.names(val))
                new_vars[[dummy_name]] <- as.numeric(source_data == val)
              }
            }
            
          } else if(input$cat_conversion_type == "effect") {
            if(is.null(input$effect_base_category) || input$effect_base_category == "") {
              showNotification("Please select a base category for effect coding.", type = "warning")
              return()
            }
            unique_values <- sort(unique(source_data[!is.na(source_data)]))
            non_base_cats <- unique_values[unique_values != input$effect_base_category]
            
            for(val in non_base_cats) {
              effect_name <- paste0(var_name, "_", make.names(val))
              effect_var <- rep(0, length(source_data))
              effect_var[source_data == val] <- 1
              effect_var[source_data == input$effect_base_category] <- -1
              effect_var[is.na(source_data)] <- NA
              new_vars[[effect_name]] <- effect_var
            }
          }
          
        } else if(input$operation_type == "cont_to_cat") {
          var_name <- input$cont_new_name
          operation_desc <- paste("Cont→Cat -", input$cont_conversion_type)
          source_data <- df[[input$cont_source_var]]
          
          if(input$cont_conversion_type == "custom_cuts") {
            if(is.null(input$custom_cutpoints) || input$custom_cutpoints == "") {
              showNotification("Please enter cutpoints.", type = "warning")
              return()
            }
            cutpoints <- as.numeric(strsplit(input$custom_cutpoints, ",")[[1]])
            cutpoints <- cutpoints[!is.na(cutpoints)]
            if(length(cutpoints) == 0) {
              showNotification("Please enter valid numeric cutpoints.", type = "warning")
              return()
            }
            cutpoints <- c(-Inf, sort(cutpoints), Inf)
            
            if(!is.null(input$custom_labels) && input$custom_labels != "") {
              labels <- trimws(strsplit(input$custom_labels, ",")[[1]])
              if(length(labels) == length(cutpoints) - 1) {
                new_var <- cut(source_data, breaks = cutpoints, labels = labels, include.lowest = TRUE)
              } else {
                new_var <- cut(source_data, breaks = cutpoints, include.lowest = TRUE)
              }
            } else {
              new_var <- cut(source_data, breaks = cutpoints, include.lowest = TRUE)
            }
            
          } else if(input$cont_conversion_type == "equal_width") {
            new_var <- cut(source_data, breaks = input$n_bins, include.lowest = TRUE)
            
          } else if(input$cont_conversion_type == "quantiles") {
            quantile_breaks <- quantile(source_data, probs = seq(0, 1, length.out = input$n_quantiles + 1), na.rm = TRUE)
            new_var <- cut(source_data, breaks = quantile_breaks, include.lowest = TRUE)
            
          } else if(input$cont_conversion_type == "sd_cuts") {
            mean_val <- mean(source_data, na.rm = TRUE)
            sd_val <- sd(source_data, na.rm = TRUE)
            
            if(input$sd_method == "1sd") {
              breaks <- c(-Inf, mean_val - sd_val, mean_val + sd_val, Inf)
              labels <- c("Low", "Medium", "High")
            } else if(input$sd_method == "half_sd") {
              breaks <- c(-Inf, mean_val - sd_val, mean_val - 0.5*sd_val, mean_val + 0.5*sd_val, mean_val + sd_val, Inf)
              labels <- c("Very Low", "Low", "Medium", "High", "Very High")
            } else if(input$sd_method == "2sd") {
              breaks <- c(-Inf, mean_val - 2*sd_val, mean_val - sd_val, mean_val + sd_val, mean_val + 2*sd_val, Inf)
              labels <- c("Very Low", "Low", "Medium", "High", "Very High")
            }
            
            new_var <- cut(source_data, breaks = breaks, labels = labels, include.lowest = TRUE)
          }
          
          new_vars[[var_name]] <- new_var
          
        } else if(input$operation_type == "rename") {
          var_name <- input$rename_new_name
          operation_desc <- "Renaming"
          new_vars[[var_name]] <- df[[input$rename_source]]
          
        } else if(input$operation_type == "missing") {
          var_name <- input$missing_new_name
          operation_desc <- "Missing Value Handling"
          source_data <- df[[input$missing_source_var]]
          new_var <- source_data
          
          if(!is.null(input$missing_values) && input$missing_values != "") {
            missing_vals <- trimws(strsplit(input$missing_values, ",")[[1]])
            
            if(is.numeric(source_data)) {
              missing_numeric <- as.numeric(missing_vals)
              missing_numeric <- missing_numeric[!is.na(missing_numeric)]
              
              if(length(missing_numeric) > 0) {
                if(input$missing_action == "na") {
                  new_var[source_data %in% missing_numeric] <- NA
                } else {
                  new_var[source_data %in% missing_numeric] <- input$replacement_value
                }
              }
            } else {
              if(input$missing_action == "na") {
                new_var[source_data %in% missing_vals] <- NA
              } else {
                new_var[source_data %in% missing_vals] <- as.character(input$replacement_value)
              }
            }
          }
          
          new_vars[[var_name]] <- new_var
        }
        
        # Add variables to dataset
        if(length(new_vars) > 0) {
          for(vname in names(new_vars)) {
            df[[vname]] <- new_vars[[vname]]
          }
          shared_data$working_data <- df
          
          # FIXED: Log R code to shared_data using add_r_code function
          add_r_code(shared_data, generated_r_code, paste("Created variable(s):", paste(names(new_vars), collapse = ", ")))
          
          # Update tracking
          for(vname in names(new_vars)) {
            source_var <- switch(input$operation_type,
                                 "recode" = input$source_var,
                                 "compute" = paste(input$compute_variables, collapse = ", "),
                                 "cat_to_cont" = input$cat_source_var,
                                 "cont_to_cat" = input$cont_source_var,
                                 "rename" = input$rename_source,
                                 "missing" = input$missing_source_var)
            
            new_row <- data.frame(
              Variable = vname, 
              Source = source_var,
              Type = operation_desc, 
              Created = Sys.time(), 
              stringsAsFactors = FALSE
            )
            values$created_vars_df <- rbind(values$created_vars_df, new_row)
          }
          
          # Update shared_data for consistency
          shared_data$created_vars <- values$created_vars_df
          
          # Success notification
          var_count <- length(new_vars)
          if(var_count == 1) {
            showNotification(paste("Variable", names(new_vars)[1], "created successfully!"), type = "message")
          } else {
            showNotification(paste(var_count, "variables created successfully!"), type = "message")
          }
        }
        
      }, error = function(e) {
        showNotification(paste("Error creating variable:", e$message), type = "error")
      })
    })
    
    # Created variables table with better performance
    output$created_variables_table <- DT::renderDataTable({
      if(nrow(values$created_vars_df) > 0) {
        display_df <- values$created_vars_df
        display_df$Created <- format(display_df$Created, "%Y-%m-%d %H:%M")
        
        DT::datatable(display_df, 
                      options = list(
                        pageLength = 10, 
                        dom = 'lrtip', 
                        scrollX = TRUE,
                        columnDefs = list(list(width = '200px', targets = 1))
                      ),
                      class = 'cell-border stripe hover compact',
                      rownames = FALSE)
      } else {
        DT::datatable(data.frame(Message = "No variables created yet"), 
                      options = list(dom = 't'),
                      rownames = FALSE)
      }
    })
    
    # Variable preview with performance optimizations
    output$variable_preview <- DT::renderDataTable({
      req(shared_data$working_data)
      
      df <- shared_data$working_data
      created_var_names <- if(nrow(values$created_vars_df) > 0) values$created_vars_df$Variable else character(0)
      
      # Performance optimization: limit columns and rows
      max_cols <- 12
      max_rows <- 100
      
      # Get first few columns
      first_cols <- names(df)[1:min(5, ncol(df))]
      
      if(length(created_var_names) > 0) {
        # Get source variables
        source_vars <- if(nrow(values$created_vars_df) > 0) {
          sources <- values$created_vars_df$Source
          all_sources <- unlist(strsplit(sources, ", "))
          unique(all_sources[all_sources %in% names(df)])
        } else character(0)
        
        # Combine columns: first cols + source vars + created vars
        preview_vars <- unique(c(first_cols, source_vars, created_var_names))
        preview_vars <- preview_vars[preview_vars %in% names(df)]
        preview_vars <- preview_vars[1:min(max_cols, length(preview_vars))]
        
        caption_text <- paste("Showing", length(preview_vars), "variables (limited for performance)")
      } else {
        preview_vars <- first_cols
        caption_text <- "Showing first 5 variables"
      }
      
      # Create preview dataframe
      preview_df <- df[, preview_vars, drop = FALSE]
      
      # Limit rows
      if(nrow(preview_df) > max_rows) {
        display_df <- preview_df[1:max_rows, , drop = FALSE]
        caption_text <- paste(caption_text, "- First", max_rows, "rows")
      } else {
        display_df <- preview_df
        caption_text <- paste(caption_text, "- All", nrow(display_df), "rows")
      }
      
      # Create datatable
      dt <- DT::datatable(display_df, 
                          caption = caption_text,
                          options = list(
                            scrollX = TRUE, 
                            scrollY = "350px", 
                            pageLength = 50, 
                            dom = 'frtip',
                            deferRender = TRUE,
                            columnDefs = list(list(className = 'dt-center', targets = '_all'))
                          ),
                          class = 'cell-border stripe hover compact',
                          rownames = FALSE)
      
      # Apply styling for created and source variables
      if(length(created_var_names) > 0) {
        created_col_indices <- which(names(display_df) %in% created_var_names)
        if(length(created_col_indices) > 0) {
          dt <- dt %>% formatStyle(columns = created_col_indices, 
                                   backgroundColor = '#d4edda', fontWeight = 'bold', color = '#155724')
        }
        
        source_vars <- if(nrow(values$created_vars_df) > 0) {
          sources <- values$created_vars_df$Source
          all_sources <- unlist(strsplit(sources, ", "))
          unique(all_sources[all_sources %in% names(df)])
        } else character(0)
        
        source_col_indices <- which(names(display_df) %in% source_vars & !names(display_df) %in% created_var_names)
        if(length(source_col_indices) > 0) {
          dt <- dt %>% formatStyle(columns = source_col_indices, 
                                   backgroundColor = '#fff3cd', fontWeight = 'bold', color = '#856404')
        }
      }
      
      return(dt)
    })
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() { paste0("modified_dataset_", Sys.Date(), ".csv") },
      content = function(file) {
        req(shared_data$working_data)
        write.csv(shared_data$working_data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
  }) # End of moduleServer
} # End of variable_creation_server function

# ===================================================================
# END OF COMPLETE WORKING VARIABLE CREATION MODULE WITH R CODE LOGGING
# ===================================================================