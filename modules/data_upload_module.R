# ===================================================================
# ENHANCED DATA UPLOAD MODULE: modules/data_upload_module.R
# Updated to support multiple file formats: .dta, .sav, .rds, .csv, .xlsx, .xls, .tsv, .txt
# ===================================================================

# Helper function for null coalescing
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}

# UI Function
data_upload_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "upload",
          fluidRow(
            box(title = "Upload Your Dataset", status = "primary", solidHeader = TRUE, width = 12,
                fileInput(ns("file"), "Choose Data File", 
                          accept = c(".dta", ".sav", ".rds", ".csv", ".xlsx", ".xls", ".tsv", ".txt")),
                helpText("📁 Supported formats: Stata (.dta), SPSS (.sav), R (.rds), CSV (.csv), Excel (.xlsx, .xls), Tab-separated (.tsv), Text (.txt)"),
                helpText("📊 Maximum file size: 100MB"),
                helpText("⚠️ For very large datasets, use the variable filtering options below."),
                
                # File format detection and options
                conditionalPanel(condition = paste0("output['", ns("showFormatOptions"), "']"),
                                 div(style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h5("📋 File Format Detected:", style = "color: #1976d2; margin-top: 0;"),
                                     htmlOutput(ns("format_info")),
                                     
                                     # CSV/Text specific options
                                     conditionalPanel(condition = paste0("output['", ns("isCsvFormat"), "']"),
                                                      h6("CSV/Text Import Options:"),
                                                      fluidRow(
                                                        column(6, 
                                                               selectInput(ns("csv_sep"), "Separator:",
                                                                           choices = list("Comma (,)" = ",", "Semicolon (;)" = ";", 
                                                                                          "Tab" = "\t", "Pipe (|)" = "|", "Space" = " "),
                                                                           selected = ",")),
                                                        column(6,
                                                               selectInput(ns("csv_dec"), "Decimal:",
                                                                           choices = list("Period (.)" = ".", "Comma (,)" = ","),
                                                                           selected = "."))
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               checkboxInput(ns("csv_header"), "Header row", value = TRUE)),
                                                        column(6,
                                                               selectInput(ns("csv_quote"), "Quote character:",
                                                                           choices = list("Double quote (\")" = '"', "Single quote (')" = "'", "None" = ""),
                                                                           selected = '"'))
                                                      ),
                                                      numericInput(ns("csv_skip"), "Skip lines:", value = 0, min = 0, max = 100)
                                     ),
                                     
                                     # Excel specific options
                                     conditionalPanel(condition = paste0("output['", ns("isExcelFormat"), "']"),
                                                      h6("Excel Import Options:"),
                                                      fluidRow(
                                                        column(6,
                                                               textInput(ns("excel_sheet"), "Sheet name/number:", value = "1")),
                                                        column(6,
                                                               numericInput(ns("excel_skip"), "Skip rows:", value = 0, min = 0, max = 100))
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               textInput(ns("excel_range"), "Cell range (optional):", placeholder = "A1:Z1000")),
                                                        column(6,
                                                               checkboxInput(ns("excel_col_names"), "Column names", value = TRUE))
                                                      )
                                     ),
                                     
                                     actionButton(ns("reload_with_options"), "Apply Import Settings", 
                                                  class = "btn-info btn-sm", style = "margin-top: 10px;")
                                 )
                ),
                
                tags$div(style = "color: #666; font-size: 12px; margin-top: 10px;",
                         "💡 Tip: If you encounter issues, try converting your file to CSV first, or use the import options above."),
                br(),
                conditionalPanel(condition = paste0("output['", ns("fileUploaded"), "']"),
                                 h4("Data Preview:"), 
                                 DT::dataTableOutput(ns("dataPreview"))
                )
            )
          ),
          fluidRow(
            box(title = "Dataset Information", status = "info", solidHeader = TRUE, width = 8,
                conditionalPanel(condition = paste0("output['", ns("fileUploaded"), "']"), 
                                 htmlOutput(ns("dataset_info")),
                                 br(),
                                 h5("Import Summary:"),
                                 verbatimTextOutput(ns("import_summary"))
                )
            )
          ),
          fluidRow(  
            box(title = "Variable Type Analysis", status = "info", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = paste0("output['", ns("fileUploaded"), "']"),
                                 h5("Variable Classification:"),
                                 DT::dataTableOutput(ns("variable_types_table"))
                )
            ),
            box(title = "Variable Details", status = "info", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = paste0("output['", ns("fileUploaded"), "']"),
                                 selectInput(ns("inspect_variable"), "Inspect Variable:", choices = NULL),
                                 actionButton(ns("reset_inspect"), "Reset Selection", class = "btn-sm btn-secondary"),
                                 br(), br(),
                                 h5("Variable Information:"),
                                 verbatimTextOutput(ns("variable_details")),
                                 br(),
                                 conditionalPanel(condition = paste0("output['", ns("is_categorical"), "']"),
                                                  h5("Categorical Variable Analysis:"),
                                                  htmlOutput(ns("categorical_analysis"))
                                 )
                )
            )
          ),
          fluidRow(
            box(title = "Data Filtering Options", status = "warning", solidHeader = TRUE, width = 6,
                conditionalPanel(condition = paste0("output['", ns("fileUploaded"), "']"),
                                 h5("Keep Specific Variables:"),
                                 helpText("Select variables to keep in your working dataset (leave empty to keep all)"),
                                 selectInput(ns("variables_to_keep"), "Variables to Keep:", 
                                             choices = NULL, multiple = TRUE),
                                 actionButton(ns("reset_var_selection"), "Reset Selection", class = "btn-sm btn-secondary"),
                                 br(), br(),
                                 h5("Filter by Conditions:"),
                                 helpText("Apply filters to keep only certain observations"),
                                 selectInput(ns("filter_variable"), "Filter Variable:", choices = NULL),
                                 selectInput(ns("filter_operator"), "Operator:", 
                                             choices = list("Equal to (==)" = "==", "Not equal to (!=)" = "!=",
                                                            "Greater than (>)" = ">", "Less than (<)" = "<",
                                                            "Greater or equal (>=)" = ">=", "Less or equal (<=)" = "<=",
                                                            "Contains" = "contains", "Is not missing" = "not_na")),
                                 conditionalPanel(condition = paste0("input['", ns("filter_operator"), "'] != 'not_na'"),
                                                  textInput(ns("filter_value"), "Filter Value:", placeholder = "Enter value")
                                 ),
                                 actionButton(ns("add_filter"), "Add Filter", class = "btn-sm btn-primary"),
                                 br(), br(),
                                 h5("Current Filters:"),
                                 verbatimTextOutput(ns("current_filters")),
                                 br(),
                                 actionButton(ns("apply_filters"), "Apply All Filters", class = "btn-warning"),
                                 actionButton(ns("clear_filters"), "Clear All Filters", class = "btn-sm btn-secondary")
                )
            )
          )
  )
}

# Enhanced Stata file loading function
load_stata_file <- function(file_path) {
  method_success <- FALSE
  df <- NULL
  
  # Method 1: Try haven with minimal processing
  tryCatch({
    cat("Attempting Method 1: haven::read_dta with minimal processing\n")
    df_raw <- haven::read_dta(file_path)
    df <- haven::zap_labels(df_raw)
    df <- as.data.frame(df)
    cat("Method 1 successful!\n")
    method_success <- TRUE
  }, error = function(e) {
    cat("Method 1 failed:", e$message, "\n")
    method_success <<- FALSE
  })
  
  # Method 2: Try foreign package if Method 1 fails
  if (!method_success) {
    tryCatch({
      cat("Attempting Method 2: foreign::read.dta\n")
      if (!require(foreign, quietly = TRUE)) {
        install.packages("foreign")
        library(foreign)
      }
      
      df <- foreign::read.dta(file_path, convert.factors = FALSE)
      df <- as.data.frame(df)
      cat("Method 2 successful!\n")
      method_success <- TRUE
    }, error = function(e) {
      cat("Method 2 failed:", e$message, "\n")
    })
  }
  
  # Method 3: Try haven with manual column processing if others fail
  if (!method_success) {
    tryCatch({
      cat("Attempting Method 3: Manual column-by-column processing\n")
      df_raw <- haven::read_dta(file_path)
      
      # Process each column individually
      df_list <- list()
      for (col_name in names(df_raw)) {
        col_data <- df_raw[[col_name]]
        
        # Remove all attributes and convert to basic R types
        if (is.numeric(col_data) || is.integer(col_data)) {
          df_list[[col_name]] <- as.numeric(col_data)
        } else {
          # Convert everything else to character first
          df_list[[col_name]] <- as.character(col_data)
        }
      }
      
      df <- data.frame(df_list, stringsAsFactors = FALSE)
      cat("Method 3 successful!\n")
      method_success <- TRUE
    }, error = function(e) {
      cat("Method 3 failed:", e$message, "\n")
    })
  }
  
  if (!method_success || is.null(df)) {
    stop("Unable to read the .dta file with any method.")
  }
  
  # Post-process: Convert appropriate character columns to factors
  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    
    if (is.character(col_data)) {
      unique_vals <- length(unique(col_data[!is.na(col_data)]))
      total_vals <- length(col_data[!is.na(col_data)])
      
      # Convert to factor if it looks categorical
      if (unique_vals < 20 || (total_vals > 0 && unique_vals / total_vals < 0.5)) {
        df[[col_name]] <- factor(col_data)
      }
    }
  }
  
  return(df)
}

# Server Function
data_upload_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # Store file information and filters
    file_info <- reactiveValues(
      extension = NULL,
      size = NULL,
      name = NULL
    )
    filters_list <- reactiveVal(list())
    
    # Detect file format and show options
    observe({
      req(input$file)
      file_path <- input$file$datapath
      file_name <- input$file$name
      file_extension <- tools::file_ext(tolower(file_name))
      
      file_info$extension <- file_extension
      file_info$size <- input$file$size
      file_info$name <- file_name
    })
    
    # Show format options based on file type
    output$showFormatOptions <- reactive({
      return(!is.null(file_info$extension))
    })
    outputOptions(output, 'showFormatOptions', suspendWhenHidden = FALSE)
    
    # Check if CSV format
    output$isCsvFormat <- reactive({
      return(!is.null(file_info$extension) && file_info$extension %in% c("csv", "txt", "tsv"))
    })
    outputOptions(output, 'isCsvFormat', suspendWhenHidden = FALSE)
    
    # Check if Excel format
    output$isExcelFormat <- reactive({
      return(!is.null(file_info$extension) && file_info$extension %in% c("xlsx", "xls"))
    })
    outputOptions(output, 'isExcelFormat', suspendWhenHidden = FALSE)
    
    # Format information display
    output$format_info <- renderUI({
      req(file_info$extension)
      
      format_descriptions <- list(
        "dta" = "📊 Stata dataset file - will preserve variable labels and value labels",
        "sav" = "📊 SPSS dataset file - will preserve variable labels and value labels", 
        "rds" = "📊 R dataset file - native R format with all attributes preserved",
        "csv" = "📄 Comma-separated values - configure import options below if needed",
        "txt" = "📄 Text file - configure separator and other options below",
        "tsv" = "📄 Tab-separated values - will auto-detect tab separator",
        "xlsx" = "📈 Excel workbook - configure sheet and range options below",
        "xls" = "📈 Excel workbook (legacy) - configure sheet and range options below"
      )
      
      description <- format_descriptions[[file_info$extension]] %||% "📄 Unknown format"
      file_size_mb <- round(file_info$size / 1024^2, 2)
      
      HTML(paste0(
        "<strong>", toupper(file_info$extension), " File:</strong> ", description, "<br/>",
        "<strong>File name:</strong> ", file_info$name, "<br/>",
        "<strong>File size:</strong> ", file_size_mb, " MB"
      ))
    })
    
    # Enhanced file loading function
    load_data_file <- function(file_path, file_extension, options = list()) {
      df <- NULL
      method_used <- ""
      
      tryCatch({
        if (file_extension == "dta") {
          # Stata files
          method_used <- "Stata (haven::read_dta)"
          df <- load_stata_file(file_path)  # Use existing function
          
        } else if (file_extension == "sav") {
          # SPSS files
          method_used <- "SPSS (haven::read_sav)"
          df_raw <- haven::read_sav(file_path)
          df <- haven::zap_labels(df_raw)
          df <- as.data.frame(df)
          
        } else if (file_extension == "rds") {
          # R files
          method_used <- "R (readRDS)"
          df <- readRDS(file_path)
          if (!is.data.frame(df)) {
            stop("RDS file does not contain a data frame")
          }
          
        } else if (file_extension %in% c("csv", "txt")) {
          # CSV and text files
          method_used <- "CSV/Text (utils::read.csv)"
          
          # Get options with defaults
          sep <- options$sep %||% ","
          dec <- options$dec %||% "."
          header <- options$header %||% TRUE
          quote <- options$quote %||% '"'
          skip <- options$skip %||% 0
          
          # Auto-detect separator for .tsv files
          if (file_extension == "tsv") sep <- "\t"
          
          df <- utils::read.csv(file_path, 
                                sep = sep, 
                                dec = dec, 
                                header = header, 
                                quote = quote, 
                                skip = skip,
                                stringsAsFactors = FALSE,
                                na.strings = c("", "NA", "NULL", "null", "N/A", "#N/A"))
          
        } else if (file_extension == "tsv") {
          # Tab-separated values
          method_used <- "TSV (utils::read.delim)"
          
          skip <- options$skip %||% 0
          header <- options$header %||% TRUE
          
          df <- utils::read.delim(file_path, 
                                  sep = "\t", 
                                  header = header, 
                                  skip = skip,
                                  stringsAsFactors = FALSE,
                                  na.strings = c("", "NA", "NULL", "null", "N/A", "#N/A"))
          
        } else if (file_extension %in% c("xlsx", "xls")) {
          # Excel files
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("Package 'readxl' is required for Excel files. Please install it.")
          }
          
          method_used <- "Excel (readxl::read_excel)"
          
          # Get options with defaults
          sheet <- options$sheet %||% 1
          skip <- options$skip %||% 0
          col_names <- options$col_names %||% TRUE
          range <- options$range
          
          # Convert sheet to numeric if it's a number
          if (is.character(sheet) && grepl("^\\d+$", sheet)) {
            sheet <- as.numeric(sheet)
          }
          
          if (!is.null(range) && range != "") {
            df <- readxl::read_excel(file_path, 
                                     sheet = sheet, 
                                     range = range, 
                                     col_names = col_names)
          } else {
            df <- readxl::read_excel(file_path, 
                                     sheet = sheet, 
                                     skip = skip, 
                                     col_names = col_names)
          }
          
          df <- as.data.frame(df)
          
        } else {
          stop(paste("Unsupported file format:", file_extension))
        }
        
        # Post-process the data
        if (!is.null(df)) {
          # Convert character columns to factors where appropriate
          for (col_name in names(df)) {
            col_data <- df[[col_name]]
            
            if (is.character(col_data)) {
              unique_vals <- length(unique(col_data[!is.na(col_data)]))
              total_vals <- length(col_data[!is.na(col_data)])
              
              # Convert to factor if it looks categorical (fewer than 20 unique values or less than 50% unique)
              if (unique_vals < 20 || (total_vals > 0 && unique_vals / total_vals < 0.5)) {
                df[[col_name]] <- factor(col_data)
              }
            }
          }
        }
        
        return(list(data = df, method = method_used, success = TRUE))
        
      }, error = function(e) {
        return(list(data = NULL, method = method_used, success = FALSE, error = e$message))
      })
    }
    
    # Generate R code for import
    generate_import_code <- function(file_extension, file_name, options = list()) {
      base_code <- "# Load required libraries\n"
      
      if (file_extension == "dta") {
        base_code <- paste0(base_code, "library(haven)\n\n")
        base_code <- paste0(base_code, "# Read Stata file\n")
        base_code <- paste0(base_code, "data <- read_dta('", file_name, "')\n")
        base_code <- paste0(base_code, "data <- zap_labels(data)  # Remove labels if desired\n")
        base_code <- paste0(base_code, "data <- as.data.frame(data)")
        
      } else if (file_extension == "sav") {
        base_code <- paste0(base_code, "library(haven)\n\n")
        base_code <- paste0(base_code, "# Read SPSS file\n")
        base_code <- paste0(base_code, "data <- read_sav('", file_name, "')\n")
        base_code <- paste0(base_code, "data <- zap_labels(data)  # Remove labels if desired\n")
        base_code <- paste0(base_code, "data <- as.data.frame(data)")
        
      } else if (file_extension == "rds") {
        base_code <- paste0(base_code, "# Read R file\n")
        base_code <- paste0(base_code, "data <- readRDS('", file_name, "')")
        
      } else if (file_extension %in% c("csv", "txt")) {
        base_code <- paste0(base_code, "# Read CSV/Text file\n")
        if (length(options) > 0) {
          base_code <- paste0(base_code, "data <- read.csv('", file_name, "'")
          if (!is.null(options$sep)) base_code <- paste0(base_code, ", sep = '", options$sep, "'")
          if (!is.null(options$dec)) base_code <- paste0(base_code, ", dec = '", options$dec, "'")
          if (!is.null(options$header)) base_code <- paste0(base_code, ", header = ", options$header)
          if (!is.null(options$quote)) base_code <- paste0(base_code, ", quote = '", options$quote, "'")
          if (!is.null(options$skip) && options$skip > 0) base_code <- paste0(base_code, ", skip = ", options$skip)
          base_code <- paste0(base_code, ", stringsAsFactors = FALSE)")
        } else {
          base_code <- paste0(base_code, "data <- read.csv('", file_name, "', stringsAsFactors = FALSE)")
        }
        
      } else if (file_extension == "tsv") {
        base_code <- paste0(base_code, "# Read TSV file\n")
        base_code <- paste0(base_code, "data <- read.delim('", file_name, "', stringsAsFactors = FALSE)")
        
      } else if (file_extension %in% c("xlsx", "xls")) {
        base_code <- paste0(base_code, "library(readxl)\n\n")
        base_code <- paste0(base_code, "# Read Excel file\n")
        if (length(options) > 0) {
          base_code <- paste0(base_code, "data <- read_excel('", file_name, "'")
          if (!is.null(options$sheet)) base_code <- paste0(base_code, ", sheet = ", ifelse(is.numeric(options$sheet), options$sheet, paste0("'", options$sheet, "'")))
          if (!is.null(options$skip) && options$skip > 0) base_code <- paste0(base_code, ", skip = ", options$skip)
          if (!is.null(options$col_names)) base_code <- paste0(base_code, ", col_names = ", options$col_names)
          if (!is.null(options$range)) base_code <- paste0(base_code, ", range = '", options$range, "'")
          base_code <- paste0(base_code, ")\n")
          base_code <- paste0(base_code, "data <- as.data.frame(data)")
        } else {
          base_code <- paste0(base_code, "data <- read_excel('", file_name, "')\n")
          base_code <- paste0(base_code, "data <- as.data.frame(data)")
        }
      }
      
      return(base_code)
    }
    
    # File upload observer (initial load)
    observeEvent(input$file, {
      req(input$file)
      
      showNotification("Reading dataset... This may take a moment for large files.", type = "message", duration = 3)
      
      file_path <- input$file$datapath
      file_extension <- tools::file_ext(tolower(input$file$name))
      
      # Load with default options first
      result <- load_data_file(file_path, file_extension)
      
      if (result$success) {
        df <- result$data
        
        if(nrow(df) > 50000) {
          showNotification(paste("Large dataset detected:", nrow(df), "rows. Consider using filtering options."), 
                           type = "warning", duration = 5)
        }
        
        # Store in shared data
        shared_data$raw_data <- df
        shared_data$working_data <- df
        
        # Update UI choices
        all_vars <- names(df)
        updateSelectInput(session, "variables_to_keep", choices = all_vars)
        updateSelectInput(session, "filter_variable", choices = c("Select variable" = "", all_vars))
        updateSelectInput(session, "inspect_variable", choices = c("Select variable" = "", all_vars))
        
        # Generate appropriate R code based on file type
        r_code <- generate_import_code(file_extension, input$file$name)
        add_r_code(shared_data, r_code, "Data Loading")
        
        showNotification(paste("Dataset loaded successfully!", nrow(df), "observations,", ncol(df), "variables"), type = "message")
        
        # Store import information for display
        shared_data$import_info <- list(
          method = result$method,
          file_type = toupper(file_extension),
          file_name = input$file$name,
          rows = nrow(df),
          cols = ncol(df)
        )
        
      } else {
        showNotification(paste("Error reading file:", result$error), type = "error", duration = 15)
        showNotification("Try adjusting the import options below, or convert to CSV format.", type = "warning", duration = 10)
      }
    })
    
    # Reload with custom options
    observeEvent(input$reload_with_options, {
      req(input$file, file_info$extension)
      
      showNotification("Reloading with custom options...", type = "message", duration = 2)
      
      file_path <- input$file$datapath
      file_extension <- file_info$extension
      
      # Collect options based on file type
      options <- list()
      
      if (file_extension %in% c("csv", "txt", "tsv")) {
        options$sep <- input$csv_sep
        options$dec <- input$csv_dec
        options$header <- input$csv_header
        options$quote <- input$csv_quote
        options$skip <- input$csv_skip
      } else if (file_extension %in% c("xlsx", "xls")) {
        options$sheet <- input$excel_sheet
        options$skip <- input$excel_skip
        options$col_names <- input$excel_col_names
        options$range <- if(input$excel_range != "") input$excel_range else NULL
      }
      
      # Reload with custom options
      result <- load_data_file(file_path, file_extension, options)
      
      if (result$success) {
        df <- result$data
        
        # Update shared data
        shared_data$raw_data <- df
        shared_data$working_data <- df
        
        # Update UI choices
        all_vars <- names(df)
        updateSelectInput(session, "variables_to_keep", choices = all_vars)
        updateSelectInput(session, "filter_variable", choices = c("Select variable" = "", all_vars))
        updateSelectInput(session, "inspect_variable", choices = c("Select variable" = "", all_vars))
        
        # Generate R code with custom options
        r_code <- generate_import_code(file_extension, input$file$name, options)
        add_r_code(shared_data, r_code, "Data Loading (Custom Options)")
        
        showNotification(paste("Dataset reloaded successfully with custom options!", nrow(df), "observations,", ncol(df), "variables"), type = "message")
        
        # Update import information
        shared_data$import_info <- list(
          method = paste(result$method, "(with custom options)"),
          file_type = toupper(file_extension),
          file_name = input$file$name,
          rows = nrow(df),
          cols = ncol(df)
        )
        
      } else {
        showNotification(paste("Error reloading file:", result$error), type = "error", duration = 15)
      }
    })
    
    # Reset button observers
    observeEvent(input$reset_var_selection, {
      updateSelectInput(session, "variables_to_keep", selected = character(0))
    })
    
    observeEvent(input$reset_inspect, {
      updateSelectInput(session, "inspect_variable", selected = "")
    })
    
    # Add filter
    observeEvent(input$add_filter, {
      req(input$filter_variable, input$filter_operator)
      
      if(input$filter_variable == "") {
        showNotification("Please select a filter variable.", type = "warning")
        return()
      }
      
      # Create filter description
      if(input$filter_operator == "not_na") {
        filter_desc <- paste(input$filter_variable, "is not missing")
        filter_condition <- list(variable = input$filter_variable, operator = "not_na", value = NA)
      } else {
        req(input$filter_value)
        if(input$filter_value == "") {
          showNotification("Please enter a filter value.", type = "warning")
          return()
        }
        filter_desc <- paste(input$filter_variable, input$filter_operator, input$filter_value)
        filter_condition <- list(variable = input$filter_variable, operator = input$filter_operator, value = input$filter_value)
      }
      
      # Add to filters list
      current_filters <- filters_list()
      current_filters[[length(current_filters) + 1]] <- filter_condition
      filters_list(current_filters)
      
      showNotification(paste("Filter added:", filter_desc), type = "message")
    })
    
    # Clear filters
    observeEvent(input$clear_filters, {
      filters_list(list())
      showNotification("All filters cleared.", type = "message")
    })
    
    # Apply filters
    observeEvent(input$apply_filters, {
      req(shared_data$raw_data)
      
      df <- shared_data$raw_data
      
      # Apply variable selection
      if(!is.null(input$variables_to_keep) && length(input$variables_to_keep) > 0) {
        df <- df[, input$variables_to_keep, drop = FALSE]
        r_code <- paste0("# Keep selected variables\ndata <- data[, c('", 
                         paste(input$variables_to_keep, collapse = "', '"), "')]")
        add_r_code(shared_data, r_code, "Variable Selection")
      }
      
      # Apply row filters
      current_filters <- filters_list()
      if(length(current_filters) > 0) {
        filter_code_lines <- c("# Apply filters")
        
        for(i in seq_along(current_filters)) {
          filter <- current_filters[[i]]
          
          if(filter$operator == "not_na") {
            df <- df[!is.na(df[[filter$variable]]), ]
            filter_code_lines <- c(filter_code_lines, paste0("data <- data[!is.na(data$", filter$variable, "), ]"))
          } else if(filter$operator == "contains") {
            df <- df[grepl(filter$value, df[[filter$variable]], ignore.case = TRUE), ]
            filter_code_lines <- c(filter_code_lines, paste0("data <- data[grepl('", filter$value, "', data$", filter$variable, ", ignore.case = TRUE), ]"))
          } else {
            # Numeric or exact filters
            if(is.numeric(df[[filter$variable]])) {
              filter_value <- as.numeric(filter$value)
              if(!is.na(filter_value)) {
                if(filter$operator == "==") {
                  df <- df[df[[filter$variable]] == filter_value & !is.na(df[[filter$variable]]), ]
                } else if(filter$operator == "!=") {
                  df <- df[df[[filter$variable]] != filter_value & !is.na(df[[filter$variable]]), ]
                } else if(filter$operator == ">") {
                  df <- df[df[[filter$variable]] > filter_value & !is.na(df[[filter$variable]]), ]
                } else if(filter$operator == "<") {
                  df <- df[df[[filter$variable]] < filter_value & !is.na(df[[filter$variable]]), ]
                } else if(filter$operator == ">=") {
                  df <- df[df[[filter$variable]] >= filter_value & !is.na(df[[filter$variable]]), ]
                } else if(filter$operator == "<=") {
                  df <- df[df[[filter$variable]] <= filter_value & !is.na(df[[filter$variable]]), ]
                }
                filter_code_lines <- c(filter_code_lines, paste0("data <- data[data$", filter$variable, " ", filter$operator, " ", filter_value, " & !is.na(data$", filter$variable, "), ]"))
              }
            } else {
              # Character/factor filters
              if(filter$operator == "==") {
                df <- df[df[[filter$variable]] == filter$value & !is.na(df[[filter$variable]]), ]
                filter_code_lines <- c(filter_code_lines, paste0("data <- data[data$", filter$variable, " == '", filter$value, "' & !is.na(data$", filter$variable, "), ]"))
              } else if(filter$operator == "!=") {
                df <- df[df[[filter$variable]] != filter$value & !is.na(df[[filter$variable]]), ]
                filter_code_lines <- c(filter_code_lines, paste0("data <- data[data$", filter$variable, " != '", filter$value, "' & !is.na(data$", filter$variable, "), ]"))
              }
            }
          }
        }
        
        add_r_code(shared_data, paste(filter_code_lines, collapse = "\n"), "Data Filtering")
      }
      
      shared_data$working_data <- df
      showNotification(paste("Filters applied! Dataset now has", nrow(df), "observations and", ncol(df), "variables."), type = "message")
    })
    
    # Current filters display
    output$current_filters <- renderText({
      current_filters <- filters_list()
      if(length(current_filters) == 0) {
        return("No filters applied")
      }
      
      filter_descriptions <- sapply(current_filters, function(f) {
        if(f$operator == "not_na") {
          paste(f$variable, "is not missing")
        } else {
          paste(f$variable, f$operator, f$value)
        }
      })
      paste(paste(1:length(filter_descriptions), filter_descriptions, sep = ". "), collapse = "\n")
    })
    
    # Check if file is uploaded
    output$fileUploaded <- reactive({ return(!is.null(shared_data$raw_data)) })
    outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
    
    # Dataset information
    output$dataset_info <- renderUI({
      req(shared_data$working_data)
      df <- shared_data$working_data
      original_df <- shared_data$raw_data
      
      HTML(paste0(
        "<strong>Dataset Overview:</strong><br/>",
        "• Original: ", format(nrow(original_df), big.mark = ","), " observations, ", ncol(original_df), " variables<br/>",
        "• Working: ", format(nrow(df), big.mark = ","), " observations, ", ncol(df), " variables<br/>",
        "• Numeric variables: ", sum(sapply(df, is.numeric)), "<br/>",
        "• File size: ~", round(object.size(df) / 1024^2, 1), " MB in memory"
      ))
    })
    
    # Import summary information
    output$import_summary <- renderText({
      req(shared_data$import_info)
      info <- shared_data$import_info
      
      summary_text <- paste(
        "Import Summary:",
        paste("File type:", info$file_type),
        paste("Import method:", info$method),
        paste("File name:", info$file_name),
        paste("Dimensions:", paste(info$rows, "×", info$cols)),
        "",
        "Import successful ✓",
        sep = "\n"
      )
      
      return(summary_text)
    })
    
    # Variable types table
    output$variable_types_table <- DT::renderDataTable({
      req(shared_data$working_data)
      df <- shared_data$working_data
      
      var_info <- data.frame(
        Variable = names(df),
        Type = sapply(df, function(x) class(x)[1]),
        Classification = sapply(df, classify_variable),
        `Unique Values` = sapply(df, function(x) length(unique(x[!is.na(x)]))),
        Missing = sapply(df, function(x) sum(is.na(x))),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(var_info, options = list(pageLength = 10, dom = 'frtip', scrollY = "300px")) %>%
        formatStyle("Classification", 
                    backgroundColor = styleEqual(c("Continuous", "Categorical", "Ordinal/Categorical"), 
                                                 c("#e8f5e8", "#fff3cd", "#e8f4fd")))
    })
    
    # Variable details
    output$variable_details <- renderPrint({
      req(shared_data$working_data, input$inspect_variable)
      if(input$inspect_variable == "") return("Select a variable to inspect")
      
      df <- shared_data$working_data
      var_data <- df[[input$inspect_variable]]
      
      cat("Variable:", input$inspect_variable, "\n")
      cat("Type:", class(var_data)[1], "\n")
      cat("Classification:", classify_variable(var_data), "\n")
      cat("Length:", length(var_data), "\n")
      cat("Missing values:", sum(is.na(var_data)), "\n")
      cat("Unique values:", length(unique(var_data[!is.na(var_data)])), "\n\n")
      
      if(is.numeric(var_data)) {
        cat("Summary Statistics:\n")
        print(summary(var_data))
      } else {
        cat("Frequency Table:\n")
        print(table(var_data, useNA = "ifany"))
      }
    })
    
    # Check if variable is categorical
    output$is_categorical <- reactive({
      req(shared_data$working_data, input$inspect_variable)
      if(input$inspect_variable == "") return(FALSE)
      
      df <- shared_data$working_data
      var_data <- df[[input$inspect_variable]]
      classification <- classify_variable(var_data)
      return(classification %in% c("Categorical", "Ordinal/Categorical"))
    })
    outputOptions(output, 'is_categorical', suspendWhenHidden = FALSE)
    
    # Categorical analysis
    output$categorical_analysis <- renderUI({
      req(shared_data$working_data, input$inspect_variable)
      if(input$inspect_variable == "") return(NULL)
      
      df <- shared_data$working_data
      var_data <- df[[input$inspect_variable]]
      classification <- classify_variable(var_data)
      
      if(classification %in% c("Categorical", "Ordinal/Categorical")) {
        if(is.factor(var_data)) {
          levels_info <- levels(var_data)
          base_category <- levels_info[1]  # First level is typically base
          
          HTML(paste0(
            "<strong>Factor Analysis:</strong><br/>",
            "• Number of levels: ", length(levels_info), "<br/>",
            "• Levels: ", paste(levels_info, collapse = ", "), "<br/>",
            "• Base category: ", base_category, "<br/>",
            "• Factor type: ", if(is.ordered(var_data)) "Ordered" else "Unordered", "<br/><br/>",
            "<strong>Frequency Distribution:</strong><br/>",
            paste(sapply(levels_info, function(lev) {
              count <- sum(var_data == lev, na.rm = TRUE)
              pct <- round(count / sum(!is.na(var_data)) * 100, 1)
              paste0("• ", lev, ": ", count, " (", pct, "%)")
            }), collapse = "<br/>")
          ))
        } else {
          unique_vals <- unique(var_data[!is.na(var_data)])
          freq_table <- table(var_data, useNA = "ifany")
          
          HTML(paste0(
            "<strong>Categorical Analysis:</strong><br/>",
            "• Number of categories: ", length(unique_vals), "<br/>",
            "• Categories: ", paste(head(unique_vals, 10), collapse = ", "), 
            if(length(unique_vals) > 10) "..." else "", "<br/>",
            "• Most frequent: ", names(freq_table)[which.max(freq_table)], "<br/>",
            "• Least frequent: ", names(freq_table)[which.min(freq_table)], "<br/><br/>",
            "<strong>Note:</strong> Consider converting to factor for analysis"
          ))
        }
      }
    })
    
    # Data preview
    output$dataPreview <- DT::renderDataTable({
      req(shared_data$working_data)
      
      tryCatch({
        preview_data <- create_data_preview(shared_data$working_data)
        DT::datatable(preview_data$data, caption = preview_data$caption,
                      options = list(scrollX = TRUE, pageLength = 10, dom = 'lrtip'))
      }, error = function(e) {
        # Fallback: just show first few columns if there's an error
        df_simple <- shared_data$working_data[1:min(100, nrow(shared_data$working_data)), 1:min(5, ncol(shared_data$working_data))]
        DT::datatable(df_simple, caption = "Simplified view due to display issues",
                      options = list(scrollX = TRUE, pageLength = 10, dom = 'lrtip'))
      })
    })
  })
}

# ===================================================================
# END OF ENHANCED DATA UPLOAD MODULE
# ===================================================================