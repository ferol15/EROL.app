# ===================================================================
# R CODE LOG MODULE: modules/r_code_log_module.R
# FIXED VERSION - Compatible with Variable Creation Module
# Tracks and displays R code equivalent of user actions
# ===================================================================

# UI Function
r_code_log_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "rcode",
          fluidRow(
            box(title = "R Code Log - Learn the Syntax!", status = "primary", solidHeader = TRUE, width = 12,
                helpText("This tab shows all the R code equivalent to your point-and-click actions. Copy and paste to learn R syntax!"),
                
                # Add refresh button and clear button
                div(style = "margin-bottom: 15px;",
                    actionButton(ns("refresh_code"), "Refresh Code", class = "btn-info btn-sm"),
                    actionButton(ns("clear_log"), "Clear Log", class = "btn-warning btn-sm", style = "margin-left: 10px;")
                ),
                
                h4("Complete R Script:"),
                div(style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; font-family: 'Courier New', monospace; max-height: 400px; overflow-y: auto;",
                    verbatimTextOutput(ns("full_r_code"))
                ),
                br(),
                
                fluidRow(
                  column(6,
                         downloadButton(ns("download_r_script"), "Download R Script", class = "btn-success")
                  ),
                  column(6,
                         downloadButton(ns("download_csv"), "Download Current Dataset", class = "btn-info")
                  )
                ),
                
                br(), br(),
                h4("Session History:"),
                DT::dataTableOutput(ns("r_code_history"))
            )
          )
  )
}

# Server Function
r_code_log_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to track code changes
    values <- reactiveValues(
      last_update = Sys.time()
    )
    
    # Full R code output - FIXED to work with data frame structure
    output$full_r_code <- renderText({
      # Trigger reactive dependency
      values$last_update
      
      if(is.null(shared_data$r_code_log) || nrow(shared_data$r_code_log) == 0) {
        return("# No R code generated yet. Start by uploading data and performing analyses.\n\n# Load your data:\n# data <- read.csv('your_file.csv')\n\n# Your analysis steps will appear here...")
      }
      
      # Build complete R script from log entries
      r_script <- c()
      
      # Add header
      r_script <- c(r_script, 
                    "# ===================================================================",
                    paste("# AUTO-GENERATED R SCRIPT -", Sys.Date()),
                    "# Generated from your point-and-click analysis",
                    "# ===================================================================",
                    "",
                    "# Load required libraries",
                    "library(dplyr)",
                    "library(ggplot2)",
                    "",
                    "# Load your data (replace with your file path)",
                    "# data <- read.csv('your_file.csv')",
                    "",
                    "# ===================================================================",
                    "# ANALYSIS STEPS",
                    "# ==================================================================="
      )
      
      # Add each code entry from the log
      for(i in 1:nrow(shared_data$r_code_log)) {
        entry <- shared_data$r_code_log[i, ]
        
        r_script <- c(r_script, 
                      "",
                      paste("#", entry$Step, "-", entry$Description),
                      paste("# Generated on:", format(entry$Timestamp, "%Y-%m-%d %H:%M:%S")),
                      entry$Code,
                      ""
        )
      }
      
      # Add footer
      r_script <- c(r_script,
                    "# ===================================================================",
                    "# END OF GENERATED SCRIPT",
                    "# ===================================================================",
                    "",
                    "# View your data",
                    "# head(data)",
                    "# summary(data)",
                    "",
                    "# Save your modified data",
                    "# write.csv(data, 'modified_data.csv', row.names = FALSE)"
      )
      
      return(paste(r_script, collapse = "\n"))
    })
    
    # R code history table - FIXED to work with data frame structure
    output$r_code_history <- DT::renderDataTable({
      # Trigger reactive dependency
      values$last_update
      
      if(is.null(shared_data$r_code_log) || nrow(shared_data$r_code_log) == 0) {
        return(DT::datatable(
          data.frame(Message = "No actions performed yet. Start by uploading data and creating variables."), 
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Create display dataframe
      history_df <- data.frame(
        Step = shared_data$r_code_log$Step,
        Description = shared_data$r_code_log$Description,
        Time = format(shared_data$r_code_log$Timestamp, "%H:%M:%S"),
        Date = format(shared_data$r_code_log$Timestamp, "%Y-%m-%d"),
        Code_Preview = sapply(shared_data$r_code_log$Code, function(code) {
          # Get first meaningful line of code (not comments)
          lines <- strsplit(code, "\n")[[1]]
          lines <- trimws(lines)
          lines <- lines[lines != "" & !grepl("^#", lines)]
          if(length(lines) > 0) {
            first_line <- lines[1]
            if(nchar(first_line) > 50) {
              paste0(substr(first_line, 1, 47), "...")
            } else {
              first_line
            }
          } else {
            "No code"
          }
        }),
        stringsAsFactors = FALSE
      )
      
      # Create datatable with better formatting
      dt <- DT::datatable(
        history_df, 
        options = list(
          pageLength = 15, 
          dom = 'frtip',
          scrollX = TRUE,
          columnDefs = list(
            list(width = '80px', targets = 0),   # Step
            list(width = '200px', targets = 1),  # Description
            list(width = '80px', targets = 2),   # Time
            list(width = '100px', targets = 3),  # Date
            list(width = '300px', targets = 4)   # Code Preview
          )
        ),
        caption = "History of all R code generated by your actions",
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
      
      return(dt)
    })
    
    # Refresh button
    observeEvent(input$refresh_code, {
      values$last_update <- Sys.time()
      showNotification("R code log refreshed", type = "message")
    })
    
    # Clear log button
    observeEvent(input$clear_log, {
      shared_data$r_code_log <- data.frame(
        Step = character(0),
        Description = character(0), 
        Code = character(0),
        Timestamp = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
      values$last_update <- Sys.time()
      showNotification("R code log cleared", type = "warning")
    })
    
    # Download R script - FIXED
    output$download_r_script <- downloadHandler(
      filename = function() { 
        paste0("analysis_script_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R") 
      },
      content = function(file) {
        if(is.null(shared_data$r_code_log) || nrow(shared_data$r_code_log) == 0) {
          writeLines(c(
            "# No R code generated yet",
            "# Upload data and perform analyses to generate R code",
            "",
            "# Load your data:",
            "# data <- read.csv('your_file.csv')",
            "",
            "# Your analysis steps will be saved here automatically"
          ), file)
        } else {
          # Get the full R code from the output
          full_code <- isolate({
            if(is.null(shared_data$r_code_log) || nrow(shared_data$r_code_log) == 0) {
              return("# No R code generated yet")
            }
            
            r_script <- c()
            
            # Add header
            r_script <- c(r_script, 
                          "# ===================================================================",
                          paste("# AUTO-GENERATED R SCRIPT -", Sys.Date()),
                          "# Generated from your point-and-click analysis",
                          "# ===================================================================",
                          "",
                          "# Load required libraries",
                          "library(dplyr)",
                          "library(ggplot2)",
                          "",
                          "# Load your data (replace with your file path)",
                          "# data <- read.csv('your_file.csv')",
                          "",
                          "# ===================================================================",
                          "# ANALYSIS STEPS",
                          "# ==================================================================="
            )
            
            # Add each code entry
            for(i in 1:nrow(shared_data$r_code_log)) {
              entry <- shared_data$r_code_log[i, ]
              
              r_script <- c(r_script, 
                            "",
                            paste("#", entry$Step, "-", entry$Description),
                            paste("# Generated on:", format(entry$Timestamp, "%Y-%m-%d %H:%M:%S")),
                            entry$Code,
                            ""
              )
            }
            
            # Add footer
            r_script <- c(r_script,
                          "# ===================================================================",
                          "# END OF GENERATED SCRIPT",
                          "# ===================================================================",
                          "",
                          "# View your data",
                          "# head(data)",
                          "# summary(data)",
                          "",
                          "# Save your modified data",
                          "# write.csv(data, 'modified_data.csv', row.names = FALSE)"
            )
            
            return(r_script)
          })
          
          writeLines(full_code, file)
        }
      },
      contentType = "text/plain"
    )
    
    # Download current dataset
    output$download_csv <- downloadHandler(
      filename = function() { 
        paste0("current_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv") 
      },
      content = function(file) {
        if(!is.null(shared_data$working_data)) {
          write.csv(shared_data$working_data, file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
        }
      },
      contentType = "text/csv"
    )
    
    # Auto-refresh when new code is added (reactive to shared_data changes)
    observe({
      req(shared_data$r_code_log)
      values$last_update <- Sys.time()
    })
  })
}

# ===================================================================
# END OF FIXED R CODE LOG MODULE
# ===================================================================