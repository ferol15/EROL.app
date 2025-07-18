# ===================================================================
# MAIN APP FILE: app.R
# ===================================================================

# Source initialization first
source("modules/init.R")

# Source utility functions
source("utils/utilities.R")

# Source all module files
source("modules/data_upload_module.R")
source("modules/variable_creation_module.R")
source("modules/descriptive_stats_module.R")
source("modules/measurement_module.R")
source("modules/correlation_module.R")
source("modules/regression_module.R")
source("modules/visualization_module.R")
source("modules/r_code_log_module.R")
source("modules/help_module.R")

# Creating custom theme matching my website
library(fresh)

fatih_theme <- create_theme(
  adminlte_color(
    light_blue = "#c41e3a",    # Main red color from your site
    blue = "#a01729",          # Darker red for hover states
    navy = "#8b1423",          # Even darker red
    olive = "#c41e3a",         # Use red for consistency
    lime = "#28a745",          # Keep green for success messages
    orange = "#fd7e14",        # Keep orange for warnings
    red = "#dc3545",           # Standard red for errors
    purple = "#6f42c1",        # Keep purple
    maroon = "#a01729",        # Match your site's dark red
    fuchsia = "#e83e8c",       # Keep fuchsia
    yellow = "#ffc107",        # Keep yellow
    teal = "#20c997",          # Keep teal
    aqua = "#17a2b8"           # Keep aqua
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#2c2c2c",           # Dark sidebar like your site
    dark_hover_bg = "#c41e3a",     # Red hover effect
    dark_hover_color = "#ffffff",   # White text on hover
    dark_color = "#ffffff",         # White text in sidebar
    light_bg = "#f8f9fa"           # Light background option
  ),
  adminlte_global(
    content_bg = "#ffffff",        # Clean white background
    box_bg = "#ffffff", 
    info_box_bg = "#ffffff"
  )
)

# ===================================================================
# USER INTERFACE
# ===================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Fatih Erol"),
  
  dashboardSidebar(
    tags$div(
      style = "padding: 15px; color: white; text-align: center; border-bottom: 1px solid #444; margin-bottom: 10px; background-color: rgba(196, 30, 58, 0.3);",
      tags$div("EROL.app", style = "font-size: 16px; font-weight: 600; letter-spacing: 0.5px;"),
      tags$div(
        HTML("<strong>E</strong>xperimental <strong>R</strong>esearch & <strong>O</strong>bservational <strong>L</strong>earning (<strong>EROL</strong>) Application"), 
        style = "font-size: 13px; font-weight: 300; letter-spacing: 0.3px; margin-bottom: 8px;"
      ),
      tags$div("Version 2025-07-18", style = "font-size: 14px; font-weight: 500; font-style: italic; letter-spacing: 0.5px"),
      tags$div("⚠️ Several features are under development.", style = "font-size: 9px; font-weight: 400;"),
      tags$div(
        "Contact: ", 
        tags$a("ferol@arizona.edu", href = "mailto:ferol@arizona.edu", style = "color: #0066cc; text-decoration: none;"), 
        style = "font-size: 11px; margin-top: 5px;"
      )
    ),
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Variable Creation", tabName = "variables", icon = icon("tools")),
      menuItem("Descriptive Statistics", tabName = "descriptives", icon = icon("chart-bar")),
      menuItem("Measurement Analysis", tabName = "measurement", icon = icon("ruler-combined")), 
      menuItem("Correlations", tabName = "correlations", icon = icon("project-diagram")),
      menuItem("OLS Regression", tabName = "ols", icon = icon("line-chart")),
      menuItem("Logistic Regression", tabName = "logit", icon = icon("chart-line")),
      menuItem("Enhanced Visualization", tabName = "visualization", icon = icon("chart-area")),
      menuItem("R Code Log", tabName = "rcode", icon = icon("code")),
      menuItem("Help & Interpretation", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    
    # Apply the custom theme
    fresh::use_theme(fatih_theme),
    
    # Custom CSS styling to match website theme
    tags$head(
      tags$style(HTML("
        /* Header styling to match your site */
        .main-header .navbar {
          background-color: #c41e3a !important;
          border-bottom: none !important;
        }
        
        .main-header .logo {
          background-color: #c41e3a !important;
          color: #ffffff !important;
          font-weight: 600 !important;
          border-bottom: none !important;
        }
        
        .main-header .logo:hover {
          background-color: #a01729 !important;
        }
        
        /* Sidebar styling */
        .main-sidebar {
          background-color: #2c2c2c !important;
        }
        
        .sidebar-menu > li > a {
          color: #ffffff !important;
          font-weight: 400 !important;
        }
        
        .sidebar-menu > li:hover > a, .sidebar-menu > li.active > a {
          background-color: #c41e3a !important;
          color: #ffffff !important;
          border-left: 3px solid #ffffff !important;
        }
        
        /* Content area - clean and minimal like your site */
        .content-wrapper, .right-side { 
          background-color: #f8f9fa !important; 
        }
        
        /* Box styling - clean cards */
        .box {
          background-color: #ffffff !important;
          border: 2px solid #6c757d !important;
          border-radius: 8px !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
        }
        
        .box-header {
          background-color: #c41e3a !important;
          color: #ffffff !important;
          border-bottom: none !important;
          padding: 15px !important;
          border-radius: 8px 8px 0 0 !important;
        }
        
        .box-header h3 {
          color: #ffffff !important;
          font-weight: 600 !important;
        }
        
        .box-title {
          color: #ffffff !important;
          font-weight: 600 !important;
        }
        
        /* Fix specific box header colors */
        .box-primary > .box-header {
          background-color: #c41e3a !important;
          color: #ffffff !important;
        }
        
        .box-info > .box-header {
          background-color: #c41e3a !important;
          color: #ffffff !important;
        }
        
        .box-warning > .box-header {
          background-color: #c41e3a !important;
          color: #ffffff !important;
        }
        
        .box-success > .box-header {
          background-color: #c41e3a !important;
          color: #ffffff !important;
        }
        
        /* Custom colored boxes to match your site */
        .interpretation-box {
          background-color: #ffffff !important; 
          border-left: 4px solid #c41e3a !important;
          border-radius: 4px !important; 
          padding: 15px !important; 
          margin: 10px 0 !important;
          color: #2c2c2c !important;
        }
        
        .warning-box {
          background-color: #ffffff !important; 
          border-left: 4px solid #ffc107 !important;
          border-radius: 4px !important; 
          padding: 15px !important; 
          margin: 10px 0 !important;
          color: #856404 !important;
        }
        
        .success-box {
          background-color: #ffffff !important; 
          border-left: 4px solid #28a745 !important;
          border-radius: 4px !important; 
          padding: 15px !important; 
          margin: 10px 0 !important;
          color: #155724 !important;
        }
        
        .created-var { 
          background-color: #ffffff !important; 
          font-weight: bold !important; 
          color: #155724 !important;
          border-left: 3px solid #28a745 !important;
        }
        
        .source-var { 
          background-color: #ffffff !important; 
          font-weight: bold !important; 
          color: #856404 !important;
          border-left: 3px solid #ffc107 !important;
        }
        
        .r-code-box {
          background-color: #f8f9fa !important; 
          border: 2px solid #6c757d !important;
          border-radius: 6px !important; 
          padding: 12px !important; 
          margin: 10px 0 !important;
          font-family: 'Monaco', 'Consolas', 'Courier New', monospace !important; 
          font-size: 13px !important;
          color: #2c2c2c !important;
        }
        
        /* Button styling to match your site */
        .btn-primary {
          background-color: #c41e3a !important;
          border-color: #c41e3a !important;
          color: #ffffff !important;
        }
        
        .btn-primary:hover {
          background-color: #a01729 !important;
          border-color: #a01729 !important;
        }
        
        /* Clean typography */
        h1, h2, h3, h4, h5, h6 {
          color: #2c2c2c;
          font-weight: 600;
        }
        
        /* Tab styling */
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top: 3px solid #c41e3a;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          color: #c41e3a;
        }
      "))
    ),
    
    # Tab items using module UIs
    tabItems(
      data_upload_UI("data_upload"),
      variable_creation_UI("var_creation"),
      descriptive_stats_UI("descriptives"),
      correlation_UI("correlations"),
      regression_UI("ols", "OLS Regression"),
      regression_UI("logit", "Logistic Regression"),
      measurement_UI("measurement"),  # New measurement module UI
      visualization_UI("visualization"),
      r_code_log_UI("r_code"),
      help_UI("help")
    )
  )
)

# ===================================================================
# SERVER LOGIC
# ===================================================================

server <- function(input, output, session) {
  
  # Shared reactive values accessible by all modules
  shared_data <- reactiveValues(
    raw_data = NULL,           # Original uploaded data
    working_data = NULL,       # Current working dataset (may be sampled)
    created_vars = data.frame( # Track created variables
      Variable = character(),
      Source = character(),
      Type = character(),
      Created = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ),
    r_code_log = character()   # Store all R code generated
  )
  
  # Global reactive for file upload status (used by conditionalPanels)
  output$fileUploaded <- reactive({ 
    return(!is.null(shared_data$raw_data)) 
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Call all module servers with shared data
  data_upload_server("data_upload", shared_data)
  variable_creation_server("var_creation", shared_data)
  descriptive_stats_server("descriptives", shared_data)
  measurement_server("measurement", shared_data)
  correlation_server("correlations", shared_data)
  regression_server("ols", shared_data, type = "ols")
  regression_server("logit", shared_data, type = "logit")
  visualization_server("visualization", shared_data)
  r_code_log_server("r_code", shared_data)
  help_server("help", shared_data)
  
  session$onSessionEnded(function() {
    cat("Session ended. Cleaning up...\n")
  })
}

# ===================================================================
# RUN THE APPLICATION
# ===================================================================

# Print startup message
cat("========================================\n")
cat("EROL.app\n")
cat("Modular Shiny Application with Measurement Analysis\n")
cat("========================================\n")
cat("Starting application...\n")

# Launch the Shiny app
shinyApp(ui = ui, server = server)

# ===================================================================
# END OF MAIN APPLICATION FILE
# ===================================================================