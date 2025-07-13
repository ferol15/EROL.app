# ===================================================================
# INITIALIZATION MODULE: modules/init.R
# Handles package installation and loading
# ===================================================================

# Required packages
packages <- c("shiny", "shinydashboard", "DT", "plotly", 
              "corrplot", "haven", "dplyr", "ggplot2", 
              "broom", "knitr", "kableExtra", "foreign", "fresh", "later")

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  cat("Installing missing packages...\n")
  install.packages(packages[!installed_packages])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Set options
options(shiny.maxRequestSize = 100*1024^2)  # 100MB file upload limit

cat("All packages loaded successfully!\n")
cat("File upload limit set to 100MB\n")

# Create a custom theme
fatih_theme <- create_theme(
  adminlte_color(
    light_blue = "#c41e3a",    
    blue = "#a01729",          
    navy = "#8b1423",         
    olive = "#c41e3a",        
    lime = "#28a745",         
    orange = "#fd7e14",       
    red = "#dc3545",          
    purple = "#6f42c1",        
    maroon = "#a01729",       
    fuchsia = "#e83e8c",       
    yellow = "#ffc107",        
    teal = "#20c997",          
    aqua = "#17a2b8"          
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#2c2c2c",          
    dark_hover_bg = "#c41e3a",     
    dark_hover_color = "#ffffff",   
    dark_color = "#ffffff",         
    light_bg = "#f8f9fa"           
  ),
  adminlte_global(
    content_bg = "#ffffff",     
    box_bg = "#ffffff", 
    info_box_bg = "#ffffff"
  )
)

# ===================================================================
# END OF INIT MODULE
# ===================================================================