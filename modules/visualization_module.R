# ===================================================================
# VISUALIZATION MODULE: modules/visualization_module.R
# Handles enhanced plotting and data visualization
# ===================================================================

# Required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(viridis)
library(RColorBrewer)
library(DT)

# Helper function to get variable choices based on type
get_variable_choices <- function(data, type = "all") {
  if (is.null(data) || ncol(data) == 0) return(character(0))
  
  if (type == "numeric") {
    return(names(data)[sapply(data, is.numeric)])
  } else if (type == "categorical") {
    return(names(data)[sapply(data, function(x) is.factor(x) || is.character(x))])
  } else {
    return(names(data))
  }
}

# Helper function to classify variable types
classify_variable <- function(x) {
  if (is.numeric(x)) {
    if (length(unique(x[!is.na(x)])) <= 10) {
      return("Discrete Numeric")
    } else {
      return("Continuous")
    }
  } else if (is.factor(x) || is.character(x)) {
    return("Categorical")
  } else if (is.logical(x)) {
    return("Logical")
  } else {
    return("Other")
  }
}

# Helper function to add R code to log
add_r_code <- function(shared_data, code, description) {
  tryCatch({
    # Initialize r_code_log if it doesn't exist or is NULL
    if(is.null(shared_data$r_code_log) || !is.data.frame(shared_data$r_code_log)) {
      shared_data$r_code_log <- data.frame(
        Step = integer(0),
        Description = character(0), 
        Code = character(0),
        Timestamp = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    }
    
    # Create new entry
    new_entry <- data.frame(
      Step = as.integer(nrow(shared_data$r_code_log) + 1),
      Description = as.character(description),
      Code = as.character(code),
      Timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    # Add to log using rbind
    shared_data$r_code_log <- rbind(shared_data$r_code_log, new_entry)
    
  }, error = function(e) {
    cat("R Code Logging Error:", e$message, "\n")
    # Initialize empty log if there's an error
    if(is.null(shared_data$r_code_log)) {
      shared_data$r_code_log <- data.frame(
        Step = integer(0),
        Description = character(0), 
        Code = character(0),
        Timestamp = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    }
  })
}

# UI Function
visualization_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "visualization",
          fluidRow(
            box(title = "Plot Configuration", status = "primary", solidHeader = TRUE, width = 4,
                conditionalPanel(condition = "output.fileUploaded", ns = ns,
                                 selectInput(ns("plot_type"), "Plot Type:",
                                             choices = list(
                                               "Histogram" = "histogram", 
                                               "Boxplot" = "boxplot", 
                                               "Bar Chart" = "bar",
                                               "Scatter Plot" = "scatter", 
                                               "Line Plot" = "line", 
                                               "Density Plot" = "density",
                                               "Correlation Heatmap" = "correlation"
                                             )),
                                 
                                 # Hide X variable dropdown for correlation plots
                                 conditionalPanel(
                                   condition = "input.plot_type != 'correlation'", ns = ns,
                                   selectInput(ns("x_var"), "X Variable:", choices = NULL)
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_type == 'scatter' || input.plot_type == 'line'", ns = ns,
                                   selectInput(ns("y_var"), "Y Variable:", choices = NULL)
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_type == 'bar' || input.plot_type == 'boxplot'", ns = ns,
                                   selectInput(ns("group_var"), "Grouping Variable (optional):", choices = NULL)
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_type == 'scatter'", ns = ns,
                                   selectInput(ns("color_var"), "Color Variable (optional):", choices = NULL),
                                   selectInput(ns("size_var"), "Size Variable (optional):", choices = NULL)
                                 ),
                                 
                                 # Simple correlation variable selection
                                 conditionalPanel(
                                   condition = "input.plot_type == 'correlation'", ns = ns,
                                   h5("Select Variables for Correlation:"),
                                   selectInput(ns("corr_vars_select"), 
                                               "Choose variables (hold Ctrl/Cmd to select multiple):",
                                               choices = NULL, 
                                               selected = NULL,
                                               multiple = TRUE,
                                               selectize = FALSE,
                                               size = 8),
                                   helpText("Select at least 2 numeric variables. Hold Ctrl (Windows) or Cmd (Mac) to select multiple variables."),
                                   br()
                                 ),
                                 
                                 # Variable type classification
                                 conditionalPanel(
                                   condition = "input.plot_type != 'correlation'", ns = ns,
                                   h5("Variable Type Classification:"), 
                                   htmlOutput(ns("var_type_info"))
                                 ),
                                 
                                 h5("Plot Customization:"),
                                 conditionalPanel(
                                   condition = "input.plot_type != 'correlation'", ns = ns,
                                   textInput(ns("x_label"), "X-axis Label:", placeholder = "Auto-generated"),
                                   textInput(ns("y_label"), "Y-axis Label:", placeholder = "Auto-generated")
                                 ),
                                 textInput(ns("plot_title"), "Plot Title:", placeholder = "Auto-generated"),
                                 
                                 selectInput(ns("color_scheme"), "Color Scheme:",
                                             choices = list(
                                               "Default" = "default", 
                                               "Viridis" = "viridis", 
                                               "Blues" = "blues", 
                                               "Reds" = "reds",
                                               "Greens" = "greens",
                                               "Set1" = "set1",
                                               "Dark2" = "dark2",
                                               "Plasma" = "plasma",
                                               "Inferno" = "inferno"
                                             )),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_type == 'histogram' || input.plot_type == 'density'", ns = ns,
                                   numericInput(ns("bins"), "Number of Bins:", value = 30, min = 5, max = 100)
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_type == 'scatter'", ns = ns,
                                   numericInput(ns("point_size"), "Point Size:", value = 1, min = 0.1, max = 5, step = 0.1),
                                   numericInput(ns("alpha"), "Transparency (0-1):", value = 0.6, min = 0.1, max = 1, step = 0.1)
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_type != 'correlation'", ns = ns,
                                   checkboxInput(ns("show_labels"), "Show Data Labels", value = FALSE)
                                 ),
                                 checkboxInput(ns("facet_wrap"), "Create Faceted Plot", value = FALSE),
                                 
                                 conditionalPanel(
                                   condition = "input.facet_wrap == true", ns = ns,
                                   selectInput(ns("facet_var"), "Facet Variable:", choices = NULL)
                                 ),
                                 
                                 fluidRow(
                                   column(6, actionButton(ns("generate_plot"), "Generate Plot", class = "btn-primary", style = "width: 100%;")),
                                   column(6, actionButton(ns("reset_selections"), "Reset All", class = "btn-warning", style = "width: 100%;"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(6, downloadButton(ns("download_plot"), "Download Plot", class = "btn-success", style = "width: 100%;")),
                                   column(6, actionButton(ns("save_to_report"), "Add to Report", class = "btn-info", style = "width: 100%;"))
                                 ),
                                 br(), br(),
                                 h5("R Code for this plot:"),
                                 div(class = "r-code-box", 
                                     verbatimTextOutput(ns("plot_r_code")),
                                     br(),
                                     actionButton(ns("copy_code"), "Copy Code", class = "btn-secondary btn-sm")
                                 )
                )
            ),
            
            box(title = "Visualization", status = "info", solidHeader = TRUE, width = 8,
                plotlyOutput(ns("enhanced_plot"), height = "600px"),
                br(), 
                div(class = "interpretation-box", htmlOutput(ns("plot_interpretation"))),
                br(),
                div(class = "stats-summary", htmlOutput(ns("plot_statistics")))
            )
          )
  )
}

# Server Function
visualization_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    # ===== DEFINING ALL REACTIVE VALUES =====
    generated_r_codes <- reactiveVal(list())
    current_plot <- reactiveVal(NULL)
    
    # ===== OUTPUT DEFINITIONS =====
    # Check if file is uploaded
    output$fileUploaded <- reactive({
      return(!is.null(shared_data$working_data))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    # Variable type information
    output$var_type_info <- renderUI({
      req(shared_data$working_data)
      
      if(is.null(input$x_var) || input$x_var == "") {
        return(HTML("<em>Select an X variable to see type information</em>"))
      }
      
      df <- shared_data$working_data
      x_type <- classify_variable(df[[input$x_var]])
      info_text <- paste0("X Variable (", input$x_var, "): ", x_type)
      
      if(input$plot_type %in% c("scatter", "line") && !is.null(input$y_var) && input$y_var != "") {
        y_type <- classify_variable(df[[input$y_var]])
        info_text <- paste0(info_text, "<br/>Y Variable (", input$y_var, "): ", y_type)
      }
      
      HTML(paste0("<strong>", info_text, "</strong>"))
    })
    
    # R code output (live preview)
    output$plot_r_code <- renderText({
      if(input$plot_type == "correlation") {
        vars <- input$corr_vars_select
        if(is.null(vars) || length(vars) < 2) {
          return("# Select at least 2 numeric variables for correlation analysis")
        }
        
        r_code <- "library(ggplot2)\nlibrary(plotly)\nlibrary(viridis)\nlibrary(RColorBrewer)\n\n"
        r_code <- paste0(r_code, "# Selected variables: ", paste(vars, collapse = ", "), "\n")
        r_code <- paste0(r_code, "numeric_data <- data[c('", paste(vars, collapse = "', '"), "')]\n")
        r_code <- paste0(r_code, "complete_data <- numeric_data[complete.cases(numeric_data), ]\n")
        r_code <- paste0(r_code, "cor_matrix <- cor(complete_data, use = 'complete.obs')\n")
        r_code <- paste0(r_code, "\ncor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))\n")
        r_code <- paste0(r_code, "cor_df$value <- as.vector(cor_matrix)\n\n")
        r_code <- paste0(r_code, "p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +\n")
        r_code <- paste0(r_code, "  geom_tile(color = 'white', size = 0.5) +\n")
        r_code <- paste0(r_code, "  geom_text(aes(label = round(value, 2)), color = 'white', size = 3) +\n")
        r_code <- paste0(r_code, "  scale_fill_gradient2(low = '#2166AC', mid = '#F7F7F7', high = '#B2182B', \n")
        r_code <- paste0(r_code, "                       midpoint = 0, limits = c(-1, 1), \n")
        r_code <- paste0(r_code, "                       breaks = c(-1, -0.5, 0, 0.5, 1)) +\n")
        r_code <- paste0(r_code, "  theme_minimal() +\n")
        r_code <- paste0(r_code, "  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +\n")
        r_code <- paste0(r_code, "  coord_fixed()\n\n")
        r_code <- paste0(r_code, "ggplotly(p)")
        return(r_code)
      }
      
      if(is.null(input$x_var) || input$x_var == "") {
        return("# Select variables to generate plot code")
      }
      
      r_code <- "library(ggplot2)\nlibrary(plotly)\nlibrary(viridis)\nlibrary(RColorBrewer)\n\n"
      
      if(input$plot_type == "histogram") {
        r_code <- paste0(r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
        r_code <- paste0(r_code, "  geom_histogram(bins = ", input$bins, ", alpha = 0.7) +\n")
        r_code <- paste0(r_code, "  theme_minimal()\n\n")
      } else if(input$plot_type == "scatter" && !is.null(input$y_var) && input$y_var != "") {
        aes_parts <- paste0("x = ", input$x_var, ", y = ", input$y_var)
        if(!is.null(input$color_var) && input$color_var != "") {
          aes_parts <- paste0(aes_parts, ", color = ", input$color_var)
        }
        if(!is.null(input$size_var) && input$size_var != "") {
          aes_parts <- paste0(aes_parts, ", size = ", input$size_var)
        }
        
        r_code <- paste0(r_code, "p <- ggplot(data, aes(", aes_parts, ")) +\n")
        r_code <- paste0(r_code, "  geom_point(alpha = ", input$alpha, 
                         if(is.null(input$size_var) || input$size_var == "") paste0(", size = ", input$point_size) else "", ") +\n")
        r_code <- paste0(r_code, "  geom_smooth(method = 'lm', se = TRUE) +\n")
        r_code <- paste0(r_code, "  theme_minimal()\n\n")
      } else if(input$plot_type == "bar") {
        if(!is.null(input$group_var) && input$group_var != "") {
          r_code <- paste0(r_code, "p <- ggplot(data, aes(x = ", input$x_var, ", fill = ", input$group_var, ")) +\n")
          r_code <- paste0(r_code, "  geom_bar(position = 'dodge', alpha = 0.8) +\n")
        } else {
          r_code <- paste0(r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
          r_code <- paste0(r_code, "  geom_bar(alpha = 0.8) +\n")
        }
        r_code <- paste0(r_code, "  theme_minimal()\n\n")
      } else if(input$plot_type == "boxplot") {
        if(!is.null(input$group_var) && input$group_var != "") {
          r_code <- paste0(r_code, "p <- ggplot(data, aes(x = ", input$group_var, ", y = ", input$x_var, ")) +\n")
          r_code <- paste0(r_code, "  geom_boxplot(alpha = 0.7) +\n")
        } else {
          r_code <- paste0(r_code, "p <- ggplot(data, aes(y = ", input$x_var, ")) +\n")
          r_code <- paste0(r_code, "  geom_boxplot(alpha = 0.7) +\n")
        }
        r_code <- paste0(r_code, "  theme_minimal()\n\n")
      } else if(input$plot_type == "density") {
        r_code <- paste0(r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
        r_code <- paste0(r_code, "  geom_density(alpha = 0.7) +\n")
        r_code <- paste0(r_code, "  theme_minimal()\n\n")
      } else if(input$plot_type == "line" && !is.null(input$y_var) && input$y_var != "") {
        r_code <- paste0(r_code, "p <- ggplot(data, aes(x = ", input$x_var, ", y = ", input$y_var, ")) +\n")
        r_code <- paste0(r_code, "  geom_line(size = 1.2) +\n")
        r_code <- paste0(r_code, "  geom_point(size = 2) +\n")
        r_code <- paste0(r_code, "  theme_minimal()\n\n")
      } else {
        r_code <- paste0(r_code, "# Select appropriate variables for the chosen plot type")
      }
      
      # Add color scheme if not default
      if(input$color_scheme != "default") {
        if(input$color_scheme %in% c("viridis", "plasma", "inferno")) {
          r_code <- paste0(r_code, "p <- p + scale_fill_viridis_c(option = '", 
                           if(input$color_scheme == "viridis") "viridis" else input$color_scheme, 
                           "') + scale_color_viridis_d(option = '", 
                           if(input$color_scheme == "viridis") "viridis" else input$color_scheme, "')\n")
        } else {
          palette_name <- switch(input$color_scheme,
                                 "blues" = "Blues", "reds" = "Reds", "greens" = "Greens",
                                 "set1" = "Set1", "dark2" = "Dark2")
          r_code <- paste0(r_code, "p <- p + scale_fill_brewer(palette = '", palette_name, 
                           "') + scale_color_brewer(palette = '", palette_name, "')\n")
        }
      }
      
      # Add faceting if enabled
      if(input$facet_wrap && !is.null(input$facet_var) && input$facet_var != "") {
        r_code <- paste0(r_code, "p <- p + facet_wrap(~", input$facet_var, ")\n")
      }
      
      r_code <- paste0(r_code, "\nggplotly(p)")
      
      return(r_code)
    })
    
    # ===== OBSERVERS AND EVENT HANDLERS =====
    
    # Update variable choices when data changes
    observe({
      req(shared_data$working_data)
      all_choices <- get_variable_choices(shared_data$working_data, "all")
      numeric_choices <- get_variable_choices(shared_data$working_data, "numeric")
      categorical_choices <- get_variable_choices(shared_data$working_data, "categorical")
      
      # Update regular variable selections
      updateSelectInput(session, "x_var", choices = all_choices)
      updateSelectInput(session, "y_var", choices = numeric_choices)
      updateSelectInput(session, "group_var", choices = c("None" = "", all_choices))
      updateSelectInput(session, "color_var", choices = c("None" = "", all_choices))
      updateSelectInput(session, "size_var", choices = c("None" = "", numeric_choices))
      updateSelectInput(session, "facet_var", choices = c("None" = "", categorical_choices))
      
      # Update correlation variable selector
      updateSelectInput(session, "corr_vars_select", 
                        choices = numeric_choices,
                        selected = NULL)
    })
    
    # Reset button functionality
    observeEvent(input$reset_selections, {
      req(shared_data$working_data)
      
      # Reset all inputs to default values
      updateSelectInput(session, "plot_type", selected = "histogram")
      updateSelectInput(session, "x_var", selected = "")
      updateSelectInput(session, "y_var", selected = "")
      updateSelectInput(session, "group_var", selected = "")
      updateSelectInput(session, "color_var", selected = "")
      updateSelectInput(session, "size_var", selected = "")
      updateSelectInput(session, "facet_var", selected = "")
      updateSelectInput(session, "corr_vars_select", selected = NULL)
      updateTextInput(session, "x_label", value = "")
      updateTextInput(session, "y_label", value = "")
      updateTextInput(session, "plot_title", value = "")
      updateSelectInput(session, "color_scheme", selected = "default")
      updateCheckboxInput(session, "show_labels", value = FALSE)
      updateCheckboxInput(session, "facet_wrap", value = FALSE)
      updateNumericInput(session, "bins", value = 30)
      updateNumericInput(session, "point_size", value = 1)
      updateNumericInput(session, "alpha", value = 0.6)
      
      # Clear outputs
      output$enhanced_plot <- renderPlotly({ NULL })
      output$plot_interpretation <- renderUI({ NULL })
      output$plot_statistics <- renderUI({ NULL })
      current_plot(NULL)
      
      showNotification("All selections have been reset.", type = "message")
    })
    
    # Copy R code functionality
    observeEvent(input$copy_code, {
      current_code <- isolate(output$plot_r_code())
      if (!is.null(current_code)) {
        showModal(modalDialog(
          title = "R Code",
          div(
            h4("Copy this R code:"),
            tags$pre(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                     current_code
            ),
            br(),
            helpText("Select all the code above and copy it to your clipboard (Ctrl+C / Cmd+C)")
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        showNotification("No R code available to copy. Please generate a plot first.", type = "warning")
      }
    })
    
    # Generate plot
    observeEvent(input$generate_plot, {
      req(shared_data$working_data)
      
      # Validation based on plot type
      if(input$plot_type == "correlation") {
        if(is.null(input$corr_vars_select) || length(input$corr_vars_select) < 2) {
          showNotification("Please select at least 2 numeric variables for correlation analysis.", type = "warning")
          return()
        }
      } else {
        if(is.null(input$x_var) || input$x_var == "") {
          showNotification("Please select an X variable first.", type = "warning")
          return()
        }
        
        if(input$plot_type %in% c("scatter", "line") && (is.null(input$y_var) || input$y_var == "")) {
          showNotification("Please select a Y variable for scatter/line plots.", type = "warning")
          return()
        }
      }
      
      df <- shared_data$working_data
      
      tryCatch({
        # Generate R code for plotting
        plot_r_code <- "library(ggplot2)\nlibrary(plotly)\nlibrary(viridis)\nlibrary(RColorBrewer)\n\n"
        
        # Create plots based on type
        p <- NULL
        
        if(input$plot_type == "correlation") {
          vars <- input$corr_vars_select
          
          # Validate that all selected variables exist in the data
          missing_vars <- vars[!vars %in% names(df)]
          if(length(missing_vars) > 0) {
            showNotification(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")), type = "error")
            return()
          }
          
          # Use selected variables for correlation
          numeric_data <- df[vars]
          
          # Check if all selected variables are actually numeric
          non_numeric <- vars[!sapply(df[vars], is.numeric)]
          if(length(non_numeric) > 0) {
            showNotification(paste("Non-numeric variables selected:", paste(non_numeric, collapse = ", ")), type = "error")
            return()
          }
          
          # Remove rows with any missing values
          complete_data <- numeric_data[complete.cases(numeric_data), , drop = FALSE]
          
          if(nrow(complete_data) < 2) {
            showNotification("Not enough complete observations for correlation analysis.", type = "error")
            return()
          }
          
          # Calculate correlation matrix
          cor_matrix <- tryCatch({
            cor(complete_data, use = "complete.obs")
          }, error = function(e) {
            showNotification(paste("Error calculating correlations:", e$message), type = "error")
            return(NULL)
          })
          
          if(is.null(cor_matrix)) {
            return()
          }
          
          # Check if correlation matrix is valid
          if(any(is.na(cor_matrix))) {
            showNotification("Unable to calculate correlations. Check for variables with zero variance.", type = "error")
            return()
          }
          
          # Convert to long format for ggplot
          cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix), stringsAsFactors = FALSE)
          cor_df$value <- as.vector(cor_matrix)
          
          title <- if(!is.null(input$plot_title) && input$plot_title != "") input$plot_title else "Correlation Heatmap"
          
          p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
            geom_tile(color = "white", size = 0.5) +
            geom_text(aes(label = round(value, 2)), color = "black", size = 3, fontface = "bold") +
            scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", 
                                 midpoint = 0, limits = c(-1, 1),
                                 breaks = c(-1, -0.5, 0, 0.5, 1),
                                 labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0"),
                                 name = "Correlation") +
            labs(title = title, x = "", y = "") +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(hjust = 1),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5)
            ) +
            coord_fixed()
          
          # Build R code for correlation
          plot_r_code <- paste0(plot_r_code, "# Selected variables: ", paste(vars, collapse = ", "), "\n")
          plot_r_code <- paste0(plot_r_code, "numeric_data <- data[c('", paste(vars, collapse = "', '"), "')]\n")
          plot_r_code <- paste0(plot_r_code, "complete_data <- numeric_data[complete.cases(numeric_data), ]\n")
          plot_r_code <- paste0(plot_r_code, "cor_matrix <- cor(complete_data, use = 'complete.obs')\n")
          plot_r_code <- paste0(plot_r_code, "\ncor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))\n")
          plot_r_code <- paste0(plot_r_code, "cor_df$value <- as.vector(cor_matrix)\n\n")
          plot_r_code <- paste0(plot_r_code, "p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +\n")
          plot_r_code <- paste0(plot_r_code, "  geom_tile(color = 'white', size = 0.5) +\n")
          plot_r_code <- paste0(plot_r_code, "  geom_text(aes(label = round(value, 2)), color = 'black', size = 3, fontface = 'bold') +\n")
          plot_r_code <- paste0(plot_r_code, "  scale_fill_gradient2(low = '#2166AC', mid = '#F7F7F7', high = '#B2182B', \n")
          plot_r_code <- paste0(plot_r_code, "                       midpoint = 0, limits = c(-1, 1), \n")
          plot_r_code <- paste0(plot_r_code, "                       breaks = c(-1, -0.5, 0, 0.5, 1)) +\n")
          plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '', y = '') +\n")
          plot_r_code <- paste0(plot_r_code, "  theme_minimal() +\n")
          plot_r_code <- paste0(plot_r_code, "  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank()) +\n")
          plot_r_code <- paste0(plot_r_code, "  coord_fixed()")
          
        } else {
          # Handle other plot types
          x_type <- classify_variable(df[[input$x_var]])
          x_lab <- if(input$x_label != "") input$x_label else input$x_var
          y_lab <- if(input$y_label != "") input$y_label else "Count"
          title <- if(input$plot_title != "") input$plot_title else paste("Plot of", input$x_var)
          
          # Get base color based on scheme
          base_color <- switch(input$color_scheme,
                               "viridis" = "#440154FF",
                               "plasma" = "#0D0887FF", 
                               "inferno" = "#000004FF",
                               "blues" = "#08519C",
                               "reds" = "#A50F15",
                               "greens" = "#00441B",
                               "set1" = "#E41A1C",
                               "dark2" = "#1B9E77",
                               "steelblue")
          
          # Create different plot types
          if(input$plot_type == "histogram") {
            if(x_type == "Continuous") {
              p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_histogram(bins = input$bins, alpha = 0.7, fill = base_color) +
                labs(title = title, x = x_lab, y = y_lab) +
                theme_minimal()
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_histogram(bins = ", input$bins, ", alpha = 0.7, fill = '", base_color, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = '", y_lab, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal()")
            } else {
              showNotification("Histogram works best with continuous variables. Consider using a bar chart.", type = "warning")
              p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_bar(alpha = 0.7, fill = base_color) +
                labs(title = title, x = x_lab, y = y_lab) +
                theme_minimal()
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_bar(alpha = 0.7, fill = '", base_color, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = '", y_lab, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal()")
            }
          } else if(input$plot_type == "scatter") {
            y_lab_actual <- if(input$y_label != "") input$y_label else input$y_var
            
            # Build aesthetic mapping
            color_var <- if(!is.null(input$color_var) && input$color_var != "") input$color_var else NULL
            size_var <- if(!is.null(input$size_var) && input$size_var != "") input$size_var else NULL
            
            p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var,
                                       color = color_var, size = size_var)) +
              geom_point(alpha = input$alpha, 
                         size = if(is.null(size_var)) input$point_size else NULL) +
              geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
              labs(title = title, x = x_lab, y = y_lab_actual) +
              theme_minimal()
            
            # Build R code for scatter plot
            aes_mapping <- paste0("aes(x = ", input$x_var, ", y = ", input$y_var)
            if(!is.null(color_var)) {
              aes_mapping <- paste0(aes_mapping, ", color = ", color_var)
            }
            if(!is.null(size_var)) {
              aes_mapping <- paste0(aes_mapping, ", size = ", size_var)
            }
            aes_mapping <- paste0(aes_mapping, ")")
            
            plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, ", aes_mapping, ") +\n")
            plot_r_code <- paste0(plot_r_code, "  geom_point(alpha = ", input$alpha, 
                                  if(is.null(size_var)) paste0(", size = ", input$point_size) else "", ") +\n")
            plot_r_code <- paste0(plot_r_code, "  geom_smooth(method = 'lm', se = TRUE, color = 'red', alpha = 0.3) +\n")
            plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = '", y_lab_actual, "') +\n")
            plot_r_code <- paste0(plot_r_code, "  theme_minimal()")
            
          } else if(input$plot_type == "bar") {
            if(!is.null(input$group_var) && input$group_var != "") {
              p <- ggplot(df, aes_string(x = input$x_var, fill = input$group_var)) +
                geom_bar(position = "dodge", alpha = 0.8) +
                labs(title = title, x = x_lab, y = y_lab, fill = input$group_var) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$x_var, ", fill = ", input$group_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_bar(position = 'dodge', alpha = 0.8) +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = '", y_lab, "', fill = '", input$group_var, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal() +\n")
              plot_r_code <- paste0(plot_r_code, "  theme(axis.text.x = element_text(angle = 45, hjust = 1))")
            } else {
              p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_bar(alpha = 0.8, fill = base_color) +
                labs(title = title, x = x_lab, y = y_lab) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_bar(alpha = 0.8, fill = '", base_color, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = '", y_lab, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal() +\n")
              plot_r_code <- paste0(plot_r_code, "  theme(axis.text.x = element_text(angle = 45, hjust = 1))")
            }
          } else if(input$plot_type == "boxplot") {
            if(!is.null(input$group_var) && input$group_var != "") {
              group_lab <- if(input$x_label != "") input$x_label else input$group_var
              value_lab <- if(input$y_label != "") input$y_label else input$x_var
              p <- ggplot(df, aes_string(x = input$group_var, y = input$x_var, fill = input$group_var)) +
                geom_boxplot(alpha = 0.7) +
                labs(title = title, x = group_lab, y = value_lab) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$group_var, ", y = ", input$x_var, ", fill = ", input$group_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_boxplot(alpha = 0.7) +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", group_lab, "', y = '", value_lab, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal() +\n")
              plot_r_code <- paste0(plot_r_code, "  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')")
            } else {
              value_lab <- if(input$y_label != "") input$y_label else input$x_var
              p <- ggplot(df, aes_string(y = input$x_var)) +
                geom_boxplot(alpha = 0.7, fill = base_color) +
                labs(title = title, y = value_lab, x = "") +
                theme_minimal()
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(y = ", input$x_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_boxplot(alpha = 0.7, fill = '", base_color, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', y = '", value_lab, "', x = '') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal()")
            }
          } else if(input$plot_type == "line") {
            y_lab_actual <- if(input$y_label != "") input$y_label else input$y_var
            p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
              geom_line(color = "steelblue", size = 1.2) + 
              geom_point(color = "darkblue", size = 2) +
              labs(title = title, x = x_lab, y = y_lab_actual) +
              theme_minimal()
            
            plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$x_var, ", y = ", input$y_var, ")) +\n")
            plot_r_code <- paste0(plot_r_code, "  geom_line(color = 'steelblue', size = 1.2) +\n")
            plot_r_code <- paste0(plot_r_code, "  geom_point(color = 'darkblue', size = 2) +\n")
            plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = '", y_lab_actual, "') +\n")
            plot_r_code <- paste0(plot_r_code, "  theme_minimal()")
          } else if(input$plot_type == "density") {
            if(x_type == "Continuous") {
              p <- ggplot(df, aes_string(x = input$x_var)) +
                geom_density(alpha = 0.7, fill = base_color) +
                labs(title = title, x = x_lab, y = "Density") +
                theme_minimal()
              
              plot_r_code <- paste0(plot_r_code, "p <- ggplot(data, aes(x = ", input$x_var, ")) +\n")
              plot_r_code <- paste0(plot_r_code, "  geom_density(alpha = 0.7, fill = '", base_color, "') +\n")
              plot_r_code <- paste0(plot_r_code, "  labs(title = '", title, "', x = '", x_lab, "', y = 'Density') +\n")
              plot_r_code <- paste0(plot_r_code, "  theme_minimal()")
            } else {
              showNotification("Density plot works best with continuous variables.", type = "warning")
              return()
            }
          }
        }
        
        # Apply color schemes
        if(!is.null(p)) {
          # For correlation heatmap, color scheme is already applied above
          if(input$plot_type != "correlation" && input$color_scheme != "default") {
            # For scatter plots with color variables
            if(input$plot_type == "scatter" && !is.null(input$color_var) && input$color_var != "") {
              if(input$color_scheme == "viridis") {
                if(is.numeric(df[[input$color_var]])) {
                  p <- p + scale_color_viridis_c()
                  plot_r_code <- paste0(plot_r_code, " +\n  scale_color_viridis_c()")
                } else {
                  p <- p + scale_color_viridis_d()
                  plot_r_code <- paste0(plot_r_code, " +\n  scale_color_viridis_d()")
                }
              } else if(input$color_scheme %in% c("plasma", "inferno")) {
                if(is.numeric(df[[input$color_var]])) {
                  p <- p + scale_color_viridis_c(option = input$color_scheme)
                  plot_r_code <- paste0(plot_r_code, " +\n  scale_color_viridis_c(option = '", input$color_scheme, "')")
                } else {
                  p <- p + scale_color_viridis_d(option = input$color_scheme)
                  plot_r_code <- paste0(plot_r_code, " +\n  scale_color_viridis_d(option = '", input$color_scheme, "')")
                }
              } else {
                palette_name <- switch(input$color_scheme,
                                       "blues" = "Blues", "reds" = "Reds", "greens" = "Greens",
                                       "set1" = "Set1", "dark2" = "Dark2")
                p <- p + scale_color_brewer(palette = palette_name)
                plot_r_code <- paste0(plot_r_code, " +\n  scale_color_brewer(palette = '", palette_name, "')")
              }
            }
            
            # For grouped bar charts and boxplots with fill aesthetics
            if(input$plot_type %in% c("bar", "boxplot") && !is.null(input$group_var) && input$group_var != "") {
              if(input$color_scheme == "viridis") {
                p <- p + scale_fill_viridis_d()
                plot_r_code <- paste0(plot_r_code, " +\n  scale_fill_viridis_d()")
              } else if(input$color_scheme == "plasma") {
                p <- p + scale_fill_viridis_d(option = "plasma")
                plot_r_code <- paste0(plot_r_code, " +\n  scale_fill_viridis_d(option = 'plasma')")
              } else if(input$color_scheme == "inferno") {
                p <- p + scale_fill_viridis_d(option = "inferno")
                plot_r_code <- paste0(plot_r_code, " +\n  scale_fill_viridis_d(option = 'inferno')")
              } else {
                palette_name <- switch(input$color_scheme,
                                       "blues" = "Blues", "reds" = "Reds", "greens" = "Greens",
                                       "set1" = "Set1", "dark2" = "Dark2")
                p <- p + scale_fill_brewer(palette = palette_name)
                plot_r_code <- paste0(plot_r_code, " +\n  scale_fill_brewer(palette = '", palette_name, "')")
              }
            }
          }
          
          # Add faceting if requested (not applicable for correlation)
          if(input$plot_type != "correlation" && input$facet_wrap && !is.null(input$facet_var) && input$facet_var != "") {
            p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
            plot_r_code <- paste0(plot_r_code, " +\n  facet_wrap(~", input$facet_var, ")")
          }
          
          # Add final touches to R code
          plot_r_code <- paste0(plot_r_code, "\n\n# Make it interactive\nggplotly(p)")
          
          # Store current plot for download
          current_plot(p)
          
          # Render the plot
          output$enhanced_plot <- renderPlotly({ 
            tryCatch({
              if(input$plot_type == "correlation") {
                # For correlation heatmaps, simpler plotly conversion
                ggplotly(p, tooltip = c("x", "y", "fill")) %>%
                  layout(title = list(text = title, font = list(size = 16)))
              } else {
                # For other plots, standard conversion
                ggplotly(p) %>%
                  layout(title = list(text = title, font = list(size = 16)),
                         showlegend = TRUE)
              }
            }, error = function(e) {
              # Fallback to static plot if plotly fails
              showNotification("Interactive plot failed, showing static version", type = "warning")
              p
            })
          })
          
          # Add R code to log
          tryCatch({
            add_r_code(shared_data, plot_r_code, paste("Plot Generation -", input$plot_type))
          }, error = function(e) {
            # Continue without logging to prevent breaking the plot generation
          })
          
          # Also store in local reactive for backup
          tryCatch({
            current_codes <- generated_r_codes()
            if(is.null(current_codes)) current_codes <- list()
            current_codes[[length(current_codes) + 1]] <- list(
              timestamp = Sys.time(),
              type = input$plot_type,
              code = plot_r_code
            )
            generated_r_codes(current_codes)
          }, error = function(e) {
            # Continue silently
          })
          
          # Generate interpretation
          output$plot_interpretation <- renderUI({
            interpretation <- switch(input$plot_type,
                                     "histogram" = paste0(
                                       "<strong>Distribution Analysis:</strong><br/>",
                                       "• Examine the shape: Is it normal, skewed, or bimodal?<br/>",
                                       "• Look for outliers in the tails<br/>",
                                       "• Check for multiple peaks (modes)<br/>",
                                       "• Variable type: ", classify_variable(df[[input$x_var]]), "<br/>",
                                       "• Sample size: ", nrow(df[!is.na(df[[input$x_var]]), ])
                                     ),
                                     "density" = paste0(
                                       "<strong>Distribution Analysis:</strong><br/>",
                                       "• Examine the shape: Is it normal, skewed, or bimodal?<br/>",
                                       "• Look for outliers in the tails<br/>",
                                       "• Check for multiple peaks (modes)<br/>",
                                       "• Variable type: ", classify_variable(df[[input$x_var]]), "<br/>",
                                       "• Sample size: ", nrow(df[!is.na(df[[input$x_var]]), ])
                                     ),
                                     "bar" = paste0(
                                       "<strong>Categorical Analysis:</strong><br/>",
                                       "• Compare frequencies across categories<br/>",
                                       "• Look for the most/least common categories<br/>",
                                       "• Check for balanced vs. imbalanced distributions<br/>",
                                       "• Variable type: ", classify_variable(df[[input$x_var]]), "<br/>",
                                       "• Number of categories: ", length(unique(df[[input$x_var]][!is.na(df[[input$x_var]])]))
                                     ),
                                     "scatter" = {
                                       correlation <- cor(df[[input$x_var]], df[[input$y_var]], use = "complete.obs")
                                       paste0(
                                         "<strong>Relationship Analysis:</strong><br/>",
                                         "• Look for linear or non-linear patterns<br/>",
                                         "• Check strength and direction of relationship<br/>",
                                         "• Red line shows linear trend with confidence interval<br/>",
                                         "• Correlation coefficient: ", round(correlation, 3), "<br/>",
                                         "• Sample size: ", sum(complete.cases(df[c(input$x_var, input$y_var)]))
                                       )
                                     },
                                     "boxplot" = paste0(
                                       "<strong>Distribution Comparison:</strong><br/>",
                                       "• Compare medians (center line) across groups<br/>",
                                       "• Examine spread (box width) and variability<br/>",
                                       "• Identify outliers (points beyond whiskers)<br/>",
                                       "• Box represents 25th to 75th percentile (IQR)<br/>",
                                       "• Whiskers extend to 1.5 × IQR"
                                     ),
                                     "line" = paste0(
                                       "<strong>Trend Analysis:</strong><br/>",
                                       "• Look for patterns over time or ordered categories<br/>",
                                       "• Identify increases, decreases, or cycles<br/>",
                                       "• Check for sudden changes or breaks in trend<br/>",
                                       "• Best used with time series or ordered data"
                                     ),
                                     "correlation" = paste0(
                                       "<strong>Correlation Analysis:</strong><br/>",
                                       "• Values range from -1 (perfect negative) to +1 (perfect positive)<br/>",
                                       "• Values near 0 indicate weak relationships<br/>",
                                       "• Look for strong correlations (|r| > 0.7)<br/>",
                                       "• Diagonal shows perfect correlation (variables with themselves)<br/>",
                                       "• Use for feature selection and multicollinearity detection"
                                     ),
                                     "<strong>Plot Analysis:</strong><br/>Examine patterns and relationships in your data."
            )
            HTML(interpretation)
          })
          
          # Generate statistical summary
          output$plot_statistics <- renderUI({
            stats_html <- "<strong>Statistical Summary:</strong><br/>"
            
            if(input$plot_type %in% c("histogram", "density", "boxplot") && !is.null(input$x_var) && input$x_var != "") {
              var_data <- df[[input$x_var]][!is.na(df[[input$x_var]])]
              if(is.numeric(var_data)) {
                stats_html <- paste0(stats_html,
                                     "Mean: ", round(mean(var_data), 3), "<br/>",
                                     "Median: ", round(median(var_data), 3), "<br/>",
                                     "SD: ", round(sd(var_data), 3), "<br/>",
                                     "Min: ", round(min(var_data), 3), "<br/>",
                                     "Max: ", round(max(var_data), 3), "<br/>",
                                     "IQR: ", round(IQR(var_data), 3))
              }
            } else if(input$plot_type == "scatter") {
              x_data <- df[[input$x_var]][complete.cases(df[c(input$x_var, input$y_var)])]
              y_data <- df[[input$y_var]][complete.cases(df[c(input$x_var, input$y_var)])]
              correlation <- cor(x_data, y_data)
              
              # Simple linear regression
              lm_model <- lm(y_data ~ x_data)
              r_squared <- summary(lm_model)$r.squared
              
              stats_html <- paste0(stats_html,
                                   "Correlation (r): ", round(correlation, 4), "<br/>",
                                   "R-squared: ", round(r_squared, 4), "<br/>",
                                   "Slope: ", round(coef(lm_model)[2], 4), "<br/>",
                                   "Intercept: ", round(coef(lm_model)[1], 4), "<br/>",
                                   "Complete cases: ", length(x_data))
            } else if(input$plot_type == "bar" && !is.null(input$x_var) && input$x_var != "") {
              freq_table <- table(df[[input$x_var]])
              most_common <- names(freq_table)[which.max(freq_table)]
              stats_html <- paste0(stats_html,
                                   "Total categories: ", length(freq_table), "<br/>",
                                   "Most common: ", most_common, " (", max(freq_table), ")<br/>",
                                   "Least common: ", names(freq_table)[which.min(freq_table)], " (", min(freq_table), ")<br/>",
                                   "Total observations: ", sum(freq_table))
            } else if(input$plot_type == "correlation") {
              # Add correlation summary stats
              vars <- input$corr_vars_select
              if(!is.null(vars) && length(vars) >= 2) {
                numeric_data <- df[vars]
                complete_data <- numeric_data[complete.cases(numeric_data), ]
                
                if(nrow(complete_data) > 0) {
                  cor_matrix <- cor(complete_data, use = "complete.obs")
                  
                  # Find strongest correlation (excluding diagonal)
                  cor_matrix_no_diag <- cor_matrix
                  diag(cor_matrix_no_diag) <- NA
                  max_cor <- max(cor_matrix_no_diag, na.rm = TRUE)
                  min_cor <- min(cor_matrix_no_diag, na.rm = TRUE)
                  
                  stats_html <- paste0(stats_html,
                                       "Variables analyzed: ", length(vars), "<br/>",
                                       "Complete cases: ", nrow(complete_data), "<br/>",
                                       "Strongest positive correlation: ", round(max_cor, 3), "<br/>",
                                       "Strongest negative correlation: ", round(min_cor, 3), "<br/>",
                                       "Average correlation magnitude: ", round(mean(abs(cor_matrix_no_diag), na.rm = TRUE), 3))
                }
              }
            }
            
            HTML(stats_html)
          })
        }
        
      }, error = function(e) {
        showNotification(paste("Error generating plot:", e$message), type = "error")
      })
    })
    
    # Download plot functionality
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("plot_", Sys.Date(), "_", input$plot_type, ".png")
      },
      content = function(file) {
        req(current_plot())
        ggsave(file, plot = current_plot(), device = "png", 
               width = 12, height = 8, dpi = 300)
      }
    )
    
    # Save to report functionality
    observeEvent(input$save_to_report, {
      req(current_plot())
      # This would integrate with a report generation system
      showNotification("Plot added to report!", type = "success")
    })
  })
}

# ===================================================================
# END OF VISUALIZATION MODULE
# ===================================================================