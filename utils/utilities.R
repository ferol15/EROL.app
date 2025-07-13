# ===================================================================
# UTILITIES MODULE: utils/utilities.R
# Common utility functions used across modules
# ===================================================================

# Function to add R code to log
add_r_code <- function(shared_data, code, description) {
  current_log <- shared_data$r_code_log
  timestamp <- format(Sys.time(), "%H:%M:%S")
  new_entry <- paste0("# ", description, " (", timestamp, ")\n", code, "\n")
  shared_data$r_code_log <- c(current_log, new_entry)
}

# Function to classify variable type
classify_variable <- function(x) {
  if(is.numeric(x)) {
    unique_vals <- length(unique(x[!is.na(x)]))
    if(unique_vals <= 10) return("Ordinal/Categorical") else return("Continuous")
  } else if(is.factor(x)) {
    if(length(levels(x)) <= 10) return("Categorical") else return("Ordinal")
  } else {
    return("Categorical")
  }
}

# Function to parse and apply conditions for variable recoding
apply_condition <- function(data, condition) {
  condition <- trimws(condition)
  if(grepl("^<=", condition)) {
    value_str <- gsub("<=", "", condition)
    if(value_str == "") return(rep(FALSE, length(data)))
    value <- as.numeric(value_str)
    if(is.na(value)) return(rep(FALSE, length(data)))
    return(data <= value & !is.na(data))
  } else if(grepl("^>=", condition)) {
    value_str <- gsub(">=", "", condition)
    if(value_str == "") return(rep(FALSE, length(data)))
    value <- as.numeric(value_str)
    if(is.na(value)) return(rep(FALSE, length(data)))
    return(data >= value & !is.na(data))
  } else if(grepl("^<", condition)) {
    value_str <- gsub("<", "", condition)
    if(value_str == "") return(rep(FALSE, length(data)))
    value <- as.numeric(value_str)
    if(is.na(value)) return(rep(FALSE, length(data)))
    return(data < value & !is.na(data))
  } else if(grepl("^>", condition)) {
    value_str <- gsub(">", "", condition)
    if(value_str == "") return(rep(FALSE, length(data)))
    value <- as.numeric(value_str)
    if(is.na(value)) return(rep(FALSE, length(data)))
    return(data > value & !is.na(data))
  } else if(grepl("^==", condition)) {
    value_str <- gsub("==", "", condition)
    if(value_str == "") return(rep(FALSE, length(data)))
    value <- as.numeric(value_str)
    if(is.na(value)) return(rep(FALSE, length(data)))
    return(data == value & !is.na(data))
  } else if(grepl("^!=", condition)) {
    value_str <- gsub("!=", "", condition)
    if(value_str == "") return(rep(FALSE, length(data)))
    value <- as.numeric(value_str)
    if(is.na(value)) return(rep(FALSE, length(data)))
    return(data != value & !is.na(data))
  }
  return(rep(FALSE, length(data)))
}

# Function to get variable choices for select inputs
get_variable_choices <- function(df, type = "all") {
  if(is.null(df)) return(c("No data loaded" = ""))
  
  all_vars <- names(df)
  
  switch(type,
         "all" = c("Select variables" = "", all_vars),
         "numeric" = {
           numeric_vars <- names(df)[sapply(df, function(x) is.numeric(x) && !all(is.na(x)))]
           c("Select variables" = "", numeric_vars)
         },
         "binary" = {
           binary_vars <- names(df)[sapply(df, function(x) {
             if(is.numeric(x)) {
               unique_vals <- unique(x[!is.na(x)])
               length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
             } else if(is.factor(x)) {
               length(levels(x)) == 2
             } else {
               FALSE
             }
           })]
           
           # Fallback: any two-level variables
           if(length(binary_vars) == 0) {
             binary_vars <- names(df)[sapply(df, function(x) {
               if(is.numeric(x)) {
                 unique_vals <- unique(x[!is.na(x)])
                 length(unique_vals) == 2
               } else if(is.factor(x)) {
                 length(levels(x)) == 2
               } else {
                 FALSE
               }
             })]
           }
           
           c("Select binary variable" = "", binary_vars)
         }
  )
}

# Function to load Stata files with multiple methods
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

# Function to create safe data preview
create_data_preview <- function(df, max_rows = 1000) {
  if(is.null(df)) return(NULL)
  
  if(nrow(df) > max_rows) {
    sample_size <- min(max_rows, nrow(df))
    set.seed(123)
    df_sample <- df[sample(nrow(df), sample_size), ]
    caption <- paste("Random sample of", sample_size, "rows from", nrow(df), "total")
  } else {
    df_sample <- df
    caption <- paste("All", nrow(df), "observations")
  }
  
  # Ensure all columns display safely
  df_display <- data.frame(lapply(df_sample, function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else if (is.numeric(x)) {
      round(x, 4)  # Round numeric values for display
    } else {
      as.character(x)
    }
  }), stringsAsFactors = FALSE)
  
  list(data = df_display, caption = caption)
}

# ===================================================================
# END OF UTILITIES MODULE
# ===================================================================