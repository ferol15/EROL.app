description_content <- c(
  "Package: EROL-app",
  "Type: Package", 
  "Title: Experimental Research & Observational Learning (EROL) Application",
  "Version: 1.0.0",
  "Imports:",
  "    shiny,",
  "    shinydashboard,", 
  "    DT,",
  "    plotly,",
  "    haven,",
  "    foreign,",
  "    dplyr,",
  "    ggplot2,",
  "    corrplot,",
  "    broom,",
  "    knitr,",
  "    kableExtra,",
  "    fresh,",
  "    later,",
  "    psych,",
  "    GPArotation,",
  "    lavaan,",
  "    semPlot,",
  "    viridis,",
  "    networkD3,",
  "    igraph,",
  "    reshape2,",
  "    RColorBrewer,",
  "    lavaanPlot,",
  "    scales,",
  "    magrittr,",
  "    sandwich,",
  "    lmtest,",
  "    performance,",
  "    htmltools,",
  "    stats",
  "Encoding: UTF-8"
)

writeLines(description_content, "DESCRIPTION")

cat("DESCRIPTION file created successfully!\n")
cat("Contents:\n")
cat(paste(description_content, collapse = "\n"))
cat("\n\n")

if (file.exists("DESCRIPTION")) {
  cat("File exists and is", file.size("DESCRIPTION"), "bytes\n")
  
  tryCatch({
    test_read <- readLines("DESCRIPTION")
    cat("File reads without warnings - properly formatted!\n")
  }, warning = function(w) {
    cat("Warning detected:", w$message, "\n")
  })
} else {
  cat("Error: DESCRIPTION file was not created\n")
}

if (requireNamespace("desc", quietly = TRUE)) {
  cat("\nAlternative: Creating with desc package...\n")
  
  library(desc)
  
  desc_obj <- description$new("!new")
  
  desc_obj$set("Package", "Erol-app")
  desc_obj$set("Type", "Package")
  desc_obj$set("Title", "Experimental Research & Observational Learning (EROL) Application")
  desc_obj$set("Version", "1.0.0")
  desc_obj$set("Encoding", "UTF-8")
  
  imports <- c(
    "shiny", "shinydashboard", "DT", "plotly", "haven", "foreign", 
    "dplyr", "ggplot2", "corrplot", "broom", "knitr", "kableExtra", 
    "fresh", "later", "psych", "GPArotation", "lavaan", "semPlot", 
    "viridis", "networkD3", "igraph", "reshape2", "RColorBrewer", 
    "lavaanPlot", "scales", "magrittr", "sandwich", "lmtest", 
    "performance", "htmltools", "stats"
  )
  
  for (pkg in imports) {
    desc_obj$set_dep(pkg, "Imports")
  }
  
  desc_obj$write("DESCRIPTION_alt")
  cat("Alternative DESCRIPTION file created as 'DESCRIPTION_alt'\n")
} else {
  cat("\nNote: Install 'desc' package for alternative creation method:\n")
  cat("install.packages('desc')\n")
}

cat("\nTo use this file:\n")
cat("1. Run this script in your Shiny app directory\n") 
cat("2. The DESCRIPTION file will be created/overwritten\n")
cat("3. Deploy your app - packages will be automatically installed\n")