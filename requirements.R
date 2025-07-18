packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "haven",
  "foreign",
  "dplyr",
  "ggplot2",
  "corrplot",
  "broom",
  "knitr",
  "kableExtra",
  "fresh",
  "later",
  "psych",
  "GPArotation",
  "lavaan",
  "semPlot",
  "viridis",
  "networkD3",
  "igraph",
  "reshape2",
  "RColorBrewer",
  "lavaanPlot",
  "scales",
  "magrittr",
  "sandwich",
  "lmtest",
  "performance",
  "htmltools",
  "stats"
)

install.packages(packages, dependencies = TRUE)

cat("All packages installed successfully!\n")
cat("Installed packages:\n")
cat(paste(packages, collapse = "\n"))
cat("\n")