# ===================================================================
# HELP MODULE: modules/help_module.R
# Provides user guidance and documentation
# ===================================================================

# UI Function
help_UI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "help",
          fluidRow(
            box(title = "How to Use This App", status = "primary", solidHeader = TRUE, width = 12,
                h3("📚 Step-by-Step Guide"),
                tags$ol(
                  tags$li(strong("Upload Data:"), " Start by uploading a Stata (.dta) file in the 'Data Upload' tab."),
                  tags$li(strong("Create Variables:"), " Use the 'Variable Creation' tab to recode or create new measures."),
                  tags$li(strong("Explore Descriptives:"), " Examine variable distributions and summary statistics."),
                  tags$li(strong("Enhanced Visualization:"), " Create customized plots with proper variable type classification."),
                  tags$li(strong("Check Correlations:"), " Look for relationships between variables."),
                  tags$li(strong("Run Regressions:"), " Analyze relationships using OLS or logistic regression."),
                  tags$li(strong("Learn R Code:"), " Check the 'R Code Log' tab to see the R syntax for your actions."),
                  tags$li(strong("Interpret Results:"), " Use the interpretation boxes to understand your findings.")
                ),
                br(),
                h3("🔧 Variable Creation Guide"),
                h4("Numeric Recoding:"),
                tags$ul(
                  tags$li("Format: old_value=new_value OR condition=new_value"),
                  tags$li("Examples: 1=0, 2=1, 99= (empty = NA)"),
                  tags$li("Conditions: <=0=, >=999=, <18=0, >100=1"),
                  tags$li("Operators: <=, >=, <, >, ==, !="),
                  tags$li("Use empty value after = to set as missing"),
                  tags$li(strong("IMPORTANT:"), " For negative values like -9, -4, use: <0= to set all negative values as NA")
                ),
                h4("Missing Value Handling:"),
                tags$ul(
                  tags$li("Enter values to treat as missing: -99, -98, 999"),
                  tags$li("Choose to set as NA or replace with specific value")
                ),
                h4("Variable Types:"),
                tags$ul(
                  tags$li(strong("Continuous:"), " Numeric with many unique values (age, income)"),
                  tags$li(strong("Ordinal:"), " Ordered categories (education levels, agreement scales)"),
                  tags$li(strong("Categorical:"), " Unordered categories (gender, party affiliation)")
                ),
                br(),
                h3("📖 Statistical Interpretation Guide"),
                h4("Descriptive Statistics:"),
                tags$ul(
                  tags$li(strong("Mean:"), " Average value of the variable"),
                  tags$li(strong("Median:"), " Middle value when data is ordered"),
                  tags$li(strong("Standard Deviation:"), " Measure of variability around the mean"),
                  tags$li(strong("Skewness:"), " Measure of asymmetry in the distribution")
                ),
                h4("Correlation Coefficients:"),
                tags$ul(
                  tags$li(strong("r = 0.1 to 0.3:"), " Weak relationship"),
                  tags$li(strong("r = 0.3 to 0.5:"), " Moderate relationship"),
                  tags$li(strong("r = 0.5 to 1.0:"), " Strong relationship"),
                  tags$li(strong("Negative values:"), " Inverse relationship")
                ),
                h4("Regression Coefficients:"),
                tags$ul(
                  tags$li(strong("Coefficient:"), " Expected change in Y for 1-unit change in X"),
                  tags$li(strong("p-value < 0.05:"), " Statistically significant relationship"),
                  tags$li(strong("R-squared:"), " Proportion of variance explained by the model"),
                  tags$li(strong("Odds Ratio > 1:"), " Increases likelihood of outcome (logistic)")
                ),
                br(),
                h3("🎯 Tips for Large Datasets:"),
                tags$ul(
                  tags$li("Use sampling for datasets > 10,000 observations"),
                  tags$li("Start with descriptive statistics before running complex models"),
                  tags$li("Check correlation matrices to identify multicollinearity"),
                  tags$li("Examine diagnostic plots for assumption violations")
                ),
                br(),
                h3("⚠️ Troubleshooting Common Issues:"),
                h4("Data Loading Problems:"),
                tags$ul(
                  tags$li("Ensure your file is a valid .dta file"),
                  tags$li("Check file size (must be under 100MB)"),
                  tags$li("Try converting to .csv if .dta loading fails"),
                  tags$li("For ANES data: consider using a subset of variables")
                ),
                h4("Variable Creation Issues:"),
                tags$ul(
                  tags$li("Check that source variable exists in your dataset"),
                  tags$li("Verify recoding syntax (old_value=new_value)"),
                  tags$li("Use conditions carefully (<=, >=, <, >, ==, !=)"),
                  tags$li("Remember: empty value after = sets to NA")
                ),
                h4("Analysis Errors:"),
                tags$ul(
                  tags$li("Ensure you have enough observations for analysis"),
                  tags$li("Check for missing data in key variables"),
                  tags$li("For regression: make sure dependent variable is appropriate type"),
                  tags$li("For logistic regression: dependent variable should be binary (0/1)")
                )
            )
          )
  )
}

# Server Function
help_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
  })
}

# ===================================================================
# END OF HELP MODULE
# ===================================================================