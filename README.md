---
title: Experimental Research & Observational Learning Application
---

# EROL.app - Experimental Research & Observational Learning Application

A comprehensive statistical analysis application built with R Shiny, designed for experimental research and observational learning in political science and social sciences.

## Features

### Core Data Analysis
- **Data Upload**: Support for CSV, Excel, SPSS, Stata, and SAS files
- **Variable Creation**: Interactive variable transformation and creation tools
- **Descriptive Statistics**: Comprehensive statistical summaries and distributions
- **Correlation Analysis**: Multiple correlation methods with advanced visualizations
- **Regression Analysis**: Both OLS and Logistic regression with interpretation

### Advanced Measurement Analysis
- **Reliability Analysis**: 
  - Cronbach's Alpha with item statistics and diagnostics
  - McDonald's Omega for more accurate reliability estimation
  - Interactive reliability improvement recommendations
- **Exploratory Factor Analysis (EFA)**:
  - Kaiser-Meyer-Olkin and Bartlett's tests for suitability
  - Multiple rotation methods (oblimin, varimax, promax)
  - Scree plots and factor loading visualizations
  - Parallel analysis for optimal factor extraction
- **Confirmatory Factor Analysis (CFA)**:
  - lavaan syntax support for model specification
  - Comprehensive fit indices (RMSEA, CFI, TLI, SRMR)
  - Modification indices for model improvement
  - Path diagrams with standardized loadings
  - Model comparison capabilities
- **Measurement Invariance Testing**:
  - Configural, metric, scalar, and strict invariance levels
  - Chi-square difference tests and practical significance assessment
  - Multi-group CFA with detailed interpretation
- **Item Analysis**:
  - Classical Test Theory metrics
  - Item difficulty, discrimination, and item-total correlations
  - Response distribution analysis
  - Item quality diagnostics and recommendations

### Visualization & Output
- **Enhanced Visualization**: Interactive plots with ggplot2 and plotly
- **R Code Log**: Track and export all generated R code for reproducibility
- **Help & Interpretation**: Built-in statistical interpretation guides
- **Professional Reports**: Comprehensive analysis summaries with methodological guidance

## Usage

1. **Upload your data** using the Data Upload tab
2. **Create new variables** if needed using the Variable Creation tools
3. **Explore descriptive statistics** to understand your data
4. **Run correlation analyses** to examine relationships
5. **Perform regression analyses** for hypothesis testing
6. **Conduct measurement analyses** for scale development and validation:
   - Test reliability using Alpha or Omega
   - Explore factor structure with EFA
   - Confirm theoretical models with CFA
   - Test measurement invariance across groups
   - Analyze individual item performance
7. **Create visualizations** to communicate your findings
8. **Export R code** to reproduce your analysis

## Supported File Formats

- CSV (.csv)
- Excel (.xlsx, .xls)
- SPSS (.sav)
- Stata (.dta)
- SAS (.sas7bdat)

## Statistical Packages Used

### Core Analysis
- **Data Manipulation**: dplyr, tidyr
- **Statistical Analysis**: psych, broom
- **Correlation**: corrplot, qgraph
- **Regression**: stats, car

### Measurement Analysis
- **Reliability**: psych (alpha, omega)
- **Factor Analysis**: psych, GPArotation
- **Structural Equation Modeling**: lavaan, semPlot, lavaanPlot
- **Measurement Invariance**: lavaan (multi-group analysis)

### Visualization
- **Static Plots**: ggplot2, corrplot
- **Interactive Plots**: plotly, networkD3
- **Tables**: DT, kableExtra
- **Colors**: viridis, RColorBrewer

### Interface
- **Dashboard**: shiny, shinydashboard
- **File I/O**: readr, readxl, haven, foreign

## Key Methodological Features

- **Comprehensive Diagnostics**: Automatic assessment of analysis assumptions
- **Effect Size Reporting**: Beyond p-values to practical significance
- **Multiple Comparison Corrections**: When appropriate for family-wise error control
- **Bootstrapped Confidence Intervals**: Robust estimation for key parameters
- **Missing Data Handling**: Listwise deletion with diagnostic information
- **Reproducible Research**: Complete R code generation for all analyses

## Educational Focus

This application emphasizes:
- **Methodological Understanding**: Detailed explanations of statistical concepts
- **Best Practices**: Guidelines for appropriate statistical analysis choices
- **Interpretation Skills**: Comprehensive result interpretation with practical implications
- **Reproducibility**: Full documentation of analytical decisions and code

## Target Users

- **Students**: Learning statistical analysis in political science and social sciences
- **Researchers**: Conducting scale development, validation, and empirical analysis
- **Instructors**: Teaching statistical methods with real-time examples
- **Practitioners**: Applying rigorous measurement and analytical techniques

## Contact

Developed by Fatih Erol  
Email: ferol@arizona.edu

---

*This application is designed for educational and research purposes in political science and social sciences.*
