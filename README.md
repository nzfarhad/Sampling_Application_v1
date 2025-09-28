# Sampling Application

A R Shiny application for sampling methodologies and sample size calculation

## Features

### Decision Tree
Interactive wizard that guides users through sampling method selection based on study design and constraints.

### Sample Calculator
Comprehensive calculators for different sampling methods with visualizations and downloadable reports.

### Sampling Tool
Under development

### Sampling guideline
Comprehensive wiki with sampling fundamentals, methodologies, and best practices.

## Quick Start

1. **Install R and required packages**:
   ```r
   install.packages(c("shiny", "bslib", "shinyjs", "DT", "plotly", "ggplot2", 
                      "dplyr", "readr", "readxl", "writexl", "openxlsx", 
                      "rmarkdown", "knitr", "htmltools", "plyr", "car", 
                      "reshape2", "stringr", "crayon"))
   ```

2. **Run the application**:
   ```r
   shiny::runApp("app.R")
   ```

3. **Start with the Decision Tree** to choose your sampling method

## Sampling Methods Supported

- **Simple Random Sampling**: Equal probability selection
- **Stratified Sampling**: Population divided into strata
- **Cluster Sampling**: Groups selected randomly
- **Two-Stage Random Sampling**: Clusters then units
- **Systematic Sampling**: Regular interval selection

## Sample Size Calculations

The application provides sample size calculations for:
- Proportion estimation
- Mean estimation
- Finite population corrections
- Design effects for complex sampling
- Confidence intervals and margins of error

## Data Formats Supported

- CSV files
- Excel files (.xlsx, .xls)

## Export Options
- Excel workbooks
- PDF reports


## Requirements

- R 4.0 or higher
- Modern web browser
- Required R packages (see Quick Start)

## Usage

### 1. Decision Tree
Answer questions about your study design to get method recommendations.

### 2. Sample Calculator
Input study parameters to calculate required sample sizes with visualizations.

### 3. Sampling Tool
Upload data and implement sampling using the recommended methodology.

### 4. Sampling guideline
Learn about sampling concepts, methods, and best practices.

## Contributing

This application is designed to be modular and extensible. New sampling methods can be added by:
1. Adding functions to existing modules
2. Creating new modules in the `modules/` directory
3. Adding documentation to the `wiki_content/` directory

## License

This project is open source and available under the MIT License.

## Support

For questions and support, please refer to the Wiki section in the application or create an issue in the project repository.

