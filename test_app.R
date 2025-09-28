# Test script for the Sampling Application
# This script tests the basic functionality of the application

# Load required libraries
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(htmltools)
library(plyr)
library(car)
library(reshape2)
library(stringr)
library(crayon)

# Test if all required packages are available
required_packages <- c("shiny", "bslib", "shinyjs", "DT", "plotly", "ggplot2", 
                       "dplyr", "readr", "readxl", "writexl", "openxlsx", 
                       "rmarkdown", "knitr", "htmltools", "plyr", "car", 
                       "reshape2", "stringr", "crayon")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Please install them using: install.packages(c(", 
      paste0('"', missing_packages, '"', collapse = ", "), "))\n")
} else {
  cat("All required packages are available.\n")
}

# Test if global functions can be loaded
if (file.exists("global.R")) {
  source("global.R")
  cat("Global functions loaded successfully.\n")
} else {
  cat("Warning: global.R not found.\n")
}

# Sampling functions are now integrated into modules
cat("Sampling functions are integrated into modules.\n")

# Test if modules can be loaded
module_files <- c("modules/decision_tree_module.R", 
                  "modules/sample_calculator_module.R", 
                  "modules/wiki_module.R")

for (module_file in module_files) {
  if (file.exists(module_file)) {
    source(module_file)
    cat("Module", module_file, "loaded successfully.\n")
  } else {
    cat("Warning:", module_file, "not found.\n")
  }
}

# Test sample size calculation functions
test_sample_size_calculations <- function() {
  cat("\nTesting sample size calculations...\n")
  
  # Test simple random sampling
  n_srs <- calculate_simple_random_sample_size(N = 10000, p = 0.5, confidence_level = 0.95, margin_error = 0.05)
  cat("Simple Random Sampling (N=10000, p=0.5, 95% CI, 5% ME):", n_srs, "\n")
  
  # Test stratified sampling with sample strata data
  strata_data <- data.frame(
    Stratum = c("Stratum 1", "Stratum 2", "Stratum 3"),
    Population = c(3333, 3333, 3334),
    stringsAsFactors = FALSE
  )
  n_strat <- calculate_stratified_sample_size(strata_data, p = 0.5, confidence_level = 0.95, margin_error = 0.05)
  cat("Stratified Sampling (N=10000, p=0.5, 95% CI, 5% ME):", n_strat, "\n")
  
  # Test cluster sampling
  n_cluster <- calculate_cluster_sample_size(N = 10000, p = 0.5, confidence_level = 0.95, margin_error = 0.05, cluster_size = 10, icc = 0.02)
  cat("Cluster Sampling (N=10000, p=0.5, 95% CI, 5% ME, cluster=10, ICC=0.02):", n_cluster, "\n")
}

# Test if the app can be run
test_app_launch <- function() {
  cat("\nTesting app launch...\n")
  
  if (file.exists("app.R")) {
    cat("app.R found. You can run the app using: shiny::runApp('app.R')\n")
  } else {
    cat("Error: app.R not found.\n")
  }
}

# Run tests
test_sample_size_calculations()
test_app_launch()

cat("\nTest completed. If no errors were shown, the application should work correctly.\n")
cat("To run the application, use: shiny::runApp('app.R')\n")
