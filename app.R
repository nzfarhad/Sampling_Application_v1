# Sampling Application - Main Shiny App
# Author: Sayed Farhad Nabizada
# Date: September 2025
# Description: This Shiny application provides tools, including a decision tree, sample size calculator, and sampling guidelines.

# Set global options for file upload size limit (100MB)
options(shiny.maxRequestSize = 100 * 1024 * 1024)

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


# Source additional modules
source("modules/decision_tree_module.R")
source("modules/sample_calculator_module.R")
source("modules/sampling_generator_module.R")
source("modules/wiki_module.R")
source("modules/user_guide_module.R")

# Source sampling functions
source("sampling_generation_functions.R")

# Define UI
ui <- page_navbar(
  title = tags$span(
    "ATR Sampling Tool",
    style = "color: #730202; font-weight: 600;"
  ),
  theme = bs_theme(
    version = 5,
    primary = "#730202",
    secondary = "#f27304",
    success = "#f27304",
    info = "#730202",
    warning = "#f27304",
    light = "#F8F9FA",
    dark = "#212529",
    bg = "#FFFFFF",
    fg = "#212529",
    "font-size-base" = "1rem"
  ),
  id = "main_navbar",
  
  # Navigation tabs
  nav_panel(
    "How to Use",
    icon = icon("question-circle"),
    user_guide_ui("user_guide")
  ),
  
  nav_panel(
    "Decision Tree",
    icon = icon("sitemap"),
    decision_tree_ui("decision_tree")
  ),
  
  nav_panel(
    "Sample Calculator",
    icon = icon("calculator"),
    sample_calculator_ui("sample_calculator")
  ),
  
  nav_panel(
    "Sample Generator",
    icon = icon("random"),
    sampling_generator_ui("sampling_generator")
  ),
  
  nav_panel(
    "Sampling Guideline",
    icon = icon("book"),
    wiki_ui("wiki")
  ),
  # Note: Ensure each module appears once in the navbar
)

# Define server
server <- function(input, output, session) {
  
  # Shared reactive value for Study Name
  study_name <- reactiveVal("")
  
  # Initialize modules with shared study_name
  user_guide_server("user_guide")
  decision_tree_server("decision_tree", study_name = study_name)
  sample_calculator_server("sample_calculator", study_name = study_name)
  sampling_generator_server("sampling_generator")
  wiki_server("wiki")
  
  # Initialize tooltips
  shinyjs::runjs("
    $(document).ready(function() {
      $('[data-bs-toggle=\"tooltip\"]').tooltip();
    });
  ")
}

# Run the application
shinyApp(ui = ui, server = server)

