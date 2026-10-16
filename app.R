# Sampling Application - Main Shiny App
# Author: Sayed Farhad Nabizada
# Date: September 2025
# Description: This Shiny application provides tools, including a decision tree, sample size calculator, and sampling guidelines.

# Upload size limit: 30 MB keeps a frame and its copies within shinyapps.io instance memory
options(shiny.maxRequestSize = 30 * 1024 * 1024)

# Load required libraries
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(htmltools)
library(stringr)
library(markdown)


# Source sampling and sample size functions (used by the modules when they load)
source("sampling_generation_functions.R")
source("sample_size_functions.R")
source("decision_tree_functions.R")

# Source additional modules
source("modules/decision_tree_module.R")
source("modules/sample_calculator_module.R")
source("modules/evaluation_calculator_module.R")
source("modules/sampling_generator_module.R")
source("modules/wiki_module.R")
source("modules/user_guide_module.R")

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
  # Not fillable: pages scroll normally and the footer sits below the content
  fillable = FALSE,
  footer = div(
    class = "text-center text-muted small py-2 border-top",
    paste("ATR Sampling Tool - version", app_version(commit = FALSE))
  ),
  
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
    "Evaluation Calculator",
    icon = icon("chart-line"),
    evaluation_calculator_ui("evaluation_calculator")
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
  calculator <- sample_calculator_server("sample_calculator", study_name = study_name)
  evaluation <- evaluation_calculator_server("evaluation_calculator", study_name = study_name)
  generator <- sampling_generator_server("sampling_generator", study_name = study_name)
  wiki <- wiki_server("wiki")

  # The Decision Tree hands its design to the calculators and the generator,
  # and opens guideline articles
  open_tab <- function(tab) {
    nav_select("main_navbar", selected = tab, session = session)
    shinyjs::runjs("window.scrollTo(0, 0);")
  }
  decision_tree_server(
    "decision_tree", study_name = study_name,
    on_handover = function(target, settings, design_name) {
      switch(target,
        sample = { calculator$apply_handover(settings, design_name); open_tab("Sample Calculator") },
        evaluation = { evaluation$apply_handover(settings, design_name); open_tab("Evaluation Calculator") },
        generator = { generator$apply_handover(settings, design_name); open_tab("Sample Generator") }
      )
    },
    on_article = function(article_id) {
      wiki$show_article(article_id)
      open_tab("Sampling Guideline")
    }
  )
  
  # Initialize tooltips
  shinyjs::runjs("
    $(document).ready(function() {
      $('[data-bs-toggle=\"tooltip\"]').tooltip();
    });
  ")
}

# Run the application
shinyApp(ui = ui, server = server)

