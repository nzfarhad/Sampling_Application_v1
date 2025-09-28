# Decision Tree Module
# Interactive wizard for sampling method selection

library(shiny)
library(shinyjs)
library(rmarkdown)

decision_tree_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Header
    div(
      class = "row mb-4",
      div(
        class = "col-12",
        h2("Sampling Method Decision Tree", class = "text-center mb-3"),
        p("Fill in the fields below to generate recommended sampling methods. These recommendations are indicative and may not fully capture all contextual factors.", 
          class = "text-center text-muted")
      )
    ),
    
    # Main content
    div(
      class = "row",
      # Input panel - Left side
      div(
        class = "col-md-4",
        div(
          class = "card",
          div(
            class = "card-body",
            # Study Name
            textInput(
              ns("study_name"),
              "Study Name",
              placeholder = "Enter study name (optional)"
            ),
            
            # Survey Mode
            selectInput(
              ns("survey_mode"),
              "1. Survey Mode",
              choices = list(
                "In-person" = "in_person",
                "Phone" = "phone"
              )
            ),
            
            # Generalization Required
            selectInput(
              ns("generalization_required"),
              "2. Generalization required",
              choices = list(
                "Yes" = "yes",
                "No" = "no"
              )
            ),
            
            # Sub-group Comparison Required
            selectInput(
              ns("subgroup_comparison"),
              "3. Sub-group comparison required",
              choices = list(
                "Yes" = "yes",
                "No" = "no"
              )
            ),
            
            # Population Data Available
            selectInput(
              ns("population_data"),
              "4. Population data available",
              choices = list(
                "Complete Sampling Frame" = "complete",
                "Partial Sampling Frame" = "partial",
                "No list available" = "none"
              )
            ),
            
            # Number of Communities/Villages
            selectInput(
              ns("num_communities"),
              "5. Number of communities/villages",
              choices = list(
                "<50–70" = "less_70",
                ">70" = "more_70"
              )
            ),
            
            # Logistical Constraints
            selectInput(
              ns("logistical_constraints"),
              "6. Logistical constraints",
              choices = list(
                "Limited budget/time" = "limited_budget",
                "Remote/insecure areas" = "remote_insecure",
                "No constraints" = "no_constraints"
              )
            ),
            
            # Submit button
            div(
              class = "mt-4",
              actionButton(
                ns("analyze"),
                "Analyze Requirements",
                class = "btn btn-primary btn-block"
              )
            )
          )
        )
      ),
      
      # Results panel - Right side
      div(
        class = "col-md-8",
        # Recommendations card
        div(
          class = "card mb-4",
          div(
            class = "card-header",
            h5("Recommended Sampling Methods", class = "mb-0")
          ),
          div(
            class = "card-body",
            uiOutput(ns("recommendations"))
          )
        ),
        
        
        # Summary table
        div(
          class = "card",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            h5("Method Details", class = "mb-0"),
            downloadButton(ns("download_report"), "Download Report", class = "btn btn-sm btn-outline-primary")
          ),
          div(
            class = "card-body",
            uiOutput(ns("method_details"))
          )
        )
      )
    )
  )
}

# Server logic
decision_tree_server <- function(id, study_name = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Sampling methods database
    methods <- list(
      "purposive" = list(
        name = "Purposive Sampling",
        category = "Non-Probability",
        description = "Selecting cases based on specific criteria or purpose",
        requirements = list(
          "Clear selection criteria",
          "Access to target groups",
          "Expert knowledge of population"
        ),
        pros = list(
          "Targeted selection",
          "Efficient for specific groups",
          "Good for in-depth study"
        ),
        cons = list(
          "Not generalizable",
          "Potential selection bias",
          "Requires expertise"
        ),
        suitable_for = list(
          "Qualitative studies",
          "Case studies",
          "Exploratory research"
        )
      ),
      "snowball" = list(
        name = "Snowball Sampling",
        category = "Non-Probability",
        description = "Participants recruit other participants",
        requirements = list(
          "Initial contacts",
          "Network connections",
          "Trust building"
        ),
        pros = list(
          "Access to hidden populations",
          "Builds on social networks",
          "Cost-effective"
        ),
        cons = list(
          "Selection bias",
          "Not representative",
          "Network limitations"
        ),
        suitable_for = list(
          "Hidden populations",
          "Sensitive topics",
          "Network studies"
        )
      ),
      "quota" = list(
        name = "Quota Sampling",
        category = "Non-Probability",
        description = "Selection based on population proportions",
        requirements = list(
          "Known population characteristics",
          "Clear quotas",
          "Access to subgroups"
        ),
        pros = list(
          "Ensures subgroup representation",
          "Cost-effective",
          "Quick implementation"
        ),
        cons = list(
          "Not random",
          "Selection bias possible",
          "Limited generalizability"
        ),
        suitable_for = list(
          "Market research",
          "Opinion polls",
          "Rapid assessments"
        )
      ),
      "convenience" = list(
        name = "Convenience Sampling",
        category = "Non-Probability",
        description = "Selecting readily available cases",
        requirements = list(
          "Easy access to participants",
          "Quick implementation needed",
          "Limited resources"
        ),
        pros = list(
          "Fast and easy",
          "Low cost",
          "Simple implementation"
        ),
        cons = list(
          "High bias risk",
          "Not representative",
          "Limited validity"
        ),
        suitable_for = list(
          "Pilot studies",
          "Preliminary research",
          "Resource constraints"
        )
      ),
      "simple_random" = list(
        name = "Simple Random Sampling",
        category = "Probability",
        description = "Each unit has an equal chance of selection",
        requirements = list(
          "Complete sampling frame required",
          "Open access to population",
          "Good for statistical generalization"
        ),
        pros = list(
          "Unbiased",
          "Easy to analyze",
          "Representative"
        ),
        cons = list(
          "Requires complete list",
          "Can be costly",
          "May miss rare subgroups"
        ),
        suitable_for = list(
          "Generalization",
          "Comparison of subgroups",
          "Routine monitoring"
        )
      ),
      "stratified" = list(
        name = "Stratified Random Sampling",
        category = "Probability",
        description = "Population divided into subgroups (strata) before sampling",
        requirements = list(
          "Complete sampling frame with group information",
          "Open access to population",
          "Known subgroup characteristics"
        ),
        pros = list(
          "Ensures subgroup representation",
          "More efficient than SRS",
          "Good for comparisons"
        ),
        cons = list(
          "Requires subgroup information",
          "More complex analysis",
          "Higher cost than SRS"
        ),
        suitable_for = list(
          "Comparison of subgroups",
          "Generalization",
          "When subgroups are important"
        )
      ),
      "cluster" = list(
        name = "Cluster Sampling",
        category = "Probability",
        description = "Sampling naturally occurring groups",
        requirements = list(
          "List of clusters",
          "Access to selected clusters",
          "Geographic/administrative units"
        ),
        pros = list(
          "Cost-effective",
          "Good for geographically dispersed populations",
          "Requires less resources"
        ),
        cons = list(
          "Lower precision",
          "Complex analysis",
          "Cluster effects"
        ),
        suitable_for = list(
          "Large geographic areas",
          "Limited resources",
          "When population is grouped"
        )
      ),
      "systematic" = list(
        name = "Systematic Sampling",
        category = "Probability",
        description = "Selecting units at fixed intervals",
        requirements = list(
          "Ordered list of population",
          "No cyclical patterns",
          "Regular access points"
        ),
        pros = list(
          "Easy to implement",
          "Spread across population",
          "Good for ongoing selection"
        ),
        cons = list(
          "Potential periodicity bias",
          "Requires ordered list",
          "Less random than SRS"
        ),
        suitable_for = list(
          "Routine monitoring",
          "Quality control",
          "Regular assessments"
        )
      ),
      "rdd" = list(
        name = "Random Digit Dialing (RDD)",
        category = "Probability",
        description = "Randomly generating phone numbers for sampling",
        requirements = list(
          "No sampling frame available",
          "Phone survey mode",
          "Random number generation capability"
        ),
        pros = list(
          "No sampling frame needed",
          "Can reach unlisted numbers",
          "Good for general population surveys"
        ),
        cons = list(
          "Low response rates",
          "Excludes non-phone households",
          "May miss certain demographics"
        ),
        suitable_for = list(
          "Phone surveys without lists",
          "General population studies",
          "Quick data collection"
        )
      ),
      "stratified_cluster" = list(
        name = "Stratified Cluster Sampling",
        category = "Probability",
        description = "Combining stratification and cluster sampling for complex populations",
        requirements = list(
          "Population can be stratified",
          "Clusters within strata",
          "Access to selected clusters"
        ),
        pros = list(
          "Ensures subgroup representation",
          "Cost-effective for large areas",
          "Good for complex populations"
        ),
        cons = list(
          "Complex analysis required",
          "Requires detailed population info",
          "Higher cost than simple cluster"
        ),
        suitable_for = list(
          "Large geographic areas",
          "Subgroup comparisons",
          "Complex population structures"
        )
      )
    )
    
    # Reactive values for results and inputs
    selected_methods <- reactiveVal(NULL)
    
    # Study Name synchronization
    if (!is.null(study_name)) {
      # Update local input when shared study_name changes
      observeEvent(study_name(), {
        if (study_name() != input$study_name) {
          updateTextInput(session, "study_name", value = study_name())
        }
      })
      
      # Update shared study_name when local input changes
      observeEvent(input$study_name, {
        if (input$study_name != study_name()) {
          study_name(input$study_name)
        }
      })
    }
    
    # Create a reactive expression for the current parameters
    current_params <- reactive({
      list(
        survey_mode = input$survey_mode,
        generalization = input$generalization_required,
        subgroup_comparison = input$subgroup_comparison,
        population_data = input$population_data,
        num_communities = input$num_communities,
        constraints = input$logistical_constraints
      )
    })
    
    # Analyze button handler
    observeEvent(input$analyze, {
      # Reset selected methods to force re-render
      selected_methods(NULL)
      
      # Decision logic to select appropriate methods
      recommended <- analyze_requirements(
        survey_mode = current_params()$survey_mode,
        generalization = current_params()$generalization,
        subgroup_comparison = current_params()$subgroup_comparison,
        population_data = current_params()$population_data,
        num_communities = current_params()$num_communities,
        constraints = current_params()$constraints
      )
      
      # Update selected methods
      selected_methods(recommended)
    })
    
    # Analysis function
    analyze_requirements <- function(survey_mode, generalization, subgroup_comparison, population_data, num_communities, constraints) {
      selected <- character(0)
      
      # Step 1: Phone Survey Logic
      if (survey_mode == "phone") {
        if (population_data %in% c("complete", "partial")) {
          if (subgroup_comparison == "no") {
            selected <- c("simple_random", "systematic")
          } else {
            selected <- c("stratified")
          }
        } else if (population_data == "none") {
          selected <- c("rdd")
        }
      }
      
      # Step 2: In-person Survey Logic
      if (survey_mode == "in_person") {
        # Probability Sampling (Generalization Required = Yes)
        if (generalization == "yes") {
          if (subgroup_comparison == "no" && population_data %in% c("complete", "partial") && num_communities == "less_70" && constraints == "no_constraints") {
            selected <- c("simple_random", "systematic")
          } else if (subgroup_comparison == "yes" && population_data %in% c("complete", "partial") && num_communities == "less_70" && constraints == "no_constraints") {
            selected <- c("stratified")
          } else if (subgroup_comparison == "no" && population_data %in% c("complete", "partial") && num_communities == "less_70" && constraints %in% c("limited_budget", "remote_insecure")) {
            selected <- c("cluster")
          } else if (subgroup_comparison == "yes" && population_data %in% c("complete", "partial") && num_communities == "less_70" && constraints %in% c("limited_budget", "remote_insecure")) {
            selected <- c("stratified_cluster")
          } else if (subgroup_comparison == "no" && population_data == "none" && num_communities == "less_70") {
            selected <- c("cluster")
          } else if (subgroup_comparison == "yes" && population_data == "none" && num_communities == "less_70") {
            selected <- c("stratified_cluster")
          } else if (subgroup_comparison == "no" && num_communities == "more_70") {
            selected <- c("cluster")
          } else if (subgroup_comparison == "yes" && num_communities == "more_70") {
            selected <- c("stratified_cluster")
          }
        }
        
        # Non-Probability Sampling (Generalization Required = No)
        if (generalization == "no") {
          if (subgroup_comparison == "yes" && population_data %in% c("complete", "partial") && constraints %in% c("limited_budget", "remote_insecure")) {
            selected <- c("quota")
          } else if (subgroup_comparison == "yes" && population_data %in% c("complete", "partial") && constraints == "no_constraints") {
            selected <- c("quota")
          } else if (subgroup_comparison == "no" && population_data %in% c("complete", "partial") && constraints == "no_constraints") {
            selected <- c("convenience")
          } else if (subgroup_comparison == "no" && population_data == "none") {
            selected <- c("snowball")
          } else if (subgroup_comparison == "yes" && population_data == "none") {
            selected <- c("snowball")
          } else if (subgroup_comparison == "yes" && population_data %in% c("complete", "partial") && constraints == "limited_budget") {
            selected <- c("purposive", "convenience")
          } else if (subgroup_comparison == "yes" && population_data %in% c("complete", "partial") && constraints == "remote_insecure") {
            selected <- c("purposive", "snowball")
          }
        }
      }
      
      # If no methods are selected, provide default recommendations
      if (length(selected) == 0) {
        if (survey_mode == "phone") {
          if (generalization == "yes") {
            selected <- c("simple_random")
          } else {
            selected <- c("convenience")
          }
        } else {
          if (generalization == "yes") {
            selected <- c("cluster")
          } else {
            if (population_data == "none") {
              selected <- c("snowball")
            } else {
              selected <- c("purposive")
            }
          }
        }
      }
      
      # Remove duplicates and return unique methods
      return(unique(selected))
    }
    
    # Render recommendations
    output$recommendations <- renderUI({
      # Invalidate cache when analyze button is clicked
      input$analyze
      
      req(selected_methods())
      isolate({
        method_cards <- lapply(selected_methods(), function(method_id) {
        method <- methods[[method_id]]
        div(
          class = "card mb-3",
          div(
            class = "card-body",
            h4(method$name, class = "card-title"),
            p(class = "card-text", method$description),
            p(
              class = "card-text",
              tags$small(
                class = "text-muted",
                paste("Category:", method$category)
              )
            )
          )
        )
      })
      
      div(class = "recommendations-container", method_cards)
      })
    })
    
    
    # Render method details
    output$method_details <- renderUI({
      req(selected_methods())
      
      details_tables <- lapply(selected_methods(), function(method_id) {
        method <- methods[[method_id]]
        
        div(
          class = "method-details mb-4",
          h5(method$name),
          tags$table(
            class = "table table-sm",
            tags$tr(
              tags$th(width = "30%", "Requirements"),
              tags$td(tags$ul(class = "mb-0", lapply(method$requirements, tags$li)))
            ),
            tags$tr(
              tags$th("Pros"),
              tags$td(tags$ul(class = "mb-0", lapply(method$pros, tags$li)))
            ),
            tags$tr(
              tags$th("Cons"),
              tags$td(tags$ul(class = "mb-0", lapply(method$cons, tags$li)))
            ),
            tags$tr(
              tags$th("Suitable for"),
              tags$td(tags$ul(class = "mb-0", lapply(method$suitable_for, tags$li)))
            )
          )
        )
      })
      
      div(class = "details-container", details_tables)
    })
    
    # Download handler for report
    output$download_report <- downloadHandler(
      filename = function() {
        study_name_clean <- if (input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Method")
        } else {
          "sampling-recommendations"
        }
        paste0(study_name_clean, "-", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(
          sprintf('---
title: "%s"
date: "%s"
output:
  pdf_document: default
---

## Study Information

**Study Name**: %s

## Study Requirements

- **Survey Mode**: %s
- **Generalization Required**: %s
- **Sub-group Comparison Required**: %s
- **Population Data Available**: %s
- **Number of Communities/Villages**: %s
- **Logistical Constraints**: %s

## Recommended Methods

%s
',
            if (input$study_name != "") paste0(input$study_name, " - Sampling Method Recommendations") else "Sampling Method Recommendations",
            format(Sys.time(), "%B %d, %Y"),
            if (input$study_name != "") input$study_name else "Not specified",
            input$survey_mode,
            input$generalization_required,
            input$subgroup_comparison,
            input$population_data,
            input$num_communities,
            input$logistical_constraints,
            paste(lapply(selected_methods(), function(method_id) {
              method <- methods[[method_id]]
              sprintf("### %s\n\n%s\n\n#### Requirements\n%s\n\n#### Advantages\n%s\n\n#### Disadvantages\n%s\n\n#### Best Used For\n%s\n",
                     method$name,
                     method$description,
                     paste("*", method$requirements, collapse = "\n"),
                     paste("*", method$pros, collapse = "\n"),
                     paste("*", method$cons, collapse = "\n"),
                     paste("*", method$suitable_for, collapse = "\n"))
            }), collapse = "\n\n")
          ),
          temp_rmd
        )
        
        rmarkdown::render(temp_rmd,
                         output_file = file,
                         quiet = TRUE)
        
        unlink(temp_rmd)
      }
    )
  })
}