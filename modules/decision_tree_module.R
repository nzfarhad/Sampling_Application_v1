# Decision Tree Module
# Interactive wizard for sampling method selection

library(shiny)
library(shinyjs)
library(rmarkdown)

# Answer choices (label = value); also used to print readable answers in the report
DECISION_CHOICES <- list(
  survey_mode = c("In-person" = "in_person", "Phone" = "phone"),
  generalization = c("Yes" = "yes", "No" = "no"),
  subgroup_comparison = c("Yes" = "yes", "No" = "no"),
  population_data = c(
    "Complete sampling frame" = "complete",
    "Partial sampling frame (some areas or groups missing)" = "partial",
    "No list available" = "none",
    "Hidden or hard-to-reach group (no list is possible)" = "hidden"
  ),
  num_communities = c(
    "Few enough to visit all of them" = "all",
    "Fewer than about 70" = "less_70",
    "More than about 70" = "more_70"
  ),
  constraints = c(
    "Limited budget/time" = "limited_budget",
    "Remote/insecure areas" = "remote_insecure",
    "No constraints" = "no_constraints"
  )
)

DECISION_QUESTIONS <- c(
  survey_mode = "Survey Mode",
  generalization = "Generalization Required",
  subgroup_comparison = "Sub-group Comparison Required",
  population_data = "Population Data Available",
  num_communities = "Number of Communities/Villages",
  constraints = "Logistical Constraints"
)

#' Readable label for an answer code
decision_label <- function(question, value) {
  ch <- DECISION_CHOICES[[question]]
  lab <- names(ch)[ch == value]
  if (length(lab) == 0) value else lab
}

# Sampling methods database
SAMPLING_METHODS <- list(
  "purposive" = list(
    name = "Purposive Sampling",
    category = "Non-Probability",
    description = "Selecting cases based on specific criteria or purpose",
    requirements = list("Clear selection criteria", "Access to target groups", "Expert knowledge of population"),
    pros = list("Targeted selection", "Efficient for specific groups", "Good for in-depth study"),
    cons = list("Not generalizable", "Potential selection bias", "Requires expertise"),
    suitable_for = list("Qualitative studies (KIIs, FGDs)", "Case studies", "Exploratory research")
  ),
  "snowball" = list(
    name = "Snowball Sampling",
    category = "Non-Probability",
    description = "Participants recruit other participants",
    requirements = list("Initial contacts", "Network connections", "Trust building"),
    pros = list("Access to hidden populations", "Builds on social networks", "Cost-effective"),
    cons = list("Selection bias", "Not representative", "Network limitations"),
    suitable_for = list("Hidden populations with no list", "Sensitive topics", "Network studies")
  ),
  "quota" = list(
    name = "Quota Sampling",
    category = "Non-Probability",
    description = "Filling set numbers of respondents per subgroup, without random selection",
    requirements = list("Known population characteristics", "Clear quotas", "Access to subgroups"),
    pros = list("Ensures subgroup representation", "Cost-effective", "Quick implementation"),
    cons = list("Not random", "Selection bias possible", "Limited generalizability"),
    suitable_for = list("Rapid assessments", "Perception studies", "Pre-testing with set subgroups")
  ),
  "convenience" = list(
    name = "Convenience Sampling",
    category = "Non-Probability",
    description = "Selecting readily available cases",
    requirements = list("Easy access to participants", "Quick implementation needed", "Limited resources"),
    pros = list("Fast and easy", "Low cost", "Simple implementation"),
    cons = list("High bias risk", "Not representative", "Limited validity"),
    suitable_for = list("Pilot studies", "Tool pre-testing", "Preliminary research")
  ),
  "simple_random" = list(
    name = "Simple Random Sampling",
    category = "Probability",
    description = "Each unit has an equal chance of selection",
    requirements = list("Complete sampling frame required", "Access to every selected unit", "Good for statistical generalization"),
    pros = list("Unbiased", "Easy to analyze", "Representative"),
    cons = list("Requires complete list", "Can be costly when units are spread out", "May miss rare subgroups"),
    suitable_for = list("Generalization", "Small, accessible populations with a full list", "Routine monitoring")
  ),
  "stratified" = list(
    name = "Stratified Random Sampling",
    category = "Probability",
    description = "Population divided into subgroups (strata) before sampling",
    requirements = list("Complete sampling frame with group information", "Access to selected units", "Known stratum sizes"),
    pros = list("Ensures subgroup representation", "Often more precise than SRS", "Good for comparisons"),
    cons = list("Requires subgroup information on the frame", "Needs weights if allocation is not proportional", "Larger total sample when every stratum needs its own precision"),
    suitable_for = list("Comparison of subgroups", "Generalization", "When subgroups are important")
  ),
  "cluster" = list(
    name = "Cluster Sampling",
    category = "Probability",
    description = "Selecting villages or other groups first, then interviewing several units in each",
    requirements = list("List of clusters (e.g. villages) with a size measure", "Access to selected clusters", "Household listing in selected clusters if no list exists"),
    pros = list("Cost-effective", "Good for geographically dispersed populations", "Needs a list of clusters only"),
    cons = list("Lower precision (design effect)", "More complex analysis", "Needs more interviews than SRS for the same precision"),
    suitable_for = list("Large geographic areas", "Limited resources", "When no household list exists")
  ),
  "systematic" = list(
    name = "Systematic Sampling",
    category = "Probability",
    description = "Selecting units at fixed intervals from a list, after a random start",
    requirements = list("Ordered list of population", "No cyclical patterns in the list order", "Known list size"),
    pros = list("Easy to implement", "Spread across the list", "Good for ongoing selection"),
    cons = list("Potential periodicity bias", "Requires ordered list", "No exact variance formula"),
    suitable_for = list("Routine monitoring", "Quality control", "Beneficiary lists sorted by area")
  ),
  "rdd" = list(
    name = "Random Digit Dialing (RDD)",
    category = "Probability",
    description = "Randomly generating phone numbers for sampling",
    requirements = list("Known number ranges per operator", "Phone survey mode", "Random number generation capability"),
    pros = list("No list of respondents needed", "Can reach unlisted numbers", "Good for general population surveys"),
    cons = list("Low response rates", "Excludes non-phone households", "Under-represents women, rural and poorer households"),
    suitable_for = list("Phone surveys without lists", "General population perception studies", "Quick data collection")
  ),
  "stratified_cluster" = list(
    name = "Stratified Cluster Sampling",
    category = "Probability",
    description = "Combining stratification and cluster sampling for complex populations",
    requirements = list("Population can be stratified", "List of clusters within each stratum", "Access to selected clusters"),
    pros = list("Ensures subgroup representation", "Cost-effective for large areas", "Good for complex populations"),
    cons = list("Complex analysis required", "Requires detailed population info", "Larger sample than simple cluster when each stratum needs its own precision"),
    suitable_for = list("Large geographic areas", "Subgroup comparisons", "Province or district estimates")
  ),
  "site_census" = list(
    name = "All Sites, Sample Within Each",
    category = "Probability",
    description = "Visit every community or site, and select respondents randomly within each one",
    requirements = list("Small number of sites that can all be reached", "List of units, or a listing, within each site", "Sample size per site"),
    pros = list("No site-selection error", "Every site gets a result", "Simple weighting"),
    cons = list("Travel cost grows with the number of sites", "Small samples per site give wide margins", "Needs a list or listing in every site"),
    suitable_for = list("Project monitoring with few sites", "Site verification", "Programmes in a handful of villages")
  )
)

#' Recommend Sampling Methods
#'
#' @param p List of answers: survey_mode, generalization, subgroup_comparison,
#'   population_data, num_communities, constraints (codes from DECISION_CHOICES)
#'
#' @return List with `methods` (ids in SAMPLING_METHODS) and `notes`
#'   (caveats and warnings to show with the recommendation)
recommend_sampling <- function(p) {
  methods <- character(0)
  notes <- character(0)
  subgroups <- p$subgroup_comparison == "yes"
  frame <- p$population_data

  if (p$generalization == "no") {
    # Non-probability: results describe the people interviewed, not the population
    if (frame == "hidden") {
      methods <- "snowball"
    } else if (subgroups) {
      methods <- c("quota", "purposive")
    } else {
      methods <- "purposive"
      if (p$constraints == "limited_budget") {
        methods <- c(methods, "convenience")
        notes <- c(notes, "Use convenience sampling only for pilots and tool pre-testing, not for findings you will report.")
      }
    }
    notes <- c(notes, "Without random selection the results cannot be generalised to the population or given margins of error.")
    if (p$survey_mode == "phone") {
      notes <- c(notes, "For phone interviews, select respondents from the contact lists you have against your criteria, and record who could not be reached.")
    }
  } else if (frame == "hidden") {
    methods <- "snowball"
    notes <- c(notes, paste(
      "A hidden group has no list, so a probability sample (and generalisation) is not possible with standard methods.",
      "Consider building a frame first (e.g. through community leaders or service providers), or treat the results as indicative."
    ))
  } else if (p$survey_mode == "phone") {
    if (frame %in% c("complete", "partial")) {
      methods <- if (subgroups) "stratified" else c("simple_random", "systematic")
      notes <- c(notes, "Draw extra numbers as replacements and set a call-back rule (e.g. 3 attempts on different days and times) before replacing.")
    } else {
      methods <- "rdd"
      notes <- c(notes, paste(
        "Afghanistan caveat: there is no public list of allocated number ranges, many women have no phone of their own,",
        "and coverage is lower in rural areas. RDD will under-represent these groups; weight the results or use a",
        "beneficiary or partner contact list where one exists."
      ))
    }
    notes <- c(notes, "Phone samples miss households without a phone; state this coverage gap in the report.")
  } else {
    # In-person, probability sampling
    spread <- p$num_communities == "more_70" || p$constraints != "no_constraints"
    if (p$num_communities == "all") {
      methods <- if (subgroups) c("site_census", "stratified") else "site_census"
      notes <- c(notes, "With few sites, include every site and draw respondents randomly within each one; use the sites as strata.")
    } else if (frame == "none") {
      methods <- if (subgroups) "stratified_cluster" else "cluster"
      notes <- c(notes, "With no household list, select villages from a village list (e.g. with population sizes), then list households in the selected villages before drawing the sample.")
    } else if (spread) {
      methods <- if (subgroups) "stratified_cluster" else "cluster"
    } else {
      methods <- if (subgroups) "stratified" else c("simple_random", "systematic")
    }
    if (p$constraints == "remote_insecure") {
      notes <- c(notes, "Draw reserve clusters or units in advance, record every inaccessible site, and report it as a coverage gap rather than replacing it silently.")
    }
  }

  if (frame == "partial" && p$generalization == "yes") {
    notes <- c(notes, paste(
      "Coverage warning: the frame is incomplete, so results only describe the units on the list.",
      "Update the frame first, or use a dual-frame design (list sample plus an area sample for the missing parts)."
    ))
  }

  list(methods = unique(methods), notes = notes)
}

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
            textInput(ns("study_name"), "Study Name", placeholder = "Enter study name (optional)"),
            selectInput(ns("survey_mode"), "1. Survey Mode", choices = DECISION_CHOICES$survey_mode),
            selectInput(ns("generalization_required"), "2. Generalization required", choices = DECISION_CHOICES$generalization),
            selectInput(ns("subgroup_comparison"), "3. Sub-group comparison required", choices = DECISION_CHOICES$subgroup_comparison),
            selectInput(ns("population_data"), "4. Population data available", choices = DECISION_CHOICES$population_data),
            selectInput(ns("num_communities"), "5. Number of communities/villages", choices = DECISION_CHOICES$num_communities,
                        selected = "less_70"),
            selectInput(ns("logistical_constraints"), "6. Logistical constraints", choices = DECISION_CHOICES$constraints),
            div(
              class = "mt-4",
              actionButton(ns("analyze"), "Analyze Requirements", class = "btn btn-primary btn-block")
            )
          )
        )
      ),

      # Results panel - Right side
      div(
        class = "col-md-8",
        div(
          class = "card mb-4",
          div(class = "card-header", h5("Recommended Sampling Methods", class = "mb-0")),
          div(class = "card-body", uiOutput(ns("recommendations")))
        ),
        div(
          class = "card",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            h5("Method Details", class = "mb-0"),
            downloadButton(ns("download_report"), "Download Report", class = "btn btn-sm btn-outline-primary")
          ),
          div(class = "card-body", uiOutput(ns("method_details")))
        )
      )
    )
  )
}

# Server logic
decision_tree_server <- function(id, study_name = NULL) {
  moduleServer(id, function(input, output, session) {

    methods <- SAMPLING_METHODS
    recommendation <- reactiveVal(NULL)

    # Study Name synchronization
    if (!is.null(study_name)) {
      observeEvent(study_name(), {
        if (!identical(study_name(), input$study_name)) updateTextInput(session, "study_name", value = study_name())
      })
      observeEvent(input$study_name, {
        if (!identical(input$study_name, study_name())) study_name(input$study_name)
      })
    }

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

    observeEvent(input$analyze, {
      recommendation(c(recommend_sampling(current_params()), list(params = current_params())))
    })

    # Once shown, keep the recommendation in step with the answers
    observeEvent(current_params(), {
      if (!is.null(recommendation())) {
        recommendation(c(recommend_sampling(current_params()), list(params = current_params())))
      }
    }, ignoreInit = TRUE)

    selected_methods <- reactive(recommendation()$methods)

    output$recommendations <- renderUI({
      rec <- recommendation()
      req(rec)
      method_cards <- lapply(rec$methods, function(method_id) {
        method <- methods[[method_id]]
        div(
          class = "card mb-3",
          div(
            class = "card-body",
            h4(method$name, class = "card-title"),
            p(class = "card-text", method$description),
            p(class = "card-text", tags$small(class = "text-muted", paste("Category:", method$category)))
          )
        )
      })
      notes <- if (length(rec$notes) > 0) {
        div(class = "alert alert-warning small mb-0", tags$strong("Points to consider"),
            tags$ul(class = "mb-0", lapply(rec$notes, tags$li)))
      }
      div(class = "recommendations-container", method_cards, notes)
    })

    output$method_details <- renderUI({
      req(selected_methods())
      details_tables <- lapply(selected_methods(), function(method_id) {
        method <- methods[[method_id]]
        div(
          class = "method-details mb-4",
          h5(method$name),
          tags$table(
            class = "table table-sm",
            tags$tr(tags$th(width = "30%", "Requirements"), tags$td(tags$ul(class = "mb-0", lapply(method$requirements, tags$li)))),
            tags$tr(tags$th("Pros"), tags$td(tags$ul(class = "mb-0", lapply(method$pros, tags$li)))),
            tags$tr(tags$th("Cons"), tags$td(tags$ul(class = "mb-0", lapply(method$cons, tags$li)))),
            tags$tr(tags$th("Suitable for"), tags$td(tags$ul(class = "mb-0", lapply(method$suitable_for, tags$li))))
          )
        )
      })
      div(class = "details-container", details_tables)
    })

    # Download handler for report
    output$download_report <- downloadHandler(
      filename = function() {
        study_name_clean <- if (!is.null(input$study_name) && input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Method")
        } else {
          "sampling-recommendations"
        }
        paste0(study_name_clean, "-", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        # Always report the current answers, even if they changed after the last analysis
        rec <- c(recommend_sampling(current_params()), list(params = current_params()))
        study <- if (is.null(input$study_name)) "" else trimws(input$study_name)
        # YAML single-quoted string: double any single quote; drop characters LaTeX cannot print
        yaml_title <- gsub("'", "''", ascii_text(if (study != "") paste(study, "- Sampling Method Recommendations") else "Sampling Method Recommendations"))
        answers <- paste0("- **", DECISION_QUESTIONS, "**: ",
                          vapply(names(DECISION_QUESTIONS), function(q) decision_label(q, rec$params[[q]]), character(1)),
                          collapse = "\n")
        notes <- if (length(rec$notes) > 0) paste0("## Points to Consider\n\n", paste0("- ", rec$notes, collapse = "\n"), "\n\n") else ""
        method_text <- paste(lapply(rec$methods, function(method_id) {
          method <- methods[[method_id]]
          sprintf("### %s\n\n%s\n\n#### Requirements\n%s\n\n#### Advantages\n%s\n\n#### Disadvantages\n%s\n\n#### Best Used For\n%s\n",
                  method$name, method$description,
                  paste("*", method$requirements, collapse = "\n"),
                  paste("*", method$pros, collapse = "\n"),
                  paste("*", method$cons, collapse = "\n"),
                  paste("*", method$suitable_for, collapse = "\n"))
        }), collapse = "\n\n")

        rmd <- paste0(
          "---\n",
          "title: '", yaml_title, "'\n",
          "date: '", format(Sys.time(), "%B %d, %Y"), "'\n",
          "output:\n  pdf_document: default\n",
          "---\n\n",
          "## Study Information\n\n",
          "**Study Name**: ", if (study != "") ascii_text(study) else "Not specified", "\n\n",
          "## Study Requirements\n\n", answers, "\n\n",
          "## Recommended Methods\n\n", method_text, "\n\n",
          notes,
          "\n*Generated by the ATR Sampling Tool", if (exists("app_version")) paste0(" (version ", app_version(), ")") else "", ".*\n"
        )
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(rmd, temp_rmd, useBytes = TRUE)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
        unlink(temp_rmd)
      }
    )
  })
}
