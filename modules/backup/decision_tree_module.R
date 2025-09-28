# Decision Tree Module (Stepwise Sampling Logic)
library(shiny)

decision_tree_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(class = "row mb-4",
      div(class = "col-12",
        h2("Sampling Method Decision Tree", class = "text-center mb-3"),
        p("Answer the questions below to find the most appropriate sampling method for your study.", 
          class = "text-center text-muted")
      )
    ),
    div(class = "row",
      div(class = "col-md-5",
        div(class = "card",
          div(class = "card-body",
            uiOutput(ns("step1")),
            uiOutput(ns("step2")),
            uiOutput(ns("step3")),
            div(class = "mt-4",
              actionButton(ns("analyze"), "Analyze Requirements", class = "btn btn-primary btn-block")
            )
          )
        )
      ),
      div(class = "col-md-7",
        div(class = "card mb-4",
          div(class = "card-header",
            h5("Recommended Sampling Method", class = "mb-0")
          ),
          div(class = "card-body",
            uiOutput(ns("recommendation"))
          )
        )
      )
    )
  )
}

decision_tree_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Step 1: Sub-group / Prevalence Requirement
    output$step1 <- renderUI({
      radioButtons(
        session$ns("subgroup"),
        "Is prevalence data (e.g., food security estimates) required for specific sub-groups (defined by administrative boundaries or other criteria)?",
        choices = c("Yes" = "yes", "No" = "no"),
        selected = character(0)
      )
    })

    # Step 2a/2b: Household List
    output$step2 <- renderUI({
      req(input$subgroup)
      radioButtons(
        session$ns("household_list"),
        "Is a complete list of all households within the population available or easily constructed?",
        choices = c("Yes" = "yes", "No" = "no"),
        selected = character(0)
      )
    })

    # Step 3a/3b: Village/Block List
    output$step3 <- renderUI({
      req(input$subgroup)
      req(input$household_list == "no")
      radioButtons(
        session$ns("village_list"),
        "Is a complete list of villages (rural) or neighborhoods/blocks (urban) available or easily constructed?",
        choices = c("Yes" = "yes", "No" = "no"),
        selected = character(0)
      )
    })

    # Decision logic
    get_recommendation <- reactive({
      req(input$subgroup)
      req(input$household_list)
      if (input$household_list == "no") req(input$village_list)

      if (input$subgroup == "yes") {
        # Stratified path
        if (input$household_list == "yes") {
          return(list(
            method = "Stratified Sampling (single stage)",
            rationale = "Sub-group estimates are needed and a complete household list is available. Stratified sampling ensures representation of each subgroup.",
            details = c("Define strata (sub-groups)", "Randomly select households within each stratum")
          ))
        } else if (input$village_list == "yes") {
          return(list(
            method = "Two-stage random or cluster sampling",
            rationale = "Sub-group estimates are needed, no household list but a village/block list is available. Use two-stage or cluster sampling.",
            details = c("Randomly select villages/blocks (clusters)", "List households in selected clusters", "Randomly select households within clusters")
          ))
        } else {
          return(list(
            method = "Multi-stage sampling / Non-probability sampling",
            rationale = "No lists are available. Multi-stage or non-probability sampling may be required.",
            details = c("Define broad clusters", "Use available information to select units", "Document limitations")
          ))
        }
      } else {
        # Not stratified path
        if (input$household_list == "yes") {
          return(list(
            method = "Simple Random Sampling (single stage)",
            rationale = "A complete household list is available. Simple random sampling is appropriate.",
            details = c("Randomly select households from the list")
          ))
        } else if (input$village_list == "yes") {
          return(list(
            method = "Two-stage random or cluster sampling",
            rationale = "No household list but a village/block list is available. Use two-stage or cluster sampling.",
            details = c("Randomly select villages/blocks (clusters)", "List households in selected clusters", "Randomly select households within clusters")
          ))
        } else {
          return(list(
            method = "Multi-stage sampling / Non-probability sampling",
            rationale = "No lists are available. Multi-stage or non-probability sampling may be required.",
            details = c("Define broad clusters", "Use available information to select units", "Document limitations")
          ))
        }
      }
    })

    # Update recommendation when analyze button is clicked
    observeEvent(input$analyze, {
      output$recommendation <- renderUI({
        rec <- get_recommendation()
        tagList(
          h4(rec$method, class = "card-title mb-2"),
          p(tags$b("Rationale:"), rec$rationale),
          p(tags$b("Field Steps:"), tags$ul(lapply(rec$details, tags$li)))
        )
      })
    })
  })
}