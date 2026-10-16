# Decision Tree Module
# Asks about the study and recommends one sampling design, with its stages,
# reasons, alternatives and warnings, and hands the settings to the
# calculators and the Sample Generator. The rules are in decision_tree_functions.R.

library(shiny)
library(shinyjs)
library(rmarkdown)

# JavaScript condition that shows a question only when it applies
# (mirrors decision_applicable() in decision_tree_functions.R)
decision_condition <- function(question) {
  switch(question,
    unit = , spread = "['level','change','verify'].indexOf(input.purpose) > -1",
    frame = , reporting = "['level','change','verify','qualitative'].indexOf(input.purpose) > -1",
    mode = "input.purpose && input.purpose.length > 0",
    constraints = "input.purpose && input.purpose !== 'pilot'",
    other_group = "['level','change','verify','qualitative'].indexOf(input.purpose) > -1 && input.reporting && input.reporting.indexOf('other') > -1",
    comparison = "input.purpose === 'change'",
    panel = "input.purpose === 'change' && ['before_after','groups_both'].indexOf(input.comparison) > -1",
    random = "input.purpose === 'change' && ['groups_endline','groups_both'].indexOf(input.comparison) > -1"
  )
}

decision_tree_ui <- function(id) {
  ns <- NS(id)
  # Each question is a block (styles.css: .dt-q); questions that do not apply are hidden
  question <- function(input_tag, class = "dt-q") div(class = class, input_tag)
  ask <- function(id, input_tag, class = "dt-q") {
    conditionalPanel(condition = decision_condition(id), ns = ns, question(input_tag, class))
  }
  # Question text with a hint on how to answer
  q_label <- function(id, multiple = FALSE) {
    tagList(DECISION_QUESTIONS[[id]], span(class = "dt-hint", if (multiple) "Tick all that apply" else "Choose one"))
  }

  fluidPage(
    useShinyjs(),

    div(
      class = "row mb-4",
      div(
        class = "col-12",
        h2("Sampling Design Decision Tree", class = "text-center mb-3"),
        p("Answer the questions about your study. The recommended design updates as you answer, and can be sent to the calculators and the Sample Generator.",
          class = "text-center text-muted")
      )
    ),

    div(
      class = "row",
      # Questions
      div(
        class = "col-md-4",
        div(
          class = "card mb-4",
          div(class = "card-header", h5("Your Study", class = "mb-0")),
          div(
            class = "card-body",
            textInput(ns("study_name"), "Study Name", placeholder = "Enter study name (optional)"),
            question(radioButtons(ns("purpose"), q_label("purpose"), choices = DECISION_CHOICES$purpose, selected = character(0))),
            ask("unit", radioButtons(ns("unit"), q_label("unit"), choices = DECISION_CHOICES$unit)),
            ask("frame", radioButtons(ns("frame"), q_label("frame"), choices = DECISION_CHOICES$frame)),
            ask("reporting", checkboxGroupInput(ns("reporting"), q_label("reporting", multiple = TRUE),
                                                choices = DECISION_CHOICES$reporting, selected = "overall")),
            ask("other_group", textInput(ns("other_group"), "Name of the other group", placeholder = "e.g. returnees, people with disabilities"),
                class = "dt-q dt-q-sub"),
            ask("spread", radioButtons(ns("spread"), q_label("spread"), choices = DECISION_CHOICES$spread, selected = "many")),
            ask("mode", radioButtons(ns("mode"), q_label("mode"), choices = DECISION_CHOICES$mode)),
            ask("constraints", checkboxGroupInput(ns("constraints"), q_label("constraints", multiple = TRUE), choices = DECISION_CHOICES$constraints)),
            ask("comparison", radioButtons(ns("comparison"), q_label("comparison"), choices = DECISION_CHOICES$comparison)),
            ask("panel", radioButtons(ns("panel"), q_label("panel"), choices = DECISION_CHOICES$panel, selected = "no")),
            ask("random", radioButtons(ns("random"), q_label("random"), choices = DECISION_CHOICES$random, selected = "no"))
          )
        )
      ),

      # Recommendation
      div(
        class = "col-md-8",
        div(
          class = "card mb-4",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            h5("Recommended Design", class = "mb-0"),
            downloadButton(ns("download_report"), "Download Report", class = "btn btn-sm btn-outline-primary")
          ),
          div(class = "card-body", uiOutput(ns("recommendation")))
        ),
        uiOutput(ns("design_details"))
      )
    )
  )
}

#' Decision tree server
#'
#' @param study_name Shared reactiveVal for the study name
#' @param on_handover function(target, settings, design_name), called by the
#'   hand-over buttons; target is "sample", "evaluation" or "generator"
#' @param on_article function(article_id), opens a Sampling Guideline article
decision_tree_server <- function(id, study_name = NULL, on_handover = NULL, on_article = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!is.null(study_name)) {
      observeEvent(study_name(), {
        if (!identical(study_name(), input$study_name)) updateTextInput(session, "study_name", value = study_name())
      })
      observeEvent(input$study_name, {
        if (!identical(input$study_name, study_name())) study_name(input$study_name)
      })
    }

    answers <- reactive({
      list(purpose = if (is.null(input$purpose)) "" else input$purpose,
           unit = input$unit, frame = input$frame, reporting = input$reporting,
           other_group = if (is.null(input$other_group)) "" else input$other_group,
           spread = input$spread, mode = input$mode, constraints = input$constraints,
           comparison = input$comparison, panel = input$panel, random = input$random)
    })

    # Recommendation follows the answers; NULL until the purpose is answered
    recommendation <- reactive(recommend_design(answers()))

    # Links to guideline articles: <a class="dt-article" data-article="id">
    shinyjs::runjs(sprintf(
      "$(document).on('click', '#%s .dt-article', function(e) { e.preventDefault(); Shiny.setInputValue('%s', $(this).data('article'), {priority: 'event'}); });",
      ns("recommendation_area"), ns("open_article")
    ))
    observeEvent(input$open_article, if (!is.null(on_article)) on_article(input$open_article))

    article_link <- function(article_id) {
      title <- WIKI_ARTICLES[[article_id]]$title
      tags$a(href = "#", class = "dt-article", `data-article` = article_id, if (is.null(title)) article_id else title)
    }
    bullet_list <- function(items) tags$ul(class = "mb-0", lapply(items, tags$li))

    output$recommendation <- renderUI({
      rec <- recommendation()
      if (is.null(rec)) {
        return(p(class = "text-muted mb-0", "Answer the first question, \"What is the study for?\", to see a recommendation."))
      }
      des <- DESIGNS[[rec$design]]
      ho <- rec$handover
      calc_label <- if (!is.null(ho$calculator)) {
        if (ho$calculator$target == "evaluation") "Calculate sample size (Evaluation Calculator)" else "Calculate sample size (Sample Calculator)"
      }

      div(
        id = ns("recommendation_area"),
        h4(class = "mb-1", id = ns("design_name"), des$name),
        p(class = "text-muted small mb-3", paste("Category:", des$category),
          if (!is.null(rec$evaluation)) paste0(" | Evaluation design: ", EVALUATION_DESIGN_LABELS[[rec$evaluation]])),

        h6("How to select the sample"),
        tags$ol(lapply(rec$stages, tags$li)),

        h6("Why this design"),
        div(class = "mb-3", bullet_list(rec$reasons)),

        if (length(rec$alternatives) > 0) tagList(h6("Alternatives, and when to switch"), div(class = "mb-3", bullet_list(rec$alternatives))),

        if (length(rec$warnings) > 0) {
          div(class = "alert alert-warning small", tags$strong("Points to watch"), bullet_list(rec$warnings))
        },

        h6("Next steps"),
        div(
          class = "d-flex flex-wrap gap-2 mb-3",
          if (!is.null(ho$calculator)) actionButton(ns("to_calculator"), calc_label, class = "btn btn-primary btn-sm"),
          if (!is.null(ho$generator)) actionButton(ns("to_generator"), "Draw the sample (Sample Generator)", class = "btn btn-outline-primary btn-sm"),
          if (is.null(ho$calculator) && is.null(ho$generator)) span(class = "text-muted small", "No sample size to calculate for this design; follow the steps above.")
        ),
        p(class = "small mb-0", tags$strong("Field protocols: "),
          tagList(lapply(seq_along(rec$protocols), function(i) {
            tagList(if (i > 1) " | ", article_link(rec$protocols[[i]]))
          })))
      )
    })

    output$design_details <- renderUI({
      rec <- recommendation()
      req(rec)
      des <- DESIGNS[[rec$design]]
      div(
        class = "card",
        div(class = "card-header", h5("About This Design", class = "mb-0")),
        div(
          class = "card-body",
          tags$table(
            class = "table table-sm mb-0",
            tags$tr(tags$th(width = "25%", "When to use"), tags$td(des$when)),
            tags$tr(tags$th("List needed"), tags$td(des$needs)),
            tags$tr(tags$th("Common field problems"), tags$td(bullet_list(des$field)))
          )
        )
      )
    })

    observeEvent(input$to_calculator, {
      rec <- recommendation()
      req(rec, rec$handover$calculator, on_handover)
      on_handover(rec$handover$calculator$target, rec$handover$calculator, DESIGNS[[rec$design]]$name)
    })
    observeEvent(input$to_generator, {
      rec <- recommendation()
      req(rec, rec$handover$generator, on_handover)
      on_handover("generator", rec$handover$generator, DESIGNS[[rec$design]]$name)
    })

    # PDF report, always built from the current answers
    output$download_report <- downloadHandler(
      filename = function() {
        stem <- if (!is.null(input$study_name) && input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Design")
        } else {
          "sampling-design"
        }
        paste0(stem, "-", format(Sys.time(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        rec <- recommendation()
        validate(need(!is.null(rec), "Answer the first question before downloading the report."))
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(decision_report_rmd(rec, input$study_name), temp_rmd, useBytes = TRUE)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
        unlink(temp_rmd)
      }
    )

    invisible(list(recommendation = recommendation))
  })
}

#' Note shown on a tab that received settings from the Decision Tree
#'
#' @param handover NULL, or list(design = design name, details = character
#'   vector, action = optional next action)
#' @param dismiss_id Namespaced id of the dismiss link
handover_note_ui <- function(handover, dismiss_id) {
  if (is.null(handover)) return(NULL)
  div(
    class = "alert alert-light border small",
    tags$strong(paste0("Settings from the Decision Tree (", handover$design, "): ")),
    paste0(paste(handover$details, collapse = "; "), ". ",
           "Check them against your study and change any value as needed",
           if (!is.null(handover$action)) paste0(", ", handover$action) else "", ". "),
    actionLink(dismiss_id, "Dismiss")
  )
}

#' R Markdown source of the decision tree PDF report
decision_report_rmd <- function(rec, study_name = "") {
  study <- if (is.null(study_name)) "" else trimws(study_name)
  # YAML single-quoted string: double any single quote; drop characters LaTeX cannot print
  yaml_title <- gsub("'", "''", ascii_text(if (study != "") paste(study, "- Sampling Design") else "Sampling Design"))
  bullets <- function(x) paste0("- ", ascii_text(x), collapse = "\n")
  des <- DESIGNS[[rec$design]]
  a <- rec$answers
  answers <- vapply(rec$applicable, function(q) {
    paste0("- **", DECISION_QUESTIONS[[q]], "** ", ascii_text(decision_label(q, a[[q]])),
           if (q == "reporting" && "other" %in% a$reporting && nzchar(trimws(a$other_group))) paste0(" (", ascii_text(trimws(a$other_group)), ")") else "")
  }, character(1))
  ho <- rec$handover
  next_steps <- c(
    if (!is.null(ho$calculator) && ho$calculator$target == "evaluation")
      paste0("Calculate the sample size in the Evaluation Calculator (design: ", EVALUATION_DESIGN_LABELS[[ho$calculator$design]], ")."),
    if (!is.null(ho$calculator) && ho$calculator$target == "sample") "Calculate the sample size in the Sample Calculator.",
    if (!is.null(ho$generator)) paste0("Draw the sample in the Sample Generator",
                                       if (length(ho$generator$strata)) paste0(" (strata: ", paste(ho$generator$strata, collapse = ", "), ")") else "", "."),
    paste0("Follow the field protocols in the Sampling Guideline: ",
           paste(vapply(rec$protocols, function(x) WIKI_ARTICLES[[x]]$title, character(1)), collapse = ", "), ".")
  )
  paste0(
    "---\n", "title: '", yaml_title, "'\n", "date: '", format(Sys.time(), "%B %d, %Y"), "'\n",
    "output:\n  pdf_document: default\n", "---\n\n",
    "## Study\n\n", "**Study name:** ", if (study != "") ascii_text(study) else "Not specified", "\n\n",
    paste(answers, collapse = "\n"), "\n\n",
    "## Recommended Design: ", ascii_text(des$name), "\n\n",
    "*Category: ", des$category, if (!is.null(rec$evaluation)) paste0(". Evaluation design: ", EVALUATION_DESIGN_LABELS[[rec$evaluation]]), ".*\n\n",
    "### How to Select the Sample\n\n", paste0(seq_along(rec$stages), ". ", ascii_text(rec$stages), collapse = "\n"), "\n\n",
    "### Why This Design\n\n", bullets(rec$reasons), "\n\n",
    if (length(rec$alternatives)) paste0("### Alternatives, and When to Switch\n\n", bullets(rec$alternatives), "\n\n") else "",
    if (length(rec$warnings)) paste0("### Points to Watch\n\n", bullets(rec$warnings), "\n\n") else "",
    "### About This Design\n\n", "**When to use:** ", ascii_text(des$when), "\n\n", "**List needed:** ", ascii_text(des$needs), "\n\n",
    "**Common field problems:**\n\n", bullets(des$field), "\n\n",
    "## Next Steps\n\n", bullets(next_steps), "\n\n",
    "\n*Generated by the ATR Sampling Tool", if (exists("app_version")) paste0(" (version ", app_version(), ")") else "", ".*\n"
  )
}
