# Evaluation Calculator Module
# Sample size and minimum detectable difference when comparing groups or survey
# rounds: two independent groups, panel, difference-in-differences and ANCOVA,
# for proportions and means, with several indicators, subgroups and field
# adjustments. The formulas are in sample_size_functions.R.

EVAL_INDICATOR_COLUMNS <- c(Indicator = "Indicator", Type = "Type (proportion / mean)", Baseline = "Baseline",
                            SD = "SD (means)", Change = "Change", ICC = "ICC (optional)",
                            Control_Baseline = "Control baseline (DiD / ANCOVA)", Control_Trend = "Control trend (DiD / ANCOVA)")
EVAL_SUBGROUP_COLUMNS <- c(Subgroup = "Subgroup", Share = "Share of the sample (%)", Fixed = "Fixed sample (optional)")

eval_default_indicators <- function() {
  data.frame(Indicator = c("Food consumption score", "Households with poor food consumption"),
             Type = c("mean", "proportion"), Baseline = c(40, 0.30), SD = c(12, NA), Change = c(3, -0.08),
             ICC = NA_real_, Control_Baseline = NA_real_, Control_Trend = NA_real_, stringsAsFactors = FALSE)
}

eval_default_subgroups <- function() {
  data.frame(Subgroup = c("Female-headed households", "Male-headed households"), Share = c(20, 80),
             Fixed = NA_real_, stringsAsFactors = FALSE)
}

#' Small HTML table for result tables that need no paging or editing
eval_html_table <- function(df) {
  cell <- function(v) if (is.na(v)) "" else if (is.numeric(v)) format(v, big.mark = ",") else as.character(v)
  div(
    class = "table-responsive",
    tags$table(
      class = "table table-sm table-striped mb-0",
      tags$thead(tags$tr(lapply(gsub("_", " ", names(df)), tags$th))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(df[i, , drop = TRUE], function(v) tags$td(cell(v))))))
    )
  )
}

#' A difference for display: points for a proportion, units for a mean
eval_effect_label <- function(a, x) {
  if (is.na(x)) return(NA_character_)
  if (eval_is_mean(a)) format(signif(abs(x), 4)) else paste0(round(abs(x) * 100, 1), " pts")
}

evaluation_calculator_ui <- function(id) {
  ns <- NS(id)
  panel_designs <- "['panel', 'did_panel', 'ancova'].indexOf(input.design) >= 0"
  arm_designs <- "['did_cross', 'did_panel', 'ancova'].indexOf(input.design) >= 0"
  one <- "input.indicator_count == 'one'"
  several <- "input.indicator_count == 'several'"
  size_mode <- "input.eval_mode == 'sample_size'"
  prop <- paste0(one, " && input.outcome != 'mean'")
  mean_one <- paste0(one, " && input.outcome == 'mean'")
  scale_is <- function(s) paste0(size_mode, " && input.effect_scale == '", s, "'")

  fluidPage(
    useShinyjs(),

    tags$head(
      tags$style(HTML("
        .eval-calc .card-body { padding: 0.75rem; }
        .eval-calc .card-header { padding: 0.5rem 0.75rem; }
        .eval-calc .form-group { margin-bottom: 0.5rem; }
        .eval-calc label { margin-bottom: 0.25rem; font-size: 0.9rem; }
        .eval-calc .card { margin-bottom: 0.9rem; }
        .eval-calc .headline-value { font-size: 1.8rem; font-weight: 600; color: #730202; }
        .eval-calc .headline-label { font-size: 0.85rem; color: #666; }
        .eval-calc table.dataTable td { white-space: nowrap; }
      "))
    ),

    div(
      class = "eval-calc",

      # Header
      div(
        class = "row mb-3",
        div(
          class = "col-12",
          h2("Evaluation Sample Size Calculator", class = "text-center mb-2"),
          p("Sample size, or the smallest difference you can detect, when comparing proportions or means between groups or survey rounds.",
            class = "text-center text-muted")
        )
      ),
      uiOutput(ns("handover_note")),

      # Design and mode
      div(
        class = "card",
        div(class = "card-header", h5("Study Design", class = "mb-0")),
        div(
          class = "card-body",
          div(
            class = "row g-2",
            div(
              class = "col-md-4",
              textInput(ns("study_name"), "Study Name", placeholder = "Enter study name (optional)", width = "100%")
            ),
            div(
              class = "col-md-4",
              radioButtons(
                ns("eval_mode"),
                "What do you want to calculate?",
                choices = list(
                  "Sample size for a given difference" = "sample_size",
                  "Smallest detectable difference (MDE) for a given sample" = "mde"
                ),
                selected = "sample_size"
              )
            ),
            div(
              class = "col-md-4",
              selectInput(ns("design"), "Design", choices = EVALUATION_DESIGNS, selected = "independent", width = "100%")
            )
          ),
          uiOutput(ns("design_help"))
        )
      ),

      div(
        class = "row g-2",

        # Test and indicators
        div(
          class = "col-md-6",
          div(
            class = "card",
            div(class = "card-header", h5("Test and Indicators", class = "mb-0")),
            div(
              class = "card-body",
              div(
                class = "row g-2",
                div(class = "col-md-4",
                    numericInput(ns("power"), "Power (1 - β)", value = 0.80, min = 0.5, max = 0.99, step = 0.05)),
                div(class = "col-md-4",
                    numericInput(ns("alpha"), "Significance level (α)", value = 0.05, min = 0.001, max = 0.2, step = 0.01)),
                div(class = "col-md-4",
                    radioButtons(ns("sided"), "Test", choices = list("Two-sided" = "2", "One-sided" = "1"),
                                 selected = "2", inline = TRUE))
              ),
              radioButtons(ns("indicator_count"), "Key indicators",
                           choices = list("One" = "one", "Several (the largest sample is used)" = "several"),
                           selected = "one", inline = TRUE),
              hr(class = "my-2"),

              # One indicator
              conditionalPanel(
                condition = one,
                ns = ns,
                div(
                  class = "row g-2",
                  div(class = "col-md-6",
                      textInput(ns("indicator_name"), "Indicator name (optional)", placeholder = "e.g. Food consumption score")),
                  div(class = "col-md-6",
                      radioButtons(ns("outcome"), "Outcome",
                                   choices = list("Proportion" = "proportion", "Mean (FCS, income, ...)" = "mean"),
                                   selected = "proportion", inline = TRUE))
                ),
                conditionalPanel(
                  condition = paste0(prop, " && input.design == 'independent'"),
                  ns = ns,
                  radioButtons(
                    ns("variance_method"), "Variance",
                    choices = list("Unpooled (standard)" = "unpooled", "Pooled (Fleiss; matches R power.prop.test)" = "pooled"),
                    selected = "unpooled", inline = TRUE
                  )
                ),
                conditionalPanel(
                  condition = prop,
                  ns = ns,
                  div(
                    class = "row g-2",
                    div(class = "col-md-6",
                        numericInput(ns("p1"), "Proportion in group 1 (comparison / baseline, p₁)", value = 0.5,
                                     min = 0.01, max = 0.99, step = 0.01)),
                    conditionalPanel(
                      condition = arm_designs,
                      ns = ns,
                      class = "col-md-6",
                      numericInput(ns("p_control"), "Control group baseline proportion", value = 0.5, min = 0.01, max = 0.99, step = 0.01)
                    )
                  )
                ),
                conditionalPanel(
                  condition = mean_one,
                  ns = ns,
                  div(
                    class = "row g-2",
                    div(class = "col-md-4", numericInput(ns("mean1"), "Baseline mean", value = 40, step = 1)),
                    div(class = "col-md-4", numericInput(ns("sd"), "Standard deviation (SD)", value = 12, min = 0, step = 1)),
                    conditionalPanel(
                      condition = arm_designs,
                      ns = ns,
                      class = "col-md-4",
                      numericInput(ns("mean_control"), "Control group baseline mean", value = 40, step = 1)
                    )
                  ),
                  p("Take the SD from an earlier survey of a similar population (a baseline, PDM or the WFP/FSAC dataset). ",
                    "If it is unknown, state the change as a standardised effect (Cohen's d): 0.2 is small, 0.5 medium.",
                    class = "text-muted small")
                )
              ),

              # Several indicators
              conditionalPanel(
                condition = several,
                ns = ns,
                p("One row per key indicator. Type is proportion or mean. Give the change with its sign (negative for a decrease), ",
                  "on the scale chosen below. Leave the ICC blank to use the common ICC, and the control columns blank for the same ",
                  "baseline as treatment and no trend. Double-click a cell to edit it.",
                  class = "text-muted small"),
                DT::dataTableOutput(ns("indicators_table")),
                div(
                  class = "mt-2 mb-2",
                  actionButton(ns("add_indicator"), "Add indicator", class = "btn btn-outline-secondary btn-sm"),
                  actionButton(ns("remove_indicator"), "Remove last", class = "btn btn-outline-secondary btn-sm")
                ),
                checkboxInput(ns("bonferroni"), "Bonferroni correction (α divided by the number of indicators)", value = FALSE)
              ),

              conditionalPanel(
                condition = size_mode,
                ns = ns,
                radioButtons(ns("effect_scale"), "State the change to detect as",
                             choices = list("Difference (points or units)" = "absolute",
                                            "Relative change (% of baseline)" = "relative",
                                            "Second value (p₂)" = "value2"),
                             selected = "absolute", inline = TRUE)
              ),
              conditionalPanel(
                condition = one,
                ns = ns,
                div(
                  class = "row g-2",
                  conditionalPanel(
                    condition = paste0(scale_is("absolute"), " && input.outcome != 'mean'"),
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("effect"), "Detectable difference (D, 0.10 = 10 points)", value = 0.10,
                                 min = 0.005, max = 0.9, step = 0.01)
                  ),
                  conditionalPanel(
                    condition = paste0(scale_is("absolute"), " && input.outcome == 'mean'"),
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("effect_mean"), "Detectable difference (D, in the indicator's units)", value = 3, min = 0, step = 0.5)
                  ),
                  conditionalPanel(
                    condition = scale_is("relative"),
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("effect_relative"), "Relative change (% of the baseline, e.g. 20)", value = 20, min = 0.1, step = 1)
                  ),
                  conditionalPanel(
                    condition = scale_is("d"),
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("effect_d"), "Standardised effect (Cohen's d)", value = 0.25, min = 0.01, step = 0.05)
                  ),
                  conditionalPanel(
                    condition = paste0(scale_is("value2"), " && input.outcome != 'mean'"),
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("p2"), "Second proportion (p₂)", value = 0.6, min = 0.01, max = 0.99, step = 0.01)
                  ),
                  conditionalPanel(
                    condition = paste0(scale_is("value2"), " && input.outcome == 'mean'"),
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("mean2"), "Second mean", value = 43, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.eval_mode == 'mde' || input.effect_scale != 'value2'",
                    ns = ns,
                    class = "col-md-6",
                    radioButtons(ns("direction"), "Expected change",
                                 choices = list("Increase" = "increase", "Decrease (e.g. stunting)" = "decrease"),
                                 selected = "increase", inline = TRUE)
                  )
                ),
                conditionalPanel(
                  condition = paste0(arm_designs, " && input.outcome != 'mean'"),
                  ns = ns,
                  numericInput(ns("control_change"), "Change expected without the programme (control group trend, e.g. 0.02 or -0.03)",
                               value = 0, min = -0.5, max = 0.5, step = 0.01)
                ),
                conditionalPanel(
                  condition = paste0(arm_designs, " && input.outcome == 'mean'"),
                  ns = ns,
                  numericInput(ns("control_change_mean"), "Change expected without the programme (control group trend, in units)",
                               value = 0, step = 0.5)
                )
              ),
              conditionalPanel(
                condition = "input.design != 'panel'",
                ns = ns,
                numericInput(ns("ratio"), "Allocation ratio (group 2 : group 1, or control : treatment)", value = 1,
                             min = 0.1, max = 10, step = 0.1)
              )
            )
          )
        ),

        # Field assumptions, design effect and adjustments
        div(
          class = "col-md-6",
          div(
            class = "card",
            div(class = "card-header", h5("Field Assumptions", class = "mb-0")),
            div(
              class = "card-body",
              div(
                class = "row g-2",
                div(class = "col-md-6",
                    numericInput(ns("response_rate"), "Response rate (%)", value = 90, min = 1, max = 100, step = 1)),
                conditionalPanel(
                  condition = panel_designs,
                  ns = ns,
                  class = "col-md-6",
                  numericInput(ns("attrition"), "Attrition between rounds (%)", value = 10, min = 0, max = 90, step = 1)
                )
              ),
              conditionalPanel(
                condition = panel_designs,
                ns = ns,
                numericInput(ns("rho"), "Baseline-endline correlation (ρ)", value = 0, min = 0, max = 0.95, step = 0.05),
                p("ρ is the correlation between a household's baseline and endline values. Household survey outcomes usually ",
                  "fall between 0.2 and 0.5 (income and consumption towards 0.5, food security and other volatile indicators ",
                  "towards 0.2). Use a value from earlier panel data when you have it: overstating ρ leaves the study underpowered.",
                  class = "text-muted small")
              ),
              hr(class = "my-2"),
              checkboxInput(ns("clustered"), "Clustered sample (e.g. several households per village)", value = TRUE),
              conditionalPanel(
                condition = "input.clustered",
                ns = ns,
                radioButtons(ns("deff_method"), "Design effect from",
                             choices = list("ICC and interviews per cluster" = "icc",
                                            "ICC and unequal cluster sizes (CV)" = "cv",
                                            "A known DEFF from an earlier survey" = "known"),
                             selected = "icc"),
                div(
                  class = "row g-2",
                  div(class = "col-md-6",
                      numericInput(ns("m"), "Interviews per cluster (m, the mean when sizes vary)", value = 10, min = 1, step = 1)),
                  conditionalPanel(
                    condition = "input.deff_method != 'known'",
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("icc"), "Intraclass correlation (ICC)", value = 0.06, min = 0, max = 1, step = 0.01)
                  ),
                  conditionalPanel(
                    condition = "input.deff_method == 'cv'",
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("cv_cluster"), "CV of cluster sizes (SD / mean)", value = 0.5, min = 0, step = 0.05)
                  ),
                  conditionalPanel(
                    condition = "input.deff_method == 'known'",
                    ns = ns,
                    class = "col-md-6",
                    numericInput(ns("deff_known"), "Design effect (DEFF)", value = 1.5, min = 1, step = 0.1)
                  )
                ),
                p("ICC is how alike households in the same cluster are. Household indicators often fall between 0.02 and 0.10; ",
                  "services shared by a whole village (water source, school access) can be 0.2 or more.",
                  class = "text-muted small"),
                div(
                  class = "row g-2",
                  div(class = "col-md-6",
                      numericInput(ns("inaccessible"), "Expected inaccessible clusters (%)", value = 0, min = 0, max = 90, step = 1)),
                  div(class = "col-md-6",
                      p(class = "text-muted small mt-md-4 mb-0", "Separate from non-response. Follow the ",
                        actionLink(ns("open_replacement"), "Replacement Protocol"), " in the field."))
                ),
                checkboxInput(ns("small_cluster_t"), "Use t-values when there are fewer than 20 clusters per group", value = TRUE)
              ),
              hr(class = "my-2"),
              checkboxInput(ns("weighting"), "Unequal weights (weighting effect)", value = FALSE),
              conditionalPanel(
                condition = "input.weighting",
                ns = ns,
                numericInput(ns("cv_weights"), "CV of the weights (SD / mean)", value = 0.3, min = 0, step = 0.05),
                p("The design effect is multiplied by 1 + CV² (Kish). Compute the CV from the base_weight column of the ",
                  "Sample Generator output, or of an earlier survey.", class = "text-muted small")
              ),
              div(
                class = "row g-2",
                div(class = "col-md-6",
                    numericInput(ns("population"), "Population per group (optional, for small programmes)", value = NA, min = 2, step = 1)),
                div(class = "col-md-6",
                    checkboxInput(ns("matching"), "Matched comparison group (e.g. propensity score matching)", value = FALSE),
                    conditionalPanel(
                      condition = "input.matching",
                      ns = ns,
                      numericInput(ns("matching_factor"), "Matching factor (1.2 to 1.5)", value = 1.3, min = 1, step = 0.05)
                    ))
              ),
              conditionalPanel(
                condition = "input.matching",
                ns = ns,
                p("Only when the comparison group is built by matching after data collection: the factor allows for units ",
                  "dropped outside common support.", class = "text-muted small")
              ),
              conditionalPanel(
                condition = "input.eval_mode == 'mde'",
                ns = ns,
                hr(class = "my-2"),
                h6("Available sample (group 1, or the treatment arm)"),
                conditionalPanel(
                  condition = "input.clustered",
                  ns = ns,
                  numericInput(ns("clusters_per_group"), "Clusters per group", value = 40, min = 1, step = 1)
                ),
                conditionalPanel(
                  condition = "!input.clustered",
                  ns = ns,
                  numericInput(ns("n_per_group"), "Interviews per group", value = 400, min = 2, step = 1)
                )
              )
            )
          )
        )
      ),

      # Subgroups
      div(
        class = "card",
        div(class = "card-header", h5("Subgroups (optional)", class = "mb-0")),
        div(
          class = "card-body",
          checkboxInput(ns("use_subgroups"), "Report results by subgroup (e.g. female-headed households, provinces)", value = FALSE),
          conditionalPanel(
            condition = "input.use_subgroups",
            ns = ns,
            conditionalPanel(
              condition = size_mode,
              ns = ns,
              radioButtons(ns("subgroup_goal"), "Goal",
                           choices = list("Detect the difference within each subgroup" = "within",
                                          "Report the overall result; show each subgroup's MDE" = "overall"),
                           selected = "overall", inline = TRUE)
            ),
            p("Give each subgroup's expected share of the sample, or a fixed sample when you will oversample it ",
              "(then weight the results). Double-click a cell to edit it.", class = "text-muted small"),
            DT::dataTableOutput(ns("subgroups_table")),
            div(
              class = "mt-2",
              actionButton(ns("add_subgroup"), "Add subgroup", class = "btn btn-outline-secondary btn-sm"),
              actionButton(ns("remove_subgroup"), "Remove last", class = "btn btn-outline-secondary btn-sm")
            )
          )
        )
      ),

      # Results
      div(
        class = "card border-success",
        div(class = "card-header bg-success text-white", h5("Result", class = "mb-0")),
        div(
          class = "card-body",
          uiOutput(ns("headline")),
          uiOutput(ns("warnings")),
          div(
            class = "d-flex justify-content-between align-items-center mt-2",
            tags$strong("Methods text for the inception report"),
            tags$button(
              type = "button", class = "btn btn-outline-secondary btn-sm", id = ns("copy_methods"),
              onclick = sprintf(paste0(
                "navigator.clipboard.writeText(document.getElementById('%s').innerText);",
                "var b = this; b.innerText = 'Copied'; setTimeout(function() { b.innerText = 'Copy'; }, 1500);"
              ), ns("explanation")),
              "Copy"
            )
          ),
          p(textOutput(ns("explanation"), inline = TRUE), class = "text-muted mt-1 mb-0")
        )
      ),
      uiOutput(ns("detail_tables")),

      div(
        class = "row g-2",
        div(
          class = "col-md-6",
          div(
            class = "card",
            div(class = "card-header", h5("Power Curve", class = "mb-0")),
            div(class = "card-body", plotlyOutput(ns("power_curve"), height = "320px"))
          ),
          div(
            class = "card",
            div(class = "card-header", h5("Sample by Group", class = "mb-0")),
            div(class = "card-body", DT::dataTableOutput(ns("groups_table")))
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "card",
            div(class = "card-header", h5("Detailed Calculations", class = "mb-0")),
            div(class = "card-body", DT::dataTableOutput(ns("steps_table")))
          ),
          div(
            class = "card",
            div(class = "card-header", h5("Download Results", class = "mb-0")),
            div(class = "card-body",
                downloadButton(ns("download_excel"), "Excel", class = "btn btn-outline-success btn-sm"),
                downloadButton(ns("download_pdf"), "PDF Report", class = "btn btn-outline-danger btn-sm"))
          )
        )
      )
    )
  )
}

evaluation_calculator_server <- function(id, study_name = NULL, on_article = NULL) {
  moduleServer(id, function(input, output, session) {

    # Study Name synchronization with the other tabs
    if (!is.null(study_name)) {
      observeEvent(study_name(), {
        if (!identical(study_name(), input$study_name)) updateTextInput(session, "study_name", value = study_name())
      })
      observeEvent(input$study_name, {
        if (!identical(input$study_name, study_name())) study_name(input$study_name)
      })
    }

    observeEvent(input$open_replacement, if (!is.null(on_article)) on_article("replacement_protocol"))

    is_arm <- reactive(input$design %in% EVALUATION_ARM_DESIGNS)
    several <- reactive(identical(input$indicator_count, "several"))

    observeEvent(input$design, {
      label <- switch(input$design,
        independent = "Proportion in group 1 (comparison / baseline, p₁)",
        panel = "Baseline proportion (p₁)",
        "Treatment group baseline proportion"
      )
      updateNumericInput(session, "p1", label = label)
      updateNumericInput(session, "mean1", label = switch(input$design,
        independent = "Mean in group 1 (comparison / baseline)", panel = "Baseline mean", "Treatment group baseline mean"))
      # ANCOVA gains from rho; 0.3 is a typical household survey value
      if (input$design == "ancova" && isTRUE(input$rho == 0)) updateNumericInput(session, "rho", value = 0.3)
    })

    # Scales offered for the change: d for means, the second value for one indicator in two-value designs
    observe({
      req(input$design)
      choices <- list("Difference (points or units)" = "absolute", "Relative change (% of baseline)" = "relative")
      if (several() || identical(input$outcome, "mean")) choices[["Standardised effect (Cohen's d)"]] <- "d"
      if (!several() && input$design %in% c("independent", "panel")) {
        choices[[if (identical(input$outcome, "mean")) "Second value (mean 2)" else "Second value (p₂)"]] <- "value2"
      }
      current <- isolate(input$effect_scale)
      updateRadioButtons(session, "effect_scale", choices = choices, inline = TRUE,
                         selected = if (!is.null(current) && current %in% unlist(choices)) current else "absolute")
    })

    output$design_help <- renderUI({
      text <- switch(input$design,
        independent = "Two separate samples compared once: treatment vs control at one point in time, or a baseline and an endline survey that each draw a new sample. Group sizes below are per group.",
        panel = "One sample of households interviewed at baseline and again at endline. The correlation between rounds (ρ) reduces the sample; attrition (households lost by endline) increases it.",
        did_cross = "Treatment and control areas, each surveyed at baseline and endline with new samples (four groups). The effect is the change in treatment minus the change in control.",
        did_panel = "Treatment and control households, each interviewed at baseline and endline. The effect is the change in treatment minus the change in control. When ρ is below 0.5, an ANCOVA analysis needs fewer households.",
        ancova = "Treatment and control households interviewed at baseline and endline; the endline outcomes are compared, controlling for each household's baseline value. Needs fewer households than difference-in-differences: the variance factor is 1 - ρ² against 2(1 - ρ)."
      )
      p(text, class = "text-muted small mb-0")
    })

    # Editable tables: indicators and subgroups
    indicators <- reactiveVal(eval_default_indicators())
    subgroups <- reactiveVal(eval_default_subgroups())

    edit_table <- function(store, edit, text_cols) {
      tab <- store()
      col <- names(tab)[edit$col + 1]
      value <- edit$value
      if (col %in% text_cols) {
        if (col == "Type") {
          v <- tolower(trimws(value))
          value <- if (startsWith(v, "m")) "mean" else if (startsWith(v, "p")) "proportion" else {
            showNotification("Type must be proportion or mean", type = "warning")
            tab$Type[edit$row]
          }
        }
        tab[edit$row, col] <- value
      } else {
        tab[edit$row, col] <- suppressWarnings(as.numeric(value))
      }
      store(tab)
    }
    observeEvent(input$indicators_table_cell_edit, edit_table(indicators, input$indicators_table_cell_edit, c("Indicator", "Type")))
    observeEvent(input$subgroups_table_cell_edit, edit_table(subgroups, input$subgroups_table_cell_edit, "Subgroup"))
    observeEvent(input$add_indicator, {
      tab <- indicators()
      new <- tab[1, ]
      new[1, ] <- NA
      new$Indicator <- paste("Indicator", nrow(tab) + 1)
      new$Type <- "proportion"
      indicators(rbind(tab, new))
    })
    observeEvent(input$remove_indicator, if (nrow(indicators()) > 1) indicators(indicators()[-nrow(indicators()), ]))
    observeEvent(input$add_subgroup, {
      tab <- subgroups()
      subgroups(rbind(tab, data.frame(Subgroup = paste("Subgroup", nrow(tab) + 1), Share = NA_real_, Fixed = NA_real_)))
    })
    observeEvent(input$remove_subgroup, if (nrow(subgroups()) > 1) subgroups(subgroups()[-nrow(subgroups()), ]))

    # The Change column is read on the chosen scale: when the scale changes, convert
    # the values so each indicator keeps the same change (points stay points)
    table_scale <- "absolute"
    observeEvent(input$effect_scale, {
      new_scale <- if (identical(input$effect_scale, "value2")) "absolute" else input$effect_scale
      if (new_scale != table_scale) {
        indicators(evaluation_convert_change(indicators(), table_scale, new_scale))
        if (several()) showNotification("The Change column was converted to the new scale.", type = "message")
        table_scale <<- new_scale
      }
    }, ignoreInit = TRUE)
    change_label <- reactive(switch(if (is.null(input$effect_scale)) "absolute" else input$effect_scale,
      relative = "Change (% of baseline)", d = "Change (d for means, points for proportions)", "Change (points or units)"))

    editable_table <- function(tab, labels) {
      DT::datatable(tab, colnames = unname(labels[names(tab)]), rownames = FALSE, selection = "none",
                    editable = list(target = "cell"),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    }
    output$indicators_table <- DT::renderDataTable({
      labels <- EVAL_INDICATOR_COLUMNS
      labels[["Change"]] <- change_label()
      editable_table(indicators(), labels)
    })
    output$subgroups_table <- DT::renderDataTable(editable_table(subgroups(), EVAL_SUBGROUP_COLUMNS))

    args <- reactive({
      req(input$design, input$eval_mode, input$indicator_count)
      is_mean <- identical(input$outcome, "mean") && !several()
      scale <- if (input$eval_mode == "sample_size") input$effect_scale else "absolute"
      if (is.null(scale)) scale <- "absolute"
      use_p2 <- input$eval_mode == "sample_size" && identical(scale, "value2") && input$design %in% c("independent", "panel")
      sg <- NULL
      if (isTRUE(input$use_subgroups)) {
        sg <- subgroups()
        sg$Share <- sg$Share / 100
      }
      list(
        design = input$design,
        outcome = if (is_mean) "mean" else "proportion",
        indicator_name = trimws(if (is.null(input$indicator_name)) "" else input$indicator_name),
        p1 = if (is_mean) input$mean1 else input$p1,
        sd = input$sd,
        p2 = if (use_p2) (if (is_mean) input$mean2 else input$p2) else NA,
        effect_scale = if (use_p2) "absolute" else scale,
        effect = if (is_mean) input$effect_mean else input$effect,
        effect_relative = input$effect_relative / 100,
        effect_d = input$effect_d,
        direction = input$direction,
        p_control = if (is_mean) input$mean_control else input$p_control,
        control_change = if (is_mean) input$control_change_mean else input$control_change,
        alpha = input$alpha,
        power = input$power,
        sided = as.numeric(input$sided),
        pooled = identical(input$variance_method, "pooled"),
        ratio = input$ratio,
        rho = input$rho,
        response_rate = input$response_rate / 100,
        attrition = input$attrition / 100,
        clustered = isTRUE(input$clustered),
        deff_method = if (is.null(input$deff_method)) "icc" else input$deff_method,
        icc = input$icc,
        m = input$m,
        cv_cluster = input$cv_cluster,
        deff_known = input$deff_known,
        inaccessible = if (is.null(input$inaccessible) || is.na(input$inaccessible)) 0 else input$inaccessible / 100,
        small_cluster_t = !isFALSE(input$small_cluster_t),
        weighting = isTRUE(input$weighting),
        cv_weights = input$cv_weights,
        population = input$population,
        matching_factor = if (isTRUE(input$matching)) input$matching_factor else 1,
        indicators = if (several()) indicators() else NULL,
        bonferroni = several() && isTRUE(input$bonferroni),
        subgroups = sg,
        subgroup_goal = if (is.null(input$subgroup_goal)) "overall" else input$subgroup_goal,
        clusters_per_group = input$clusters_per_group,
        n_per_group = input$n_per_group
      )
    })

    result <- reactive({
      a <- args()
      res <- tryCatch(
        if (input$eval_mode == "mde") evaluation_mde(a) else evaluation_sample_size(a),
        error = function(e) e
      )
      if (inherits(res, "error")) validate(need(FALSE, conditionMessage(res)))
      res
    })

    headline_box <- function(value, label) {
      div(class = "col-md-3 col-6 text-center", div(class = "headline-value", value), div(class = "headline-label", label))
    }

    output$headline <- renderUI({
      res <- result()
      adj <- res$adj
      a <- res$args
      arms <- a$design %in% EVALUATION_ARM_DESIGNS
      first_label <- switch(a$design,
        independent = "Sample per group",
        panel = "Households at baseline",
        did_cross = "Sample per group (each arm, each round)",
        "Households per arm"
      )
      if (a$design != "panel" && !isTRUE(all.equal(eval_opt(a$ratio, 1), 1))) {
        first_label <- paste(first_label, "(group 1 / treatment)")
      }
      boxes <- if (res$mode == "sample_size") {
        list(
          headline_box(format(res$n_per_group, big.mark = ","), first_label),
          headline_box(format(res$total_sample, big.mark = ","), if (adj$panel) "Total households" else "Total sample"),
          if (adj$clustered) headline_box(format(res$groups$Clusters[1], big.mark = ","), "Clusters per group") else
            headline_box(format(res$groups$Completed_Needed[1], big.mark = ","), "Completed interviews needed per group"),
          headline_box(paste0(round(res$power_achieved * 100, 1), "%"), "Power at this sample")
        )
      } else {
        at_mde <- if (arms) res$p[["t2"]] else res$p[["p2"]]
        list(
          headline_box(eval_effect_label(a, res$mde), "Minimum detectable difference"),
          headline_box(if (eval_is_mean(a)) format(signif(at_mde, 4)) else round(at_mde, 3),
                       if (arms) "Treatment endline value at the MDE" else "Second value at the MDE"),
          headline_box(format(round(res$n_eff), big.mark = ","), "Effective completed interviews per group"),
          headline_box(format(res$total_interviews, big.mark = ","), "Total interviews (all rounds)")
        )
      }
      tagList(
        div(class = "row", boxes),
        if (res$n_tests > 1 && res$mode == "sample_size") {
          p(class = "text-center small mb-0 mt-1", "Set by the indicator that needs the largest sample: ", tags$strong(res$indicator_name))
        }
      )
    })

    output$warnings <- renderUI({
      w <- result()$warnings
      if (length(w) == 0) return(NULL)
      div(class = "alert alert-warning small mt-2 mb-0", tags$ul(class = "mb-0", lapply(w, tags$li)))
    })

    output$explanation <- renderText({
      evaluation_report_text(result(), if (is.null(input$study_name)) "" else trimws(input$study_name))
    })

    # Indicator, subgroup and alternative-analysis tables, formatted for display
    display_tables <- reactive({
      res <- result()
      a <- res$args
      out <- list()
      if (!is.null(res$indicators)) {
        ind <- res$indicators
        if ("MDE" %in% names(ind)) {
          is_m <- ind$Type == "Mean"
          ind$MDE <- ifelse(is_m, format(signif(ind$MDE, 4)), paste0(round(ind$MDE * 100, 1), " pts"))
        }
        out$indicators <- ind
      }
      if (!is.null(res$subgroups)) {
        sg <- res$subgroups
        sg$MDE <- vapply(sg$MDE, function(x) if (is.na(x)) "Cannot detect" else eval_effect_label(a, x), character(1))
        names(sg)[names(sg) == "Share_pct"] <- "Share (%)"
        out$subgroups <- sg
      }
      if (!is.null(res$alternatives)) {
        alt <- res$alternatives
        if ("MDE" %in% names(alt)) alt$MDE <- vapply(alt$MDE, function(x) eval_effect_label(a, x), character(1))
        out$alternatives <- alt
      }
      out
    })

    output$detail_tables <- renderUI({
      tabs <- display_tables()
      card <- function(title, body, note = NULL) {
        div(class = "card", div(class = "card-header", h5(title, class = "mb-0")),
            div(class = "card-body", if (!is.null(note)) p(note, class = "text-muted small"), body))
      }
      tagList(
        if (!is.null(tabs$indicators)) card("Key Indicators", eval_html_table(tabs$indicators),
                                            "Each indicator sized (or its MDE found) under the same design and assumptions."),
        if (!is.null(tabs$subgroups)) card("Subgroups", eval_html_table(tabs$subgroups),
                                           "Subgroups are spread over the same clusters, so each has fewer interviews per cluster."),
        if (!is.null(tabs$alternatives)) card("Analysis Comparison (McKenzie 2012)", eval_html_table(tabs$alternatives),
                                              "Same households and assumptions; only the analysis changes. ANCOVA needs a baseline, as panel DiD does.")
      )
    })

    output$power_curve <- renderPlotly({
      res <- result()
      curve <- evaluation_power_curve(res)
      is_mean <- eval_is_mean(res$args)
      if (res$mode == "sample_size") {
        x_mark <- res$n_per_group
        x_title <- "Sample per group (group 1)"
        hover <- paste0("n = ", curve$x, "<br>Power: ", round(curve$power * 100, 1), "%")
      } else {
        if (!is_mean) curve$x <- curve$x * 100
        x_mark <- if (is_mean) res$mde else res$mde * 100
        x_title <- if (is_mean) "True difference (units)" else "True difference (percentage points)"
        hover <- paste0("Difference: ", signif(curve$x, 3), if (is_mean) "" else " pts", "<br>Power: ", round(curve$power * 100, 1), "%")
      }
      plot_ly(x = curve$x, y = curve$power, type = "scatter", mode = "lines", line = list(color = "#2E86AB", width = 2),
              hovertext = hover, hoverinfo = "text") %>%
        plotly::layout(
          title = list(text = if (res$mode == "sample_size") "Power by sample size" else "Power by true difference"),
          xaxis = list(title = x_title),
          yaxis = list(title = "Power", tickformat = ".0%", range = c(0, 1.02)),
          shapes = list(plotly_reference_line(y = res$args$power),
                        plotly_reference_line(x = x_mark, color = "#730202", dash = "dot")),
          margin = list(t = 50)
        )
    })

    groups_display <- reactive({
      g <- result()$groups
      g$multiplier <- NULL
      g$rounds <- NULL
      if ("Clusters_Needed" %in% names(g) && identical(g$Clusters_Needed, g$Clusters)) g$Clusters_Needed <- NULL
      names(g) <- gsub("_", " ", names(g))
      names(g)[names(g) == "Clusters"] <- "Clusters to select"
      names(g)[names(g) == "Interviews"] <- "Interviews (all rounds)"
      g
    })

    output$groups_table <- DT::renderDataTable({
      g <- groups_display()
      DT::datatable(g, options = list(dom = 't', ordering = FALSE, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatRound(which(vapply(g, is.numeric, logical(1))), digits = 0) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })

    output$steps_table <- DT::renderDataTable({
      DT::datatable(result()$steps, options = list(pageLength = 60, dom = 't', ordering = FALSE, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })

    study_label <- function() if (is.null(input$study_name) || input$study_name == "") "" else trimws(input$study_name)
    file_stem <- function(default) {
      if (study_label() != "") paste0(gsub("[^A-Za-z0-9_-]", "_", study_label()), "_Evaluation_Sample") else default
    }
    result_line <- function(res) {
      if (res$mode == "mde") {
        paste0("MDE = ", evaluation_format_effect(res$args, res$mde))
      } else {
        paste0(res$n_per_group, " per group, ", res$total_sample, " in total")
      }
    }

    output$download_excel <- downloadHandler(
      filename = function() paste0(file_stem("evaluation_sample_size"), "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        res <- result()
        summary <- data.frame(
          Item = c("Study name", "Calculation", "Design", "Result", "Date", "App version"),
          Value = c(
            if (study_label() == "") "Not specified" else study_label(),
            if (res$mode == "mde") "Minimum detectable difference" else "Sample size",
            names(EVALUATION_DESIGNS)[EVALUATION_DESIGNS == res$args$design],
            result_line(res),
            as.character(Sys.Date()),
            if (exists("app_version")) app_version() else ""
          ),
          stringsAsFactors = FALSE
        )
        curve <- evaluation_power_curve(res)
        names(curve) <- c(if (res$mode == "mde") "True difference" else "Sample per group", "Power")
        tabs <- display_tables()
        sheets <- list(Summary = summary,
                       `Methods Text` = data.frame(Text = evaluation_report_text(res, study_label())),
                       Calculation = res$steps, Groups = groups_display())
        if (!is.null(tabs$indicators)) sheets$Indicators <- tabs$indicators
        if (!is.null(tabs$subgroups)) sheets$Subgroups <- tabs$subgroups
        if (!is.null(tabs$alternatives)) sheets$`Analysis Comparison` <- tabs$alternatives
        if (length(res$warnings) > 0) sheets$Warnings <- data.frame(Warning = res$warnings)
        sheets$`Power Curve` <- curve
        writexl::write_xlsx(sheets, file)
      }
    )

    output$download_pdf <- downloadHandler(
      filename = function() paste0(file_stem("evaluation_report"), "_", Sys.Date(), ".pdf"),
      content = function(file) {
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(evaluation_report_rmd(result(), study_label(), groups_display(), display_tables()), temp_rmd, useBytes = TRUE)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }
    )

    # Settings sent from the Decision Tree
    handover <- reactiveVal(NULL)
    output$handover_note <- renderUI(handover_note_ui(handover(), session$ns("dismiss_handover")))
    observeEvent(input$dismiss_handover, handover(NULL))

    list(apply_handover = function(settings, design_name) {
      updateSelectInput(session, "design", selected = settings$design)
      updateCheckboxInput(session, "clustered", value = settings$clustered)
      if (settings$clustered) {
        updateRadioButtons(session, "deff_method", selected = "icc")
        updateNumericInput(session, "m", value = settings$m)
        updateNumericInput(session, "icc", value = settings$icc)
      }
      if (!is.null(settings$rho)) updateNumericInput(session, "rho", value = settings$rho)
      handover(list(design = design_name, details = c(
        paste("design:", names(EVALUATION_DESIGNS)[EVALUATION_DESIGNS == settings$design]),
        if (settings$clustered) sprintf("clustered, %d interviews per cluster and ICC %.2f (a common starting point)", settings$m, settings$icc)
        else "not clustered",
        if (!is.null(settings$rho)) sprintf("baseline-endline correlation ρ %.2f (typical for household outcomes; use earlier panel data if you have it)", settings$rho)
      )))
    })
  })
}

#' R Markdown source of the evaluation PDF report
evaluation_report_rmd <- function(res, study = "", groups = res$groups, tables = list()) {
  yaml_title <- gsub("'", "''", ascii_text(if (study != "") paste(study, "- Evaluation Sample Size") else "Evaluation Sample Size"))
  section <- function(title, df) if (is.null(df)) "" else paste0("\n\n## ", title, "\n\n", markdown_table(df))
  result <- if (res$mode == "mde") {
    paste0("Minimum detectable difference: **", ascii_text(evaluation_format_effect(res$args, res$mde)), "**")
  } else {
    paste0("**", format(res$n_per_group, big.mark = ","), "** per group, **", format(res$total_sample, big.mark = ","),
           "** in total", if (res$adj$panel) paste0(" (", format(res$total_interviews, big.mark = ","), " interviews over both rounds)") else "")
  }
  paste0(
    "---\n",
    "title: '", yaml_title, "'\n",
    "subtitle: 'ATR Consulting'\n",
    "date: '", Sys.Date(), "'\n",
    "output: pdf_document\n",
    "---\n\n",
    "## Study Information\n\n",
    "**Study Name:** ", if (study != "") ascii_text(study) else "Not specified", "\n\n",
    "**Design:** ", ascii_text(names(EVALUATION_DESIGNS)[EVALUATION_DESIGNS == res$args$design]), "\n\n",
    "## Result\n\n", result, "\n\n",
    "## Methods Text\n\n", ascii_text(evaluation_report_text(res, study)),
    if (length(res$warnings) > 0) paste0("\n\n## Points to Check\n\n", paste0("- ", ascii_text(res$warnings), collapse = "\n")) else "",
    section("Key Indicators", tables$indicators),
    section("Subgroups", tables$subgroups),
    section("Analysis Comparison", tables$alternatives),
    section("Sample by Group", groups),
    section("Detailed Calculations", res$steps),
    "\n\n*Generated by the ATR Sampling Tool", if (exists("app_version")) paste0(" (version ", app_version(), ")") else "", ".*\n"
  )
}
