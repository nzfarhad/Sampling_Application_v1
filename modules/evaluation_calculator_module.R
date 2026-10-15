# Evaluation Calculator Module
# Sample size and minimum detectable difference for comparing proportions:
# two independent groups, panel (same households twice) and difference-in-differences.
# The formulas are in sample_size_functions.R.

evaluation_calculator_ui <- function(id) {
  ns <- NS(id)
  panel_designs <- "input.design == 'panel' || input.design == 'did_panel'"
  did_designs <- "input.design == 'did_cross' || input.design == 'did_panel'"

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
          p("Sample size, or the smallest difference you can detect, when comparing proportions between groups or survey rounds.",
            class = "text-center text-muted")
        )
      ),

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

        # Test and indicator
        div(
          class = "col-md-6",
          div(
            class = "card",
            div(class = "card-header", h5("Test and Indicator", class = "mb-0")),
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
              conditionalPanel(
                condition = "input.design == 'independent'",
                ns = ns,
                radioButtons(
                  ns("variance_method"), "Variance",
                  choices = list("Unpooled (standard)" = "unpooled", "Pooled (Fleiss; matches R power.prop.test)" = "pooled"),
                  selected = "unpooled", inline = TRUE
                )
              ),
              hr(class = "my-2"),
              div(
                class = "row g-2",
                div(class = "col-md-6",
                    numericInput(ns("p1"), "Proportion in group 1 (comparison / baseline, p₁)", value = 0.5,
                                 min = 0.01, max = 0.99, step = 0.01)),
                conditionalPanel(
                  condition = did_designs,
                  ns = ns,
                  class = "col-md-6",
                  numericInput(ns("p_control"), "Control group baseline proportion", value = 0.5, min = 0.01, max = 0.99, step = 0.01)
                )
              ),
              conditionalPanel(
                condition = paste0("input.eval_mode == 'sample_size' && !(", did_designs, ")"),
                ns = ns,
                radioButtons(ns("specify_by"), "Specify the expected result as",
                             choices = list("A difference (D)" = "difference", "The second proportion (p₂)" = "p2"),
                             selected = "difference", inline = TRUE)
              ),
              div(
                class = "row g-2",
                conditionalPanel(
                  condition = paste0("input.eval_mode == 'sample_size' && (input.specify_by != 'p2' || ", did_designs, ")"),
                  ns = ns,
                  class = "col-md-6",
                  numericInput(ns("effect"), "Detectable difference (D, 0.10 = 10 points)", value = 0.10,
                               min = 0.005, max = 0.9, step = 0.01)
                ),
                conditionalPanel(
                  condition = paste0("input.eval_mode == 'sample_size' && input.specify_by == 'p2' && !(", did_designs, ")"),
                  ns = ns,
                  class = "col-md-6",
                  numericInput(ns("p2"), "Second proportion (p₂)", value = 0.6, min = 0.01, max = 0.99, step = 0.01)
                ),
                conditionalPanel(
                  condition = paste0("input.eval_mode == 'mde' || input.specify_by != 'p2' || ", did_designs),
                  ns = ns,
                  class = "col-md-6",
                  radioButtons(ns("direction"), "Expected change",
                               choices = list("Increase" = "increase", "Decrease (e.g. stunting)" = "decrease"),
                               selected = "increase", inline = TRUE)
                )
              ),
              conditionalPanel(
                condition = did_designs,
                ns = ns,
                numericInput(ns("control_change"), "Change expected without the programme (control group trend, e.g. 0.02 or -0.03)",
                             value = 0, min = -0.5, max = 0.5, step = 0.01)
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

        # Field assumptions and clustering
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
                p("ρ is the correlation between a household's baseline and endline answers. Use 0 unless earlier panel ",
                  "data from a similar population shows otherwise: a higher ρ shrinks the sample, and overstating it ",
                  "leaves the study underpowered.", class = "text-muted small")
              ),
              hr(class = "my-2"),
              checkboxInput(ns("clustered"), "Clustered sample (e.g. several households per village)", value = TRUE),
              conditionalPanel(
                condition = "input.clustered",
                ns = ns,
                div(
                  class = "row g-2",
                  div(class = "col-md-6",
                      numericInput(ns("m"), "Interviews per cluster (m)", value = 10, min = 1, step = 1)),
                  div(class = "col-md-6",
                      numericInput(ns("icc"), "Intraclass correlation (ICC)", value = 0.06, min = 0, max = 1, step = 0.01))
                ),
                p("ICC is how alike households in the same cluster are. Household indicators often fall between 0.02 and 0.10; ",
                  "services shared by a whole village (water source, school access) can be 0.2 or more.",
                  class = "text-muted small")
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

      # Results
      div(
        class = "card border-success",
        div(class = "card-header bg-success text-white", h5("Result", class = "mb-0")),
        div(
          class = "card-body",
          uiOutput(ns("headline")),
          p(textOutput(ns("explanation"), inline = TRUE), class = "text-muted mt-2 mb-0")
        )
      ),

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
                downloadButton(ns("download_excel"), "Excel", class = "btn btn-outline-success btn-sm"))
          )
        )
      )
    )
  )
}

evaluation_calculator_server <- function(id, study_name = NULL) {
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

    did <- reactive(input$design %in% c("did_cross", "did_panel"))

    observeEvent(input$design, {
      label <- switch(input$design,
        independent = "Proportion in group 1 (comparison / baseline, p₁)",
        panel = "Baseline proportion (p₁)",
        "Treatment group baseline proportion"
      )
      updateNumericInput(session, "p1", label = label)
    })

    output$design_help <- renderUI({
      text <- switch(input$design,
        independent = "Two separate samples compared once: treatment vs control at one point in time, or a baseline and an endline survey that each draw a new sample. Group sizes below are per group.",
        panel = "One sample of households interviewed at baseline and again at endline. The correlation between rounds (ρ) reduces the sample; attrition (households lost by endline) increases it.",
        did_cross = "Treatment and control areas, each surveyed at baseline and endline with new samples (four groups). The effect is the change in treatment minus the change in control.",
        did_panel = "Treatment and control households, each interviewed at baseline and endline. The effect is the change in treatment minus the change in control."
      )
      p(text, class = "text-muted small mb-0")
    })

    args <- reactive({
      req(input$design, input$eval_mode, input$p1)
      use_p2 <- input$eval_mode == "sample_size" && identical(input$specify_by, "p2") && !did()
      list(
        design = input$design,
        p1 = input$p1,
        p2 = if (use_p2) input$p2 else NA,
        effect = input$effect,
        direction = input$direction,
        p_control = input$p_control,
        control_change = input$control_change,
        alpha = input$alpha,
        power = input$power,
        sided = as.numeric(input$sided),
        pooled = identical(input$variance_method, "pooled"),
        ratio = input$ratio,
        rho = input$rho,
        response_rate = input$response_rate / 100,
        attrition = input$attrition / 100,
        clustered = isTRUE(input$clustered),
        icc = input$icc,
        m = input$m,
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
      div(class = "col-md-3 text-center", div(class = "headline-value", value), div(class = "headline-label", label))
    }

    output$headline <- renderUI({
      res <- result()
      adj <- res$adj
      first_label <- switch(res$args$design,
        independent = "Sample per group",
        panel = "Households at baseline",
        did_cross = "Sample per group (each arm, each round)",
        did_panel = "Households per arm"
      )
      if (res$args$design %in% c("independent", "did_cross", "did_panel") && !isTRUE(all.equal(res$args$ratio, 1))) {
        first_label <- paste(first_label, "(group 1 / treatment)")
      }
      boxes <- if (res$mode == "sample_size") {
        list(
          headline_box(format(res$n_per_group, big.mark = ","), first_label),
          headline_box(format(res$total_sample, big.mark = ","), if (adj$panel) "Total households" else "Total sample"),
          if (adj$clustered) headline_box(res$groups$Clusters[1], "Clusters per group") else
            headline_box(res$groups$Completed_Needed[1], "Completed interviews needed per group"),
          headline_box(paste0(round(res$power_achieved * 100, 1), "%"), "Power at this sample")
        )
      } else {
        list(
          headline_box(paste0(round(res$mde * 100, 1), " pts"), "Minimum detectable difference"),
          headline_box(if (did()) round(res$p[["t2"]], 3) else round(res$p[["p2"]], 3),
                       if (did()) "Treatment endline proportion at the MDE" else "p₂ at the MDE"),
          headline_box(format(round(res$n_eff), big.mark = ","), "Effective completed interviews per group"),
          headline_box(format(res$total_interviews, big.mark = ","), "Total interviews (all rounds)")
        )
      }
      div(class = "row", boxes)
    })

    output$explanation <- renderText({
      res <- result()
      a <- res$args
      adj <- res$adj
      test <- paste0(if (a$sided == 2) "two-sided" else "one-sided", " test at α = ", a$alpha, " with ",
                     round(a$power * 100), "% power")
      cluster_txt <- if (adj$clustered) {
        paste0(" Clustering adds a design effect of ", round(adj$deff, 2), " (", adj$m, " interviews per cluster, ICC ", adj$icc, ").")
      } else ""
      loss_txt <- paste0(" It allows for a ", round(a$response_rate * 100), "% response rate",
                         if (adj$panel) paste0(" and ", round(adj$attrition * 100), "% attrition by endline") else "", ".")
      if (res$mode == "sample_size") {
        paste0("To detect a difference of ", round(abs(res$terms$effect) * 100, 1), " percentage points (",
               if (did()) paste0("difference-in-differences; treatment ", round(res$p[["t1"]], 3), " → ", round(res$p[["t2"]], 3),
                                 ", control ", round(res$p[["c1"]], 3), " → ", round(res$p[["c2"]], 3))
               else paste0(round(res$p[["p1"]], 3), " vs ", round(res$p[["p2"]], 3)),
               ") with a ", test, ", sample ", format(res$n_per_group, big.mark = ","), " ",
               if (adj$panel) "households" else "units", " in group 1",
               if (nrow(res$groups) > 1) paste0(" (", format(res$total_sample, big.mark = ","), " in total)") else "",
               ".", loss_txt, cluster_txt,
               if (adj$panel) paste0(" Each household is interviewed twice, so plan ", format(res$total_interviews, big.mark = ","), " interviews.") else "")
      } else {
        paste0("With ", format(res$n_per_group, big.mark = ","), " units per group, a ", test, " can detect a ",
               if (a$direction == "decrease") "decrease" else "increase", " of ", round(res$mde * 100, 1),
               " percentage points or more.", loss_txt, cluster_txt,
               " Smaller true differences are likely to be missed.")
      }
    })

    output$power_curve <- renderPlotly({
      res <- result()
      curve <- evaluation_power_curve(res)
      if (res$mode == "sample_size") {
        x_mark <- res$n_per_group
        x_title <- "Sample per group (group 1)"
        hover <- paste0("n = ", curve$x, "<br>Power: ", round(curve$power * 100, 1), "%")
      } else {
        curve$x <- curve$x * 100
        x_mark <- res$mde * 100
        x_title <- "True difference (percentage points)"
        hover <- paste0("Difference: ", round(curve$x, 1), " pts<br>Power: ", round(curve$power * 100, 1), "%")
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
      names(g) <- gsub("_", " ", names(g))
      names(g)[names(g) == "Interviews"] <- "Interviews (all rounds)"
      g
    })

    output$groups_table <- DT::renderDataTable({
      DT::datatable(groups_display(), options = list(dom = 't', ordering = FALSE, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })

    output$steps_table <- DT::renderDataTable({
      DT::datatable(result()$steps, options = list(pageLength = 40, dom = 't', ordering = FALSE, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })

    output$download_excel <- downloadHandler(
      filename = function() {
        stem <- if (!is.null(input$study_name) && input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Evaluation_Sample")
        } else {
          "evaluation_sample_size"
        }
        paste0(stem, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        res <- result()
        summary <- data.frame(
          Item = c("Study name", "Calculation", "Design", "Result", "Date", "App version"),
          Value = c(
            if (is.null(input$study_name) || input$study_name == "") "Not specified" else input$study_name,
            if (res$mode == "mde") "Minimum detectable difference" else "Sample size",
            names(EVALUATION_DESIGNS)[EVALUATION_DESIGNS == res$args$design],
            if (res$mode == "mde") paste0("MDE = ", round(res$mde * 100, 2), " percentage points")
            else paste0(res$n_per_group, " per group, ", res$total_sample, " in total"),
            as.character(Sys.Date()),
            if (exists("app_version")) app_version() else ""
          ),
          stringsAsFactors = FALSE
        )
        curve <- evaluation_power_curve(res)
        names(curve) <- c(if (res$mode == "mde") "True difference" else "Sample per group", "Power")
        writexl::write_xlsx(list(Summary = summary, Calculation = res$steps, Groups = groups_display(),
                                 `Power Curve` = curve), file)
      }
    )
  })
}
