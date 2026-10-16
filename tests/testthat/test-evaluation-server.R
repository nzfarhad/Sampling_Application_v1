# Server-logic tests for the evaluation calculator module (no browser needed)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  library(shinyjs)
  library(plotly)
  source(file.path(root, "sampling_generation_functions.R"))
  source(file.path(root, "sample_size_functions.R"))
  source(file.path(root, "modules", "sample_calculator_module.R"))   # plotly_reference_line()
  source(file.path(root, "modules", "decision_tree_module.R"))       # handover_note_ui()
  source(file.path(root, "modules", "evaluation_calculator_module.R"))
})

eval_inputs <- function(session, ...) {
  session$setInputs(design = "independent", eval_mode = "sample_size", indicator_count = "one", outcome = "proportion",
                    indicator_name = "", p1 = 0.15, effect_scale = "absolute", effect = 0.10, p2 = 0.6,
                    mean1 = 40, sd = 12, effect_mean = 3, mean2 = 43, mean_control = 40, control_change_mean = 0,
                    effect_relative = 20, effect_d = 0.25, direction = "increase", p_control = 0.5, control_change = 0,
                    alpha = 0.05, power = 0.80, sided = "2", variance_method = "unpooled", ratio = 1, rho = 0,
                    response_rate = 80, attrition = 10, clustered = TRUE, deff_method = "icc", icc = 0.10, m = 6,
                    cv_cluster = 0.5, deff_known = 1.5, inaccessible = 0, small_cluster_t = TRUE, weighting = FALSE,
                    cv_weights = 0.3, population = NA, matching = FALSE, matching_factor = 1.3, bonferroni = FALSE,
                    use_subgroups = FALSE, subgroup_goal = "overall",
                    clusters_per_group = 40, n_per_group = 400, study_name = "Eval test")
  session$setInputs(...)
}

test_that("evaluation server: regression case, methods text and Excel export", {
  testServer(evaluation_calculator_server, {
    eval_inputs(session)
    r <- result()
    expect_equal(r$n_per_group, 468)
    expect_equal(r$groups$Clusters[1], 78)
    expect_match(output$explanation, "10 percentage points")
    expect_match(output$explanation, "for Eval test")
    expect_match(output$power_curve, "Power by sample size")

    path <- output$download_excel
    expect_true(all(c("Summary", "Methods Text", "Calculation", "Groups", "Power Curve") %in% openxlsx::getSheetNames(path)))
  })
})

test_that("evaluation server: MDE mode, p2 entry and design switches", {
  testServer(evaluation_calculator_server, {
    eval_inputs(session, eval_mode = "mde", clusters_per_group = 78)
    expect_lt(result()$mde, 0.10)
    expect_gt(result()$mde, 0.09)

    eval_inputs(session, effect_scale = "value2", p2 = 0.25)
    expect_equal(result()$n_per_group, 468)

    eval_inputs(session, design = "did_panel", rho = 0.4, control_change = 0.02)
    expect_equal(nrow(result()$groups), 2)
    expect_equal(result()$total_interviews, 2 * result()$total_sample)
    expect_match(paste(as.character(output$detail_tables), collapse = ""), "Analysis Comparison")

    # Invalid input gives a validation message, not a crash
    eval_inputs(session, p1 = 0.95)
    expect_error(result(), "between 0 and 1")
  })
})

test_that("evaluation server: means, several indicators and subgroups", {
  testServer(evaluation_calculator_server, {
    eval_inputs(session, outcome = "mean", indicator_name = "FCS")
    expect_equal(result()$args$outcome, "mean")
    expect_equal(result()$n_base, (qnorm(0.975) + qnorm(0.8))^2 * 2 * 144 / 9)
    expect_match(output$explanation, "FCS is a mean")

    eval_inputs(session, indicator_count = "several")
    r <- result()
    expect_equal(nrow(r$indicators), 2)
    expect_match(paste(as.character(output$headline), collapse = ""), r$indicator_name)
    # Editing a cell changes the indicator table
    session$setInputs(indicators_table_cell_edit = list(row = 2, col = 4, value = "-0.15"))
    expect_equal(indicators()$Change[2], -0.15)
    session$setInputs(add_indicator = 1)
    expect_equal(nrow(indicators()), 3)
    expect_error(result(), "Indicator 3")
    session$setInputs(remove_indicator = 1)
    expect_equal(nrow(result()$indicators), 2)

    eval_inputs(session, use_subgroups = TRUE, subgroup_goal = "within")
    expect_equal(result()$factor, 5)
    expect_match(paste(as.character(output$detail_tables), collapse = ""), "Female-headed")
    path <- output$download_excel
    expect_true(all(c("Subgroups", "Warnings") %in% openxlsx::getSheetNames(path)))
  })
})

test_that("evaluation server: PDF report source", {
  testServer(evaluation_calculator_server, {
    eval_inputs(session, indicator_count = "several", use_subgroups = TRUE, design = "did_panel", rho = 0.3)
    rmd <- evaluation_report_rmd(result(), "Eval test", groups_display(), display_tables())
    expect_match(rmd, "Methods Text")
    expect_match(rmd, "## Key Indicators")
    expect_match(rmd, "## Subgroups")
    expect_match(rmd, "## Analysis Comparison")
    expect_false(grepl("[ρ²√→]", rmd))
  })
})

test_that("evaluation server: PDF renders", {
  skip_if_not(nzchar(Sys.which("pdflatex")) || (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()),
              "No LaTeX")
  testServer(evaluation_calculator_server, {
    eval_inputs(session, outcome = "mean", design = "ancova", rho = 0.3, deff_method = "cv", weighting = TRUE)
    path <- output$download_pdf
    expect_true(file.exists(path))
    expect_gt(file.size(path), 10000)
    unlink(path)
  })
})

test_that("evaluation server: changing the scale converts the indicator table, so the sample stays the same", {
  # Reported case: several indicators, known DEFF 1.5, m 6, matching 1.3, then "Relative change" chosen
  testServer(evaluation_calculator_server, {
    eval_inputs(session, indicator_count = "several", deff_method = "known", deff_known = 1.5, m = 6,
                response_rate = 90, matching = TRUE, matching_factor = 1.3)
    before <- result()$indicators$Sample_per_Group
    expect_equal(before, c(546, 1014))
    session$setInputs(effect_scale = "relative")
    expect_equal(indicators()$Change, c(7.5, -0.08 / 0.3 * 100), tolerance = 1e-5)
    expect_equal(result()$indicators$Sample_per_Group, before)
    expect_length(result()$warnings, 0)
    expect_match(paste(as.character(output$indicators_table), collapse = ""), "Change \\(% of baseline\\)")
    session$setInputs(effect_scale = "d")
    expect_equal(indicators()$Change, c(0.25, -0.08), tolerance = 1e-5)   # proportions are held in points on the d scale
    expect_equal(result()$indicators$Sample_per_Group, before)
    session$setInputs(effect_scale = "absolute")
    expect_equal(indicators()$Change, c(3, -0.08), tolerance = 1e-5)
    expect_equal(result()$indicators$Sample_per_Group, before)

    # A change typed on the wrong scale is flagged
    session$setInputs(effect_scale = "relative", indicators_table_cell_edit = list(row = 2, col = 4, value = "-0.08"))
    expect_true(any(grepl("very small", result()$warnings)))
    expect_match(paste(as.character(output$warnings), collapse = ""), "very large")
  })
})
