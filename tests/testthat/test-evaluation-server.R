# Server-logic tests for the evaluation calculator module (no browser needed)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  library(shinyjs)
  library(plotly)
  source(file.path(root, "sampling_generation_functions.R"))
  source(file.path(root, "sample_size_functions.R"))
  source(file.path(root, "modules", "sample_calculator_module.R"))   # plotly_reference_line()
  source(file.path(root, "modules", "evaluation_calculator_module.R"))
})

eval_inputs <- function(session, ...) {
  session$setInputs(design = "independent", eval_mode = "sample_size", p1 = 0.15, specify_by = "difference",
                    effect = 0.10, p2 = 0.6, direction = "increase", p_control = 0.5, control_change = 0,
                    alpha = 0.05, power = 0.80, sided = "2", variance_method = "unpooled", ratio = 1, rho = 0,
                    response_rate = 80, attrition = 10, clustered = TRUE, icc = 0.10, m = 6,
                    clusters_per_group = 40, n_per_group = 400, study_name = "Eval test")
  session$setInputs(...)
}

test_that("evaluation server: regression case and Excel export", {
  testServer(evaluation_calculator_server, {
    eval_inputs(session)
    r <- result()
    expect_equal(r$n_per_group, 468)
    expect_equal(r$groups$Clusters[1], 78)
    expect_match(output$explanation, "10 percentage points")
    expect_match(output$power_curve, "Power by sample size")

    path <- output$download_excel
    expect_true(all(c("Summary", "Calculation", "Groups", "Power Curve") %in% openxlsx::getSheetNames(path)))
  })
})

test_that("evaluation server: MDE mode, p2 entry and design switches", {
  testServer(evaluation_calculator_server, {
    eval_inputs(session, eval_mode = "mde", clusters_per_group = 78)
    expect_lt(result()$mde, 0.10)
    expect_gt(result()$mde, 0.09)

    eval_inputs(session, specify_by = "p2", p2 = 0.25)
    expect_equal(result()$n_per_group, 468)

    eval_inputs(session, design = "did_panel", rho = 0.4, control_change = 0.02)
    expect_equal(nrow(result()$groups), 2)
    expect_equal(result()$total_interviews, 2 * result()$total_sample)

    # Invalid input gives a validation message, not a crash
    eval_inputs(session, p1 = 0.95)
    expect_error(result(), "between 0 and 1")
  })
})
