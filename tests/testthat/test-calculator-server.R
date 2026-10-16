# Server-logic tests for the sample size calculator module

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  library(shinyjs)
  library(plotly)
  source(file.path(root, "sampling_generation_functions.R"))
  source(file.path(root, "sample_size_functions.R"))
  source(file.path(root, "modules", "sample_calculator_module.R"))
})

test_that("calculator: cluster result is whole clusters of m interviews", {
  testServer(sample_calculator_server, {
    session$setInputs(sampling_method = "cluster", population_size = 20000, confidence_level = 0.95,
                      margin_error = 0.05, expected_proportion = 0.5, non_response_rate = 10,
                      cluster_size = 12, intraclass_correlation = 0.06, data_input_method = "manual",
                      cluster_population_column = "", cluster_name_column = "")
    r <- calculate_sample_size()
    expect_equal(r$sample_size, r$n_clusters * 12)
    expect_equal(r$deff, 1 + 11 * 0.06)
    expect_null(r$n_source)
  })
})

test_that("calculator: SRS result unchanged and deff is 1", {
  testServer(sample_calculator_server, {
    session$setInputs(sampling_method = "simple_random", population_size = 10000, confidence_level = 0.95,
                      margin_error = 0.05, expected_proportion = 0.5, non_response_rate = 5,
                      data_input_method = "manual")
    r <- calculate_sample_size()
    expect_equal(r$sample_size, calculate_simple_random_sample_size(10000, 0.5, 0.95, 0.05, 0.05))
    expect_equal(r$deff, 1)
  })
})

test_that("calculator: mean indicator and stratified precision for the total", {
  testServer(sample_calculator_server, {
    session$setInputs(sampling_method = "simple_random", indicator_type = "mean", population_size = 1e7,
                      confidence_level = 0.95, margin_error = 0.05, margin_error_mean = 1, indicator_sd = 10,
                      expected_proportion = 0.5, non_response_rate = 0, data_input_method = "manual")
    expect_equal(calculate_sample_size()$sample_size, 385)

    session$setInputs(sampling_method = "stratified", indicator_type = "proportion", non_response_rate = 5,
                      precision_target = "per_stratum", strata_allocation = "proportional", min_per_stratum = 0)
    per <- calculate_sample_size()
    expect_equal(nrow(per$breakdown), 3)
    session$setInputs(precision_target = "total")
    tot <- calculate_sample_size()
    expect_lt(tot$sample_size, per$sample_size)
    expect_lte(tot$moe_achieved, 0.05)
    expect_true("Allocation_Share_pct" %in% names(tot$breakdown))
    expect_match(output$sample_size_plot, "Total")

    # Downloads: Excel has the breakdown sheet
    path <- output$download_excel
    expect_true("Stratified Breakdown" %in% openxlsx::getSheetNames(path))
  })
})
