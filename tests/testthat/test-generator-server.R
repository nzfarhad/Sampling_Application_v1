# Server-logic tests for the sample generator module (no browser needed)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  library(shinyjs)
  source(file.path(root, "sampling_generation_functions.R"))
  source(file.path(root, "modules", "sampling_generator_module.R"))
})

frame_csv <- function() {
  set.seed(2)
  d <- data.frame(
    id = 1:600,
    province = rep(c("A", "B", "C"), c(300, 200, 100)),
    notes = ifelse(runif(600) < 0.3, NA, "x")
  )
  d$village <- paste0(d$province, "_V", sample(1:10, 600, replace = TRUE))
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  list(name = "frame.csv", datapath = path, size = file.size(path), type = "text/csv")
}

# Inputs every run needs
base_inputs <- function(session) {
  session$setInputs(dataset_file = frame_csv())
  session$setInputs(study_name = "Test study", id_variable = "id", buffer_percentage = 10,
                    systematic_sort_variable = "", strata_variable = "", within_method = "simple_random",
                    allocation_method = "proportional", min_per_stratum = 0, neyman_input = "proportion",
                    cluster_sampling_method = "population_units", cluster_variable = "", cluster_size = 10,
                    use_pps = TRUE)
}

test_that("generator: systematic draw keeps NA rows, labels buffer, records methodology", {
  testServer(sampling_generator_server, {
    base_inputs(session)
    session$setInputs(sampling_method = "systematic", sample_size = 100, random_seed = 42, generate_sample = 1)
    expect_true(frame_check_result()$can_proceed)
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    expect_equal(sum(s$sample_type == "Sample"), 100)
    expect_equal(sum(s$sample_type == "Buffer"), 10)
    expect_true(any(is.na(s$notes)))
    expect_equal(sum(s$base_weight[s$sample_type == "Sample"]), 600)
    m <- methodology()
    expect_equal(m$Value[m$Item == "Master random seed"], "42")
    expect_equal(m$Value[m$Item == "Study name"], "Test study")
    expect_null(cluster_summary())
  })
})

test_that("generator: cluster draw produces a cluster summary", {
  testServer(sampling_generator_server, {
    base_inputs(session)
    session$setInputs(sampling_method = "cluster", sample_size = 100, random_seed = 7,
                      cluster_variable = "village", generate_sample = 1)
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    cs <- cluster_summary()
    expect_equal(sum(cs$times_selected), 10)
    # Small clusters selected twice can fall short; the summary accounts for every unit
    expect_equal(sum(cs$sample_count), sum(s$sample_type == "Sample"))
    expect_equal(sum(cs$required_sample) - sum(cs$shortfall), sum(s$sample_type == "Sample"))
  })
})

test_that("generator: blank seed is replaced by a recorded random seed", {
  testServer(sampling_generator_server, {
    base_inputs(session)
    session$setInputs(sampling_method = "simple_random", sample_size = 50, buffer_percentage = 0,
                      random_seed = NA, generate_sample = 1)
    session$setInputs(confirm_draw = 1)
    expect_equal(nrow(generated_sample()), 50)
    m <- methodology()
    expect_false(is.na(suppressWarnings(as.numeric(m$Value[m$Item == "Master random seed"]))))
  })
})

test_that("generator: stratified draw with manual allocation", {
  testServer(sampling_generator_server, {
    base_inputs(session)
    session$setInputs(sampling_method = "stratified", sample_size = 60, random_seed = 3,
                      strata_variable = "province", allocation_method = "manual")
    alloc <- allocation_inputs()
    expect_equal(alloc$stratum, c("A", "B", "C"))
    allocation_inputs(transform(alloc, input = c(10, 20, 30)))
    session$setInputs(generate_sample = 1)
    expect_true(frame_check_result()$can_proceed)
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    expect_equal(as.vector(table(s$stratum[s$sample_type == "Sample"])), c(10L, 20L, 30L))
    expect_equal(allocation_result()$n_allocated, c(10L, 20L, 30L))
  })
})

test_that("generator: stratified cluster draw and Excel export with all sheets", {
  testServer(sampling_generator_server, {
    base_inputs(session)
    session$setInputs(sampling_method = "stratified_cluster", sample_size = 60, random_seed = 5,
                      strata_variable = "province", cluster_variable = "village", cluster_size = 5,
                      generate_sample = 1)
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    main <- s[s$sample_type == "Sample", ]
    expect_true(all(startsWith(main$cluster_id, main$stratum)))
    expect_true("stratum" %in% names(cluster_summary()))

    path <- output$download_sample_excel
    sheets <- openxlsx::getSheetNames(path)
    expect_true(all(c("Sample", "Methodology", "Allocation", "Cluster Summary", "Frame Checks",
                      "Column Guide", "Replacement Log") %in% sheets))
    expect_equal(nrow(openxlsx::read.xlsx(path, "Sample")), nrow(s))
  })
})

test_that("generator: frame errors block the draw", {
  testServer(sampling_generator_server, {
    base_inputs(session)
    session$setInputs(sampling_method = "simple_random", sample_size = 590, buffer_percentage = 10,
                      random_seed = 1, generate_sample = 1)
    expect_false(frame_check_result()$can_proceed)
    expect_null(generated_sample())
  })
})

# A cluster list: one row per village with its number of households
cluster_list_csv <- function() {
  set.seed(4)
  d <- data.frame(
    province = rep(c("A", "B", "C"), c(40, 30, 20)),
    village = sprintf("V%02d", 1:90),
    households = sample(3:40, 90, replace = TRUE)
  )
  d$village[c(5, 60)] <- "Same name"
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  list(name = "clusters.csv", datapath = path, size = file.size(path), type = "text/csv")
}

list_inputs <- function(session, ...) {
  base_inputs(session)
  session$setInputs(dataset_file = cluster_list_csv())
  session$setInputs(cluster_sampling_method = "cluster_list", cluster_list_variable = "village",
                    cluster_population_variable = "households", cluster_list_size = 5, use_pps_cluster_list = TRUE,
                    id_variable = "", ...)
}

test_that("generator: cluster list gives one row per selected cluster with its interviews", {
  testServer(sampling_generator_server, {
    list_inputs(session, sampling_method = "cluster", sample_size = 100, random_seed = 9)
    session$setInputs(generate_sample = 1)
    issues <- frame_check_result()$issues
    expect_true(any(grepl("Same name [(]row 5[)]", issues$detail)))
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    main <- s$sample_type == "Sample"
    expect_false(any(duplicated(s$cluster_id)))
    expect_equal(sum(s$selection_hits[main]), 20)
    expect_true(all(s$interviews[main] == pmin(5 * s$selection_hits[main], s$households[main])))
    expect_equal(sum(!main), 2)
    # Weights per interview add up to the households in the list
    expect_equal(sum(s$base_weight[main] * s$interviews[main]), sum(read.csv(cluster_list_csv()$datapath)$households))
    cs <- cluster_summary()
    expect_equal(nrow(cs), 90)
    expect_equal(sum(cs$interviews), sum(s$interviews[main]))
    m <- methodology()
    expect_equal(m$Value[m$Item == "Main sample drawn"], as.character(sum(s$interviews[main])))
    expect_equal(m$Value[m$Item == "Clusters selected"], as.character(sum(main)))
    path <- output$download_sample_excel
    guide <- openxlsx::read.xlsx(path, "Column Guide")
    expect_true("interviews" %in% guide$Column)
  })
})

test_that("generator: cluster list without sizes needs equal probability", {
  testServer(sampling_generator_server, {
    list_inputs(session, sampling_method = "cluster", sample_size = 50, random_seed = 2)
    session$setInputs(cluster_population_variable = "", generate_sample = 1)
    expect_null(frame_check_result())
    session$setInputs(use_pps_cluster_list = FALSE, generate_sample = 2)
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    expect_equal(sum(s$sample_type == "Sample"), 10)
    expect_true(all(is.na(s$base_weight)))
    expect_true(all(s$interviews[s$sample_type == "Sample"] == 5))
  })
})

test_that("generator: stratified cluster list allocates by the households in each stratum", {
  testServer(sampling_generator_server, {
    list_inputs(session, sampling_method = "stratified_cluster", sample_size = 120, random_seed = 3,
                strata_variable = "province")
    preview <- allocation_preview()
    expect_equal(preview$labels[2:3], c("Clusters", "Total size"))
    expect_equal(preview$table$frame_units, c(40L, 30L, 20L))
    hh <- read.csv(cluster_list_csv()$datapath)
    expect_equal(preview$table$n_allocated, allocate_sample(as.numeric(tapply(hh$households, hh$province, sum)), 120))
    session$setInputs(generate_sample = 1)
    session$setInputs(confirm_draw = 1)
    s <- generated_sample()
    main <- s$sample_type == "Sample"
    expect_equal(as.vector(tapply(s$selection_hits[main], s$stratum[main], sum)), preview$table$clusters_to_select)
    expect_true(all(s$province == s$stratum))
  })
})
