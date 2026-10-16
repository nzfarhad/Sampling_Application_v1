# Tests for the decision tree recommendations (modules/decision_tree_module.R)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  source(file.path(root, "sample_size_functions.R"))   # ascii_text()
  source(file.path(root, "modules", "decision_tree_module.R"))
})

ask <- function(...) {
  recommend_sampling(modifyList(list(survey_mode = "in_person", generalization = "yes", subgroup_comparison = "no",
                                     population_data = "complete", num_communities = "less_70",
                                     constraints = "no_constraints"), list(...)))
}

all_answers <- expand.grid(lapply(DECISION_CHOICES, unname), stringsAsFactors = FALSE)

test_that("decision tree: every answer combination gives at least one known method", {
  for (i in seq_len(nrow(all_answers))) {
    rec <- recommend_sampling(as.list(all_answers[i, ]))
    expect_gt(length(rec$methods), 0)
    expect_true(all(rec$methods %in% names(SAMPLING_METHODS)))
  }
})

test_that("decision tree: no generalisation never gives a probability method, phone included", {
  probability <- names(SAMPLING_METHODS)[vapply(SAMPLING_METHODS, function(m) m$category == "Probability", logical(1))]
  no_gen <- all_answers[all_answers$generalization == "no", ]
  for (i in seq_len(nrow(no_gen))) {
    expect_false(any(recommend_sampling(as.list(no_gen[i, ]))$methods %in% probability))
  }
  expect_false(any(ask(survey_mode = "phone", generalization = "no")$methods %in% c("simple_random", "rdd")))
})

test_that("decision tree: snowball only for hidden populations, convenience only on a tight budget", {
  for (i in seq_len(nrow(all_answers))) {
    a <- as.list(all_answers[i, ])
    rec <- recommend_sampling(a)
    if ("snowball" %in% rec$methods) expect_equal(a$population_data, "hidden")
    if ("convenience" %in% rec$methods) expect_equal(a$constraints, "limited_budget")
  }
})

test_that("decision tree: key branches and caveats", {
  expect_equal(ask()$methods, c("simple_random", "systematic"))
  expect_equal(ask(subgroup_comparison = "yes")$methods, "stratified")
  expect_equal(ask(constraints = "remote_insecure")$methods, "cluster")
  expect_equal(ask(num_communities = "more_70", subgroup_comparison = "yes")$methods, "stratified_cluster")
  expect_equal(ask(num_communities = "all")$methods, "site_census")
  expect_true(any(grepl("Coverage warning", ask(population_data = "partial")$notes)))
  rdd <- ask(survey_mode = "phone", population_data = "none")
  expect_equal(rdd$methods, "rdd")
  expect_true(any(grepl("Afghanistan", rdd$notes)))
  expect_equal(ask(survey_mode = "phone", subgroup_comparison = "yes")$methods, "stratified")
})

test_that("decision tree: readable labels and a PDF with quotes in the study name", {
  expect_equal(decision_label("num_communities", "less_70"), "Fewer than about 70")
  skip_if_not(rmarkdown::pandoc_available())
  testServer(decision_tree_server, {
    session$setInputs(study_name = "Ghor \"PDM\" O'Neil's", survey_mode = "phone", generalization_required = "yes",
                      subgroup_comparison = "no", population_data = "none", num_communities = "less_70",
                      logistical_constraints = "no_constraints", analyze = 1)
    expect_equal(selected_methods(), "rdd")
    path <- output$download_report
    expect_true(file.exists(path))
    expect_gt(file.size(path), 1000)
  })
})

test_that("decision tree: results follow changed answers after the first analysis", {
  testServer(decision_tree_server, {
    session$setInputs(study_name = "", survey_mode = "in_person", generalization_required = "yes",
                      subgroup_comparison = "no", population_data = "complete", num_communities = "less_70",
                      logistical_constraints = "no_constraints")
    expect_null(recommendation())
    session$setInputs(analyze = 1)
    expect_equal(selected_methods(), c("simple_random", "systematic"))
    session$setInputs(subgroup_comparison = "yes")
    expect_equal(selected_methods(), "stratified")
    expect_equal(recommendation()$params$subgroup_comparison, "yes")
  })
})
