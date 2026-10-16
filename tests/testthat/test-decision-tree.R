# Tests for the decision tree (decision_tree_functions.R and modules/decision_tree_module.R)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  source(file.path(root, "sample_size_functions.R"))   # ascii_text(), EVALUATION_DESIGNS
  source(file.path(root, "decision_tree_functions.R"))
  source(file.path(root, "modules", "wiki_module.R"))  # WIKI_ARTICLES
  source(file.path(root, "modules", "decision_tree_module.R"))
})

# A typical in-person monitoring survey; tests change only what they need
ask <- function(...) {
  recommend_design(modifyList(list(purpose = "level", unit = "people", frame = "unit_list", reporting = "overall",
                                   spread = "many", mode = "in_person", constraints = character(0)), list(...)))
}

ch <- lapply(DECISION_CHOICES, unname)
# Every subset of a set of tick boxes, including none
subsets <- function(x) lapply(seq_len(2^length(x)) - 1, function(i) x[bitwAnd(i, 2^(seq_along(x) - 1)) > 0])
# A result without the answers it echoes back
outcome <- function(rec) rec[setdiff(names(rec), "answers")]
key <- function(a) paste(names(a), vapply(a, paste, "", collapse = "+"), sep = "=", collapse = " / ")

# Every answer to the single-choice questions, keeping only the questions that
# apply to it (the others are ignored, tested below), without duplicates
full_grid <- expand.grid(purpose = ch$purpose, unit = ch$unit, frame = ch$frame, spread = ch$spread, mode = ch$mode,
                         comparison = ch$comparison, panel = ch$panel, random = ch$random, stringsAsFactors = FALSE)
full_grid <- lapply(seq_len(nrow(full_grid)), function(i) as.list(full_grid[i, ]))
singles <- lapply(full_grid, function(a) a[intersect(names(a), decision_applicable(a))])
singles <- singles[!duplicated(vapply(singles, key, ""))]

# Each one with every subset of the reporting groups and of the constraints, where
# those questions apply. No group stands for "overall"; ticking "overall" next to
# a group changes nothing (tested below).
group_sets <- lapply(subsets(setdiff(ch$reporting, "overall")), function(g) if (length(g)) g else "overall")
constraint_sets <- subsets(ch$constraints)
multi_sets <- function(a) {
  if (!"reporting" %in% decision_applicable(a)) return(list(list()))
  unlist(lapply(group_sets, function(g) lapply(constraint_sets, function(cs) list(reporting = g, constraints = cs))),
         recursive = FALSE)
}

# Checks run on every combination, in one pass: answers (with defaults filled in) and result
checks <- list(
  known_design = function(a, rec) {
    length(rec$design) == 1 && rec$design %in% names(DESIGNS) && length(rec$stages) > 0 &&
      length(rec$reasons) > 0 && all(rec$protocols %in% names(WIKI_ARTICLES))
  },
  non_probability = function(a, rec) {
    probability <- DESIGNS[[rec$design]]$category == "Probability"
    no_moe <- any(grepl("no margin of error", rec$warnings, fixed = TRUE))
    if (a$purpose %in% c("qualitative", "pilot") || a$frame == "unlistable") !probability && no_moe
    else if (probability) !no_moe
    else TRUE
  }
)
failures <- setNames(rep(list(character(0)), length(checks)), names(checks))
n_checked <- 0
for (s in singles) for (m in multi_sets(s)) {
  a <- c(s, m)
  rec <- recommend_design(a)
  n_checked <- n_checked + 1
  for (k in names(checks)) if (!isTRUE(checks[[k]](rec$answers, rec))) failures[[k]] <- c(failures[[k]], key(a))
}
# No failures; the first few are shown otherwise
expect_no_failures <- function(check) expect(length(failures[[check]]) == 0,
  paste0(length(failures[[check]]), " combinations fail, e.g.\n", paste(utils::head(failures[[check]], 5), collapse = "\n")))

test_that("decision tree: every combination gives one known design with stages and reasons", {
  # pilot: mode (3); qualitative: frame x mode (15); level, verify: unit x frame x spread x mode (60 each);
  # change: 60 x 8 comparison set-ups; all but pilot x 16 group sets x 16 constraint sets
  expect_equal(length(singles), 3 + 15 + 60 + 60 + 60 * 8)
  expect_equal(n_checked, 3 + (15 + 60 + 60 + 60 * 8) * 16 * 16)
  expect_no_failures("known_design")
})

test_that("decision tree: no result until the purpose is answered", {
  expect_null(recommend_design(list()))
  expect_null(recommend_design(list(purpose = "", frame = "none")))
})

test_that("decision tree: questions that do not apply are ignored", {
  # Every single-choice answer, with groups and constraints ticked, gives the same
  # result as the questions that apply to it alone
  ignored <- vapply(full_grid, function(a) {
    a <- c(a, list(reporting = c("area", "gender"), constraints = c("insecure", "budget")))
    rec <- recommend_design(a)
    identical(outcome(recommend_design(a[intersect(names(a), rec$applicable)])), outcome(rec))
  }, logical(1))
  expect_true(all(ignored), info = paste(utils::head(vapply(full_grid[!ignored], key, ""), 5), collapse = "\n"))

  # Ticking "overall" next to any groups changes nothing
  with_overall <- unlist(lapply(Filter(function(s) "reporting" %in% decision_applicable(s), singles), function(s)
    vapply(group_sets[-1], function(g) identical(outcome(recommend_design(c(s, list(reporting = c("overall", g))))),
                                                 outcome(recommend_design(c(s, list(reporting = g))))), logical(1))))
  expect_true(all(with_overall))

  expect_setequal(decision_applicable(list(purpose = "pilot")), c("purpose", "mode"))
  expect_true("random" %in% decision_applicable(list(purpose = "change", comparison = "groups_both")))
  expect_false("panel" %in% decision_applicable(list(purpose = "change", comparison = "groups_endline")))
})

test_that("decision tree: no random selection gives no probability design and a warning", {
  expect_no_failures("non_probability")
})

test_that("decision tree: named scenarios", {
  # Wheat PDM: beneficiary list, many villages, reported by province, insecure areas
  pdm <- ask(reporting = "area", constraints = "insecure")
  expect_equal(pdm$design, "stratified_cluster")
  expect_true(any(grepl("PPS", pdm$stages)))
  expect_true(any(grepl("Insecure areas", pdm$warnings)))
  expect_equal(pdm$handover$calculator$method, "stratified_cluster")
  expect_equal(pdm$handover$generator$method, "stratified_cluster")
  expect_true(pdm$handover$generator$pps)

  # Site verification: few sites -> visit all, many -> random sample of sites
  expect_equal(ask(purpose = "verify", unit = "sites", spread = "few")$design, "site_all")
  expect_null(ask(purpose = "verify", unit = "sites", spread = "few")$handover$calculator)
  expect_equal(ask(purpose = "verify", unit = "sites", reporting = "component")$design, "site_sample")
  expect_equal(ask(purpose = "verify", unit = "sites", reporting = "component")$handover$generator$method, "stratified")

  # Baseline/endline with a comparison group, same households -> DiD panel in the Evaluation Calculator
  ev <- ask(purpose = "change", frame = "cluster_sizes", comparison = "groups_both", panel = "yes", random = "no")
  expect_equal(ev$evaluation, "did_panel")
  expect_equal(ev$handover$calculator$target, "evaluation")
  expect_true(ev$handover$calculator$clustered)
  expect_match(ev$stages[1], "comparison areas separately")
  expect_equal(ev$handover$generator$method, "stratified_cluster")
  expect_true(any(grepl("No random assignment", ev$warnings)))
  expect_equal(ask(purpose = "change", comparison = "before_after", panel = "no")$evaluation, "independent")
  expect_true(any(grepl("cannot show that the programme caused it", ask(purpose = "change", comparison = "before_after")$warnings)))

  # Phone: list -> list sample; no list -> RDD with the Afghanistan caveat; mixed mode
  expect_equal(ask(mode = "phone")$design, "phone_list")
  rdd <- ask(mode = "phone", frame = "none")
  expect_equal(rdd$design, "rdd")
  expect_true(any(grepl("Afghanistan", rdd$warnings)))
  expect_null(rdd$handover$generator)
  expect_equal(ask(mode = "mixed")$design, "mixed_mode")

  # Qualitative and other non-probability designs
  expect_equal(ask(purpose = "qualitative")$design, "purposive")
  expect_equal(ask(purpose = "qualitative", reporting = c("area", "gender"))$design, "purposive_groups")
  expect_equal(ask(purpose = "qualitative", frame = "unlistable")$design, "snowball")
  expect_equal(ask(frame = "unlistable")$design, "hidden_group")
  expect_equal(ask(purpose = "pilot")$design, "pilot")

  # Full list without constraints -> draw from the list; few sites -> all sites
  expect_equal(ask()$design, "srs_list")
  expect_equal(ask(reporting = "area")$design, "stratified_list")
  expect_equal(ask(spread = "few")$design, "site_census")
  expect_equal(ask(constraints = "budget")$design, "cluster")

  # No household list: villages first, with listing; no village list either
  nolist <- ask(frame = "none")
  expect_equal(nolist$design, "cluster")
  expect_true(any(grepl("village list first", nolist$stages)))
  expect_true(nolist$handover$generator$cluster_list)
  expect_false(nolist$handover$generator$pps)
})

test_that("decision tree: gender is a stratum only when it is on the unit list", {
  expect_true(ask(reporting = "gender")$answers$stratify)
  village <- ask(frame = "cluster_sizes", reporting = "gender")
  expect_false(village$answers$stratify)
  expect_equal(village$design, "cluster")
  expect_true(any(grepl("cannot be a stratum", village$warnings)))
})

test_that("decision tree: coverage warnings", {
  expect_true(any(grepl("Coverage: the list is incomplete", ask(constraints = "list_outdated")$warnings)))
  expect_true(any(grepl("Coverage: phone samples", ask(mode = "phone")$warnings)))
  expect_true(any(grepl("female enumerators", ask(constraints = "women")$warnings)))
})

test_that("decision tree: readable labels", {
  expect_equal(decision_label("spread", "few"), "A few sites, all reachable")
  expect_equal(decision_label("constraints", c("insecure", "budget")), "Insecure or inaccessible areas; Tight budget or time")
  expect_equal(decision_label("constraints", character(0)), "None")
})

test_that("decision tree server: follows the answers, hands over and opens articles", {
  sent <- NULL
  opened <- NULL
  testServer(decision_tree_server, args = list(on_handover = function(target, settings, name) sent <<- list(target, settings, name),
                                               on_article = function(id) opened <<- id), {
    session$setInputs(study_name = "", purpose = "level", unit = "people", frame = "unit_list", reporting = "overall",
                      spread = "many", mode = "in_person", constraints = NULL)
    expect_equal(recommendation()$design, "srs_list")
    session$setInputs(reporting = "area", constraints = "insecure")
    expect_equal(recommendation()$design, "stratified_cluster")

    session$setInputs(to_calculator = 1)
    expect_equal(sent[[1]], "sample")
    expect_equal(sent[[2]]$method, "stratified_cluster")
    expect_equal(sent[[3]], DESIGNS$stratified_cluster$name)
    session$setInputs(to_generator = 1)
    expect_equal(sent[[1]], "generator")

    session$setInputs(purpose = "change", comparison = "groups_both", panel = "no", random = "yes")
    session$setInputs(to_calculator = 2)
    expect_equal(sent[[1]], "evaluation")
    expect_equal(sent[[2]]$design, "did_cross")

    session$setInputs(open_article = "replacement_protocol")
    expect_equal(opened, "replacement_protocol")
  })
})

test_that("decision tree: PDF report with quotes in the study name", {
  skip_if_not(rmarkdown::pandoc_available())
  testServer(decision_tree_server, {
    session$setInputs(study_name = "Ghor \"PDM\" O'Neil's", purpose = "change", unit = "people", frame = "none",
                      reporting = c("area", "other"), other_group = "returnees", spread = "many", mode = "in_person",
                      constraints = c("insecure", "women"), comparison = "groups_both", panel = "yes", random = "no")
    path <- output$download_report
    expect_true(file.exists(path))
    expect_gt(file.size(path), 1000)
  })
  rmd <- decision_report_rmd(ask(reporting = c("area", "other"), other_group = "returnees"), "Test")
  expect_match(rmd, "Strata: province or district and returnees", fixed = TRUE)
  expect_match(rmd, "## Next Steps", fixed = TRUE)
})

test_that("decision tree: hand-over updates the receiving tabs", {
  suppressMessages({
    library(shinyjs)
    library(plotly)
    source(file.path(root, "sampling_generation_functions.R"))
    source(file.path(root, "modules", "sample_calculator_module.R"))
    source(file.path(root, "modules", "evaluation_calculator_module.R"))
    source(file.path(root, "modules", "sampling_generator_module.R"))
  })
  pdm <- ask(reporting = "area", constraints = "insecure")$handover
  testServer(sample_calculator_server, {
    session$setInputs(sampling_method = "simple_random", population_size = 10000, confidence_level = 0.95,
                      margin_error = 0.05, expected_proportion = 0.5, non_response_rate = 5,
                      data_input_method = "manual")
    session$returned$apply_handover(pdm$calculator, "Test design")
    session$flushReact()
    expect_match(paste(as.character(output$handover_note), collapse = ""), "Settings from the Decision Tree")
  })
  testServer(sampling_generator_server, {
    session$returned$apply_handover(pdm$generator, "Test design")
    session$flushReact()
    expect_match(paste(as.character(output$handover_note), collapse = ""), "province or district")
  })
  ev <- ask(purpose = "change", comparison = "groups_both", panel = "yes")$handover
  testServer(evaluation_calculator_server, {
    session$returned$apply_handover(ev$calculator, "Test design")
    session$flushReact()
    expect_match(paste(as.character(output$handover_note), collapse = ""), "Difference-in-differences, panel")
  })
})
