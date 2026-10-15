# Tests for allocation, stratified designs, design weights and frame checks (Phase 2)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "sampling_generation_functions.R"))

make_strat_frame <- function(seed = 1) {
  set.seed(seed)
  sizes <- c(North = 600, South = 300, East = 100)
  d <- data.frame(
    stratum_var = rep(names(sizes), sizes),
    stringsAsFactors = FALSE
  )
  d$id <- seq_len(nrow(d))
  d$village <- paste0(d$stratum_var, "_V", sample(1:5, nrow(d), replace = TRUE))
  d
}

base_spec <- function(...) {
  modifyList(list(sample_size = 100, buffer_pct = 10, seed = 11, strata_var = "stratum_var",
                  allocation = "proportional", min_per_stratum = 0, within = "simple_random",
                  n_units = 5, use_pps = TRUE), list(...))
}

test_that("allocation methods add up and respect min and frame size", {
  pop <- c(600, 300, 100)
  expect_equal(allocate_sample(pop, 100, "proportional"), c(60L, 30L, 10L))
  expect_equal(sum(allocate_sample(pop, 100, "equal")), 100)
  expect_equal(allocate_sample(pop, 90, "equal"), c(30L, 30L, 30L))
  expect_equal(allocate_sample(pop, 100, "proportional", min_n = 20), c(53L, 27L, 20L))
  # Neyman: stratum with larger SD gets more
  ney <- allocate_sample(pop, 100, "neyman", values = c(1, 1, 5))
  expect_equal(sum(ney), 100)
  expect_gt(ney[3], 10)
  # Never above frame size
  capped <- allocate_sample(c(10, 1000), 200, "equal")
  expect_equal(capped, c(10L, 190L))
  expect_equal(allocate_sample(pop, 0, "manual", values = c(5, 6, 7)), c(5L, 6L, 7L))
  expect_error(allocate_sample(pop, 30, "proportional", min_n = 20), "minimum")
})

test_that("stratified SRS: allocation honoured per stratum, weights sum to N per stratum", {
  d <- make_strat_frame()
  res <- generate_sample_design(d, base_spec(method = "stratified"))
  s <- res$sample
  main <- s[s$sample_type == "Sample", ]
  expect_equal(as.vector(table(main$stratum)[c("North", "South", "East")]), c(60L, 30L, 10L))
  w <- tapply(main$base_weight, main$stratum, sum)
  expect_equal(as.vector(w[c("North", "South", "East")]), c(600, 300, 100))
  expect_true(all(s$stratum[s$sample_type == "Buffer"] %in% c("North", "South", "East")))
  expect_equal(anyDuplicated(s$id), 0)
  expect_equal(names(s)[seq_along(DESIGN_COLUMNS)], DESIGN_COLUMNS)
})

test_that("stratified systematic works and records stratum seeds", {
  d <- make_strat_frame()
  res <- generate_sample_design(d, base_spec(method = "stratified", within = "systematic", sort_var = "village"))
  expect_equal(sum(res$sample$sample_type == "Sample"), 100)
  expect_equal(nrow(res$allocation), 3)
  expect_equal(length(unique(res$allocation$seed)), 3)
})

test_that("stratified cluster: clusters selected within strata, weights about N", {
  d <- make_strat_frame()
  res <- generate_sample_design(d, base_spec(method = "stratified_cluster", cluster_var = "village"))
  s <- res$sample
  main <- s[s$sample_type == "Sample", ]
  expect_true(all(startsWith(main$cluster_id, main$stratum)))
  expect_equal(res$allocation$clusters_to_select, c(2L, 12L, 6L))  # East, North, South
  # PPS with size = frame count is self-weighting within stratum when no cluster falls short
  expect_equal(as.vector(tapply(main$base_weight, main$stratum, sum)[res$allocation$stratum]),
               res$allocation$frame_units, tolerance = 0.01)
})

test_that("PPS cluster weights match Hansen-Hurwitz and are constant when MOS = frame count", {
  set.seed(3)
  d <- data.frame(id = 1:1000, village = sample(paste0("V", 1:40), 1000, replace = TRUE))
  s <- two_stage_cluster_sampling(d, "village", 100, 10, 5, use_pps = TRUE)
  main <- s[s$sample_type == "Sample", ]
  expect_equal(length(unique(round(main$base_weight, 8))), 1)
  expect_equal(sum(main$base_weight), 1000)
})

test_that("equal-probability cluster weights average to N over repeated draws", {
  set.seed(4)
  d <- data.frame(id = 1:1000, village = sample(paste0("V", 1:40), 1000, replace = TRUE))
  totals <- vapply(1:300, function(seed) {
    s <- two_stage_cluster_sampling(d, "village", 100, 10, seed, use_pps = FALSE)
    sum(s$base_weight[s$sample_type == "Sample"])
  }, numeric(1))
  expect_lt(abs(mean(totals) - 1000) / 1000, 0.02)
})

test_that("SRS and systematic weights are N/n and sum to N", {
  d <- make_strat_frame()
  for (method in c("simple_random", "systematic")) {
    s <- generate_sample_design(d, base_spec(method = method))$sample
    main <- s[s$sample_type == "Sample", ]
    expect_equal(sum(main$base_weight), nrow(d))
    expect_true(all(is.na(s$stratum)))
  }
})

test_that("neyman allocation from expected proportions", {
  d <- make_strat_frame()
  spec <- base_spec(method = "stratified", allocation = "neyman", neyman_input = "proportion",
                    allocation_values = c(North = 0.5, South = 0.5, East = 0.05))
  alloc <- compute_allocation(d, spec)
  expect_equal(sum(alloc$n_allocated), 100)
  expect_true("allocation_input" %in% names(alloc))
})

test_that("frame checks flag duplicates, missing strata and impossible allocations", {
  d <- make_strat_frame()
  d$id[2] <- d$id[1]
  d$stratum_var[5] <- NA
  chk <- check_frame(d, base_spec(method = "stratified", id_var = "id"))
  expect_true(any(chk$issues$check == "Unit ID" & chk$issues$level == "warning"))
  expect_equal(chk$exclusions$rows[grepl("strata", chk$exclusions$reason)], 1L)
  expect_true(chk$can_proceed)

  bad <- check_frame(d, base_spec(method = "stratified", allocation = "manual",
                                  allocation_values = c(North = 10, South = 10, East = 500)))
  expect_false(bad$can_proceed)
})

test_that("frame column clashing with a design column is renamed, not overwritten", {
  d <- make_strat_frame()
  d$base_weight <- 99
  res <- suppressMessages(generate_sample_design(d, base_spec(method = "simple_random")))
  expect_true("base_weight_frame" %in% names(res$sample))
  expect_true(all(res$sample$base_weight_frame == 99))
})
