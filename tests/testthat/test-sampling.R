# Tests for sampling_generation_functions.R and the calculator formulas
# Run from the project root: Rscript -e 'testthat::test_dir("tests/testthat")'

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "sampling_generation_functions.R"))
source(file.path(root, "sampling_generation_functions.R"))
  source(file.path(root, "sample_size_functions.R"))
  source(file.path(root, "modules", "sample_calculator_module.R"))

make_frame <- function(N = 1003, n_clusters = 40, seed = 1) {
  set.seed(seed)
  d <- data.frame(
    id = seq_len(N),
    village = sample(paste0("V", seq_len(n_clusters)), N, replace = TRUE),
    notes = ifelse(runif(N) < 0.2, NA, "x"),
    stringsAsFactors = FALSE
  )
  d$village_pop <- ave(rep(1, N), d$village, FUN = function(v) length(v) * 10)
  d
}

test_that("SRS returns n main + b reserves, no duplicates, rows with NA kept", {
  d <- make_frame()
  s <- simple_random_sampling(d, 100, 7, 10)
  expect_equal(sum(s$sample_type == "Sample"), 100)
  expect_equal(sum(s$sample_type == "Buffer"), 10)
  expect_equal(anyDuplicated(s$id), 0)
  expect_equal(sort(s$reserve_order[s$sample_type == "Buffer"]), 1:10)
  expect_true(any(is.na(s$notes)))
})

test_that("same seed gives the same sample", {
  d <- make_frame()
  expect_identical(simple_random_sampling(d, 50, 99, 5)$id, simple_random_sampling(d, 50, 99, 5)$id)
  expect_identical(systematic_sampling(d, 50, 99, 5)$id, systematic_sampling(d, 50, 99, 5)$id)
  expect_identical(
    two_stage_cluster_sampling(d, "village", 100, 10, 99, TRUE, NULL, 20)$id,
    two_stage_cluster_sampling(d, "village", 100, 10, 99, TRUE, NULL, 20)$id
  )
})

test_that("systematic main sample covers the whole frame with probability n/N", {
  d <- make_frame()
  n <- 100
  counts <- integer(nrow(d))
  reps <- 10000
  for (seed in seq_len(reps)) {
    s <- systematic_sampling(d, n, seed)
    expect_equal(nrow(s), n)
    counts[s$id] <- counts[s$id] + 1
  }
  expect_true(all(counts > 0))
  # Binomial sd at p = 0.1 over 10,000 draws is 0.003; allow about 4 sd across 1,003 units
  expect_lt(max(abs(counts / reps - n / nrow(d))), 0.012)
})

test_that("systematic buffer is not taken from the end of the list", {
  d <- make_frame()
  s <- systematic_sampling(d, 100, 3, 10)
  main <- s$id[s$sample_type == "Sample"]
  expect_gt(max(main), nrow(d) - nrow(d) / 100)
  expect_equal(length(intersect(main, s$id[s$sample_type == "Buffer"])), 0)
})

test_that("systematic sort_by orders the frame before selection", {
  d <- make_frame()
  n <- 40
  k <- nrow(d) / n
  s <- systematic_sampling(d, n, 5, sort_by = "village")
  # After sorting, every village with at least k units forms a block of length >= k and must be hit
  big <- names(which(table(d$village) >= ceiling(k)))
  expect_true(all(big %in% s$village))
})

test_that("PPS cluster sampling: m units per hit, reserves within cluster, no duplicates", {
  d <- make_frame()
  s <- two_stage_cluster_sampling(d, "village", 100, 10, 5, use_pps = TRUE, buffer_pct = 20)
  expect_equal(anyDuplicated(s$id), 0)
  main <- s[s$sample_type == "Sample", ]
  expect_equal(nrow(main), 100)
  per_cluster <- tapply(main$selection_hits, main$village, function(v) length(v) / v[1])
  expect_true(all(per_cluster == 10))
  reserves <- s[s$sample_type == "Buffer", ]
  expect_true(all(reserves$village %in% main$village))
})

test_that("PPS selection frequency is proportional to cluster size", {
  d <- make_frame(N = 2000, n_clusters = 10)
  sizes <- table(d$village)
  hits <- setNames(numeric(length(sizes)), names(sizes))
  for (seed in 1:500) {
    s <- two_stage_cluster_sampling(d, "village", 20, 2, seed, use_pps = TRUE)
    h <- tapply(s$selection_hits, s$village, `[`, 1)
    hits[names(h)] <- hits[names(h)] + h
  }
  expected <- as.numeric(sizes / sum(sizes)) * 10 * 500
  expect_lt(max(abs(hits - expected) / expected), 0.15)
})

test_that("short clusters raise a warning instead of silently returning fewer units", {
  d <- make_frame()
  expect_warning(
    two_stage_cluster_sampling(d, "village", 300, 30, 5, use_pps = TRUE),
    "fewer units than required"
  )
})

test_that("cluster population repeated on each row is used once, not summed", {
  d <- make_frame()
  mos <- cluster_size_measure(d, "village", "village_pop")
  expect_match(mos$rule, "constant")
  expect_equal(mos$table$population, mos$table$frame_units * 10)

  d$hh <- 1
  mos2 <- cluster_size_measure(d, "village", "hh")
  expect_equal(mos2$table$population, mos2$table$frame_units)
})

test_that("equal-probability cluster sampling selects distinct clusters", {
  d <- make_frame()
  s <- two_stage_cluster_sampling(d, "village", 100, 10, 5, use_pps = FALSE)
  expect_true(all(s$selection_hits == 1))
  expect_equal(length(unique(s$village)), 10)
  expect_error(two_stage_cluster_sampling(d, "village", 1000, 10, 5, use_pps = FALSE), "Not enough clusters")
})

test_that("rows without a cluster ID are excluded with a warning", {
  d <- make_frame()
  d$village[1:5] <- NA
  expect_warning(two_stage_cluster_sampling(d, "village", 50, 10, 5), "no value")
})

test_that("cluster sample size is a whole number of clusters of m interviews", {
  n <- calculate_cluster_sample_size(10000, 0.5, 0.95, 0.05, cluster_size = 12, icc = 0.06, non_response_rate = 0.05)
  expect_equal(n %% 12, 0)
  n_unrounded <- (qnorm(0.975)^2 * 0.25 / 0.05^2) * (1 + 11 * 0.06)
  n_unrounded <- n_unrounded / (1 + (n_unrounded - 1) / 10000) / 0.95
  expect_equal(n, ceiling(n_unrounded / 12) * 12)
})

test_that("margin of error is the inverse of the sample size formula", {
  n <- calculate_simple_random_sample_size(5000, 0.5, 0.95, 0.05, 0.1)
  expect_lte(calculate_margin_of_error(n, 5000, 0.5, 0.95, 0.1), 0.05)
  expect_gt(calculate_margin_of_error(n - 2, 5000, 0.5, 0.95, 0.1), 0.05)

  deff <- 1 + 9 * 0.06
  nc <- calculate_cluster_sample_size(5000, 0.5, 0.95, 0.05, 10, 0.06, 0.1)
  expect_lte(calculate_margin_of_error(nc, 5000, 0.5, 0.95, 0.1, deff), 0.05)
  # Ignoring non-response and DEFF overstates precision
  expect_lt(calculate_margin_of_error(nc, 5000, 0.5, 0.95, 0, 1), calculate_margin_of_error(nc, 5000, 0.5, 0.95, 0.1, deff))
})

test_that("same seed gives the same sample for every design, different seeds differ", {
  d <- make_frame(N = 1200, n_clusters = 30)
  d$province <- rep(c("A", "B", "C"), length.out = nrow(d))
  specs <- list(
    list(method = "simple_random"),
    list(method = "systematic", sort_var = "village"),
    list(method = "cluster", cluster_var = "village", n_units = 5, use_pps = TRUE),
    list(method = "stratified", strata_var = "province", allocation = "proportional"),
    list(method = "stratified_cluster", strata_var = "province", allocation = "equal",
         cluster_var = "village", n_units = 5, use_pps = TRUE)
  )
  for (sp in specs) {
    spec <- c(sp, list(sample_size = 60, buffer_pct = 10))
    draw <- function(seed) suppressWarnings(generate_sample_design(d, c(spec, seed = seed))$sample)
    a <- draw(42)
    expect_identical(a, draw(42))
    expect_false(identical(a$id, draw(43)$id))
    expect_false(any(duplicated(a$id)))
  }
})
make_cluster_list <- function(C = 60, seed = 1) {
  set.seed(seed)
  data.frame(village = sprintf("V%02d", seq_len(C)), hh = sample(2:30, C, replace = TRUE), stringsAsFactors = FALSE)
}

test_that("cluster list: each row is a cluster, listed once with its interviews", {
  d <- make_cluster_list()
  s <- suppressWarnings(cluster_list_sampling(d, "village", 100, 8, 3, use_pps = TRUE, pop_var = "hh", buffer_pct = 20))
  main <- s$sample_type == "Sample"
  expect_false(any(duplicated(s$village)))
  expect_equal(sum(s$selection_hits[main]), 13)
  expect_equal(sum(!main), 3)
  expect_equal(s$reserve_order[!main], 1:3)
  expect_equal(s$interviews, as.integer(pmin(8 * s$selection_hits, s$hh)))
  # PPS weights per interview add up to the list total exactly
  expect_equal(sum(s$base_weight[main] * s$interviews[main]), sum(d$hh))
  expect_equal(s$p_stage1, 13 * s$hh / sum(d$hh))
})

test_that("cluster list: small clusters are capped and reported", {
  d <- make_cluster_list()
  d$hh[] <- 3
  d$hh[1] <- 50
  expect_warning(cluster_list_sampling(d, "village", 50, 5, 1, use_pps = TRUE, pop_var = "hh"), "smaller than the interviews needed")
})

test_that("cluster list: equal probability, with and without sizes", {
  d <- make_cluster_list()
  s <- suppressMessages(suppressWarnings(cluster_list_sampling(d, "village", 80, 8, 2, use_pps = FALSE, pop_var = "hh")))
  expect_true(all(s$selection_hits == 1))
  expect_equal(s$p_stage1, rep(10 / 60, nrow(s)))
  expect_error(cluster_list_sampling(d, "village", 800, 8, 2, use_pps = FALSE), "Not enough clusters")
  expect_error(cluster_list_sampling(d, "village", 80, 8, 2, use_pps = TRUE), "needs a cluster population variable")
  s2 <- suppressMessages(cluster_list_sampling(d, "village", 80, 8, 2, use_pps = FALSE))
  expect_true(all(is.na(s2$base_weight)))
  expect_equal(s2$interviews, rep(8L, 10))
})

test_that("cluster list: repeated names stay separate clusters, blank and zero-size rows are excluded", {
  d <- make_cluster_list()
  d$village[c(3, 40)] <- "Twin"
  ids <- cluster_list_ids(d, "village")
  expect_equal(ids[c(3, 40)], c("Twin (row 3)", "Twin (row 40)"))
  d$village[7] <- ""
  d$hh[8] <- 0
  s <- suppressMessages(suppressWarnings(cluster_list_sampling(d, "village", 500, 10, 4, use_pps = TRUE, pop_var = "hh")))
  expect_false(any(s$village %in% c("", d$village[8])))
  chk <- check_frame(d, list(method = "cluster", cluster_method = "cluster_list", cluster_var = "village",
                             pop_var = "hh", n_units = 10, use_pps = TRUE, sample_size = 100))
  expect_true(any(grepl("Twin [(]row 3[)]", chk$issues$detail)))
  expect_equal(chk$exclusions$rows, c(1L, 1L))
})

test_that("cluster list: a unit list in cluster-list mode is flagged", {
  d <- make_frame()
  chk <- check_frame(d, list(method = "cluster", cluster_method = "cluster_list", cluster_var = "village",
                             n_units = 5, use_pps = FALSE, sample_size = 50))
  expect_true(any(grepl("use Method 1", chk$issues$detail)))
})

test_that("cluster list: stratified allocation uses each stratum's total size", {
  d <- make_cluster_list(90)
  d$province <- rep(c("A", "B", "C"), c(50, 30, 10))
  spec <- list(method = "stratified_cluster", cluster_method = "cluster_list", strata_var = "province", allocation = "proportional",
               cluster_var = "village", pop_var = "hh", n_units = 5, use_pps = TRUE, sample_size = 150, buffer_pct = 10, seed = 8)
  alloc <- compute_allocation(d, spec)
  expect_equal(alloc$frame_population, as.numeric(tapply(d$hh, d$province, sum)))
  expect_equal(alloc$n_allocated, allocate_sample(alloc$frame_population, 150))
  expect_equal(alloc$clusters_to_select, as.integer(ceiling(alloc$n_allocated / 5)))
  res <- suppressMessages(suppressWarnings(generate_sample_design(d, spec)))
  s <- res$sample
  expect_equal(names(s)[seq_len(length(DESIGN_COLUMNS) + 1)], c(DESIGN_COLUMNS, "interviews"))
  main <- s$sample_type == "Sample"
  expect_equal(sum(s$base_weight[main] * s$interviews[main]), sum(d$hh))
  cs <- summarise_clusters(d, s, spec)
  expect_equal(nrow(cs), 90)
  expect_equal(sum(cs$times_selected), sum(alloc$clusters_to_select))
  # Without sizes, strata are weighted by their number of clusters
  spec$pop_var <- NULL
  spec$use_pps <- FALSE
  expect_equal(compute_allocation(d, spec)$n_allocated, c(83L, 50L, 17L))
})

test_that("a cluster list in unit-list mode points to Method 2", {
  d <- make_cluster_list()
  chk <- check_frame(d, list(method = "cluster", cluster_method = "population_units", cluster_var = "village",
                             n_units = 5, use_pps = TRUE, sample_size = 50))
  expect_true(any(grepl("use Method 2", chk$issues$detail)))
  expect_false(any(grepl("use Method 2", check_frame(make_frame(), list(method = "cluster", cluster_var = "village",
                                                                         n_units = 5, use_pps = TRUE, sample_size = 50))$issues$detail)))
})
