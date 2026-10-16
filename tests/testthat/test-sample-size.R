# Tests for sample_size_functions.R: evaluation (power) and monitoring (precision) formulas

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "sampling_generation_functions.R"))
source(file.path(root, "sample_size_functions.R"))

eval_args <- function(...) {
  modifyList(list(design = "independent", p1 = 0.15, effect = 0.10, direction = "increase", alpha = 0.05,
                  power = 0.80, sided = 2, pooled = FALSE, ratio = 1, rho = 0, response_rate = 1,
                  attrition = 0, clustered = FALSE, icc = 0, m = 1), list(...))
}

# ---- 3a.6 known-value regression ------------------------------------------

test_that("evaluation: plan regression case (D 10%, p1 0.15, RR 0.8, ICC 0.1, m 6)", {
  r <- evaluation_sample_size(eval_args(response_rate = 0.80, clustered = TRUE, icc = 0.10, m = 6))
  expect_equal(r$groups$After_Non_Response[1], 310)   # per group before clustering
  expect_equal(r$adj$deff, 1.5)
  expect_equal(r$groups$Clusters[1], 78)
  expect_equal(r$n_per_group, 468)
  expect_equal(r$total_sample, 936)
  expect_gte(r$power_achieved, 0.80)
})

test_that("evaluation: p2 entered directly gives the same result as p1 + D, and decrease mirrors", {
  a <- evaluation_sample_size(eval_args())
  b <- evaluation_sample_size(eval_args(p2 = 0.25))
  expect_equal(a$n_base, b$n_base)
  # p1 = 0.25 decreasing by 0.10 has the same variance as 0.15 increasing by 0.10
  d <- evaluation_sample_size(eval_args(p1 = 0.25, direction = "decrease"))
  expect_equal(d$n_base, a$n_base)
  expect_error(evaluation_sample_size(eval_args(p1 = 0.95)), "between 0 and 1")
})

# ---- Independent groups against R ------------------------------------------

test_that("evaluation: pooled variance matches stats::power.prop.test (3 cases)", {
  cases <- list(c(0.15, 0.25, 0.80, 0.05), c(0.50, 0.60, 0.90, 0.05), c(0.30, 0.22, 0.80, 0.01))
  for (cs in cases) {
    r <- evaluation_sample_size(eval_args(p1 = cs[1], p2 = cs[2], power = cs[3], alpha = cs[4], pooled = TRUE))
    ref <- power.prop.test(p1 = cs[1], p2 = cs[2], power = cs[3], sig.level = cs[4])$n
    expect_equal(r$n_base, ref, tolerance = 1e-4)
  }
})

test_that("evaluation: unpooled two-proportion test has the target power in simulation", {
  set.seed(11)
  r <- evaluation_sample_size(eval_args(p1 = 0.30, effect = 0.10))
  n <- r$n_per_group
  reps <- 4000
  x1 <- rbinom(reps, n, 0.30) / n
  x2 <- rbinom(reps, n, 0.40) / n
  zstat <- (x2 - x1) / sqrt(x1 * (1 - x1) / n + x2 * (1 - x2) / n)
  expect_equal(mean(abs(zstat) > qnorm(0.975)), 0.80, tolerance = 0.03)
})

test_that("evaluation: allocation ratio and one-sided test behave as expected", {
  r <- evaluation_sample_size(eval_args(ratio = 2))
  expect_equal(r$groups$Sample_Size[2], ceiling(2 * r$n_base - 1e-9))
  expect_lt(r$n_base, evaluation_sample_size(eval_args())$n_base)
  expect_lt(evaluation_sample_size(eval_args(sided = 1))$n_base, evaluation_sample_size(eval_args())$n_base)
})

# ---- Panel ------------------------------------------------------------------

test_that("evaluation: panel with rho = 0 equals independent groups; rho lowers n; attrition raises it", {
  ind <- evaluation_sample_size(eval_args())
  pan <- evaluation_sample_size(eval_args(design = "panel"))
  expect_equal(pan$n_base, ind$n_base)
  expect_lt(evaluation_sample_size(eval_args(design = "panel", rho = 0.5))$n_base, pan$n_base)
  att <- evaluation_sample_size(eval_args(design = "panel", attrition = 0.2))
  expect_equal(att$groups$After_Non_Response, ceiling(pan$n_base / 0.8 - 1e-9))
  expect_equal(att$total_interviews, 2 * att$n_per_group)
})

test_that("evaluation: panel matches a paired t-test power calculation (pwr, 3 cases)", {
  skip_if_not_installed("pwr")
  cases <- list(c(0.15, 0.25, 0.3), c(0.50, 0.40, 0.5), c(0.20, 0.35, 0.0))
  for (cs in cases) {
    a <- eval_args(design = "panel", p1 = cs[1], p2 = cs[2], rho = cs[3])
    r <- evaluation_sample_size(a)
    sd_diff <- sqrt(r$terms$v1)
    ref <- pwr::pwr.t.test(d = abs(cs[2] - cs[1]) / sd_diff, power = 0.80, sig.level = 0.05, type = "paired")$n
    # z-test vs t-test: the t-based n is 1-2 units larger
    expect_lt(abs(ref - r$n_base), 2.5)
  }
})

test_that("evaluation: panel test has the target power in simulation (correlated pairs)", {
  set.seed(5)
  p1 <- 0.40; p2 <- 0.52; rho <- 0.4
  r <- evaluation_sample_size(eval_args(design = "panel", p1 = p1, p2 = p2, rho = rho))
  n <- r$n_per_group
  p11 <- p1 * p2 + rho * sqrt(p1 * (1 - p1) * p2 * (1 - p2))
  probs <- c(p11, p1 - p11, p2 - p11, 1 - p1 - p2 + p11)   # (1,1), (1,0), (0,1), (0,0)
  reject <- replicate(3000, {
    cells <- sample(4, n, replace = TRUE, prob = probs)
    d <- (cells %in% c(1, 3)) - (cells %in% c(1, 2))        # endline - baseline
    abs(mean(d) / (sd(d) / sqrt(n))) > qnorm(0.975)
  })
  expect_equal(mean(reject), 0.80, tolerance = 0.03)
})

# ---- Difference-in-differences ----------------------------------------------

test_that("evaluation: DiD matches a two-sample t-test power calculation (3 cases)", {
  cases <- list(
    list(design = "did_cross", p1 = 0.30, p_control = 0.30, control_change = 0.05, effect = 0.10, rho = 0),
    list(design = "did_panel", p1 = 0.50, p_control = 0.45, control_change = 0.00, effect = 0.12, rho = 0.4),
    list(design = "did_cross", p1 = 0.20, p_control = 0.25, control_change = -0.03, effect = 0.08, rho = 0)
  )
  for (cs in cases) {
    r <- evaluation_sample_size(do.call(eval_args, cs))
    # n per arm for a two-sample comparison of changes with sd^2 = V / 2
    ref <- power.t.test(delta = cs$effect, sd = sqrt(r$terms$v1 / 2), power = 0.80, sig.level = 0.05)$n
    expect_lt(abs(ref - r$n_base), 2.5)
  }
})

test_that("evaluation: DiD cells, panel interviews and proportions", {
  r <- evaluation_sample_size(eval_args(design = "did_cross", p1 = 0.3, control_change = 0.05, ratio = 2,
                                        clustered = TRUE, icc = 0.05, m = 10, response_rate = 0.9))
  expect_equal(nrow(r$groups), 4)
  expect_equal(unname(r$p), c(0.3, 0.45, 0.3, 0.35))
  expect_equal(r$groups$Sample_Size %% 10, rep(0, 4))
  expect_equal(r$total_sample, sum(r$groups$Sample_Size))
  rp <- evaluation_sample_size(eval_args(design = "did_panel", rho = 0.3))
  expect_equal(nrow(rp$groups), 2)
  expect_equal(rp$total_interviews, 2 * rp$total_sample)
})

# ---- MDE ----------------------------------------------------------------------

test_that("evaluation: MDE (pooled) matches stats::power.prop.test solving for p2 (3 cases)", {
  cases <- list(c(0.15, 300), c(0.50, 800), c(0.30, 150))
  for (cs in cases) {
    m <- evaluation_mde(eval_args(p1 = cs[1], n_per_group = cs[2], pooled = TRUE))
    ref <- power.prop.test(n = cs[2], p1 = cs[1], power = 0.80, sig.level = 0.05)$p2
    expect_equal(m$p[["p2"]], ref, tolerance = 1e-4)
    expect_equal(m$power_achieved, 0.80, tolerance = 1e-6)
  }
})

test_that("evaluation: MDE inverts the sample size for every design", {
  for (d in c("independent", "panel", "did_cross", "did_panel")) {
    a <- eval_args(design = d, rho = 0.3, attrition = 0.1, response_rate = 0.85, clustered = TRUE, icc = 0.08, m = 8,
                   control_change = 0.03, p1 = 0.35)
    r <- evaluation_sample_size(a)
    m <- evaluation_mde(modifyList(a, list(clusters_per_group = r$groups$Clusters[1])))
    # The rounded-up sample detects at most the requested 10 points
    expect_lte(m$mde, 0.10 + 1e-9)
    expect_gt(m$mde, 0.09)
  }
  expect_error(evaluation_mde(eval_args(p1 = 0.5, n_per_group = 2)), "cannot detect")
})

test_that("evaluation: power curve rises with n and passes the target at the computed n", {
  r <- evaluation_sample_size(eval_args(clustered = TRUE, icc = 0.06, m = 10, response_rate = 0.9))
  curve <- evaluation_power_curve(r)
  expect_true(all(diff(curve$power) >= 0))
  expect_true(all(curve$x %% 10 == 0))
  expect_gte(evaluation_power(r$args, r$terms, r$n_per_group * r$adj$retention / r$adj$deff), 0.80)
  m <- evaluation_mde(eval_args(n_per_group = 400))
  expect_true(all(diff(evaluation_power_curve(m)$power) >= 0))
})

# ---- Monitoring: single mean (3b.1) -----------------------------------------

test_that("monitoring: single mean uses z^2 sigma^2 / e^2", {
  r <- monitoring_calculation(list(method = "simple_random", N = 1e7, confidence_level = 0.95, margin_error = 1,
                                   indicator = "mean", sd = 10, non_response_rate = 0))
  expect_equal(r$sample_size, 385)   # 1.96^2 * 100 = 384.1
  r2 <- monitoring_calculation(list(method = "cluster", N = 20000, confidence_level = 0.95, margin_error = 2,
                                    indicator = "mean", sd = 25, non_response_rate = 0.1, cluster_size = 12, icc = 0.06))
  expect_equal(r2$sample_size %% 12, 0)
  expect_lte(r2$moe_achieved, 2)
  expect_error(monitoring_calculation(list(method = "simple_random", N = 1000, confidence_level = 0.95, margin_error = 1,
                                           indicator = "mean", sd = NA, non_response_rate = 0)), "standard deviation")
})

test_that("monitoring: proportion results match the single-stage functions", {
  r <- monitoring_calculation(list(method = "simple_random", N = 10000, confidence_level = 0.95, margin_error = 0.05,
                                   p = 0.5, non_response_rate = 0.05))
  expect_equal(r$sample_size, calculate_simple_random_sample_size(10000, 0.5, 0.95, 0.05, 0.05))
  rc <- monitoring_calculation(list(method = "cluster", N = 10000, confidence_level = 0.95, margin_error = 0.05,
                                    p = 0.5, non_response_rate = 0.05, cluster_size = 12, icc = 0.06))
  expect_equal(rc$sample_size, calculate_cluster_sample_size(10000, 0.5, 0.95, 0.05, 12, 0.06, 0.05))
  expect_equal(rc$n_clusters * 12, rc$sample_size)
})

# ---- Monitoring: stratified allocation (3b.2) --------------------------------

strata3 <- data.frame(Stratum = c("A", "B", "C"), Population = c(6000, 3000, 1000), Variability = c(NA, NA, NA))

test_that("monitoring: precision per stratum adds up stratum sample sizes and meets e in each", {
  r <- monitoring_calculation(list(method = "stratified", confidence_level = 0.95, margin_error = 0.05, p = 0.5,
                                   non_response_rate = 0.05, strata = strata3, target = "per_stratum"))
  expect_equal(r$sample_size, sum(sapply(strata3$Population, calculate_simple_random_sample_size,
                                         p = 0.5, confidence_level = 0.95, margin_error = 0.05, non_response_rate = 0.05)))
  expect_true(all(r$breakdown$Expected_Margin_of_Error <= 0.05 + 1e-9))
})

test_that("monitoring: precision for the total meets e overall with every allocation", {
  for (alloc in c("proportional", "neyman", "equal")) {
    for (m in list(NULL, 10)) {
      spec <- list(method = if (is.null(m)) "stratified" else "stratified_cluster", confidence_level = 0.95,
                   margin_error = 0.05, p = 0.5, non_response_rate = 0.05, strata = strata3, target = "total",
                   allocation = alloc, cluster_size = m, icc = 0.06)
      r <- monitoring_calculation(spec)
      expect_lte(r$moe_achieved, 0.05 + 1e-9)
      expect_equal(sum(r$breakdown$Sample_Size), r$sample_size)
    }
  }
  # Proportional with a common p needs about the SRS sample size
  r <- monitoring_calculation(list(method = "stratified", confidence_level = 0.95, margin_error = 0.05, p = 0.5,
                                   non_response_rate = 0, strata = strata3, target = "total", allocation = "proportional"))
  expect_lte(abs(r$sample_size - calculate_simple_random_sample_size(10000, 0.5, 0.95, 0.05, 0)), 2)
  per <- monitoring_calculation(list(method = "stratified", confidence_level = 0.95, margin_error = 0.05, p = 0.5,
                                     non_response_rate = 0, strata = strata3, target = "per_stratum"))
  expect_lt(r$sample_size, per$sample_size)
})

test_that("monitoring: Neyman uses per-stratum variability and needs fewer interviews", {
  s <- transform(strata3, Variability = c(0.5, 0.1, 0.05))
  base <- list(method = "stratified", confidence_level = 0.95, margin_error = 0.03, p = 0.5,
               non_response_rate = 0, strata = s, target = "total")
  ney <- monitoring_calculation(modifyList(base, list(allocation = "neyman")))
  prop <- monitoring_calculation(modifyList(base, list(allocation = "proportional")))
  expect_lt(ney$sample_size, prop$sample_size)
  expect_gt(ney$breakdown$Allocation_Share_pct[1], prop$breakdown$Allocation_Share_pct[1])
  # Minimum per stratum is respected
  mn <- monitoring_calculation(modifyList(base, list(allocation = "neyman", min_per_stratum = 60)))
  expect_true(all(mn$breakdown$Completed_Interviews_Needed >= 60))
})

test_that("helpers: ascii_text and markdown_table", {
  expect_equal(ascii_text("n₀ × DEFF, σ²"), "n0 x DEFF, sigma^2")
  tab <- markdown_table(data.frame(a = c("x|y", "z"), b = 1:2))
  expect_match(tab, "| a | b |", fixed = TRUE)
  expect_match(tab, "x/y", fixed = TRUE)
})
