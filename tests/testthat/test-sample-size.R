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

# ---- Phase 6: means, ANCOVA, indicators, subgroups, DEFF and field adjustments ----

mean_args <- function(...) modifyList(eval_args(outcome = "mean", p1 = 50, sd = 10, effect = 3), list(...))

test_that("evaluation means: closed form and pwr::pwr.t.test (3 cases)", {
  skip_if_not_installed("pwr")
  cases <- list(c(10, 3, 0.80), c(4, 1, 0.90), c(25, 10, 0.80))
  for (cs in cases) {
    r <- evaluation_sample_size(mean_args(sd = cs[1], effect = cs[2], power = cs[3]))
    expect_equal(r$n_base, (qnorm(0.975) + qnorm(cs[3]))^2 * 2 * cs[1]^2 / cs[2]^2)
    ref <- pwr::pwr.t.test(d = cs[2] / cs[1], power = cs[3], sig.level = 0.05)$n
    expect_lt(abs(ref - r$n_base), 2.5)   # t-test needs 1-2 more
  }
  # Unequal groups against pwr.t2n.test: n2 = 2 n1
  r <- evaluation_sample_size(mean_args(ratio = 2))
  pw <- pwr::pwr.t2n.test(n1 = ceiling(r$n_base), n2 = 2 * ceiling(r$n_base), d = 0.3, sig.level = 0.05)$power
  expect_equal(pw, 0.80, tolerance = 0.01)
})

test_that("evaluation means: d, relative and absolute entries agree; MDE in units and d", {
  base <- evaluation_sample_size(mean_args())
  by_d <- evaluation_sample_size(mean_args(effect_scale = "d", effect_d = 0.3))
  by_rel <- evaluation_sample_size(mean_args(effect_scale = "relative", effect_relative = 0.06))
  expect_equal(by_d$n_base, base$n_base)
  expect_equal(by_rel$n_base, base$n_base)
  expect_error(evaluation_sample_size(eval_args(effect_scale = "d", effect_d = 0.3)), "means only")
  expect_error(evaluation_sample_size(mean_args(sd = NA)), "standard deviation")

  m <- evaluation_mde(mean_args(n_per_group = base$n_per_group))
  expect_lte(m$mde, 3 + 1e-9)
  expect_gt(m$mde, 2.95)
  expect_true(any(grepl("Cohen's d", m$steps$Parameter)))
  # MDE for a mean is closed form: (z_a + z_b) sqrt(V / n)
  expect_equal(m$mde, (qnorm(0.975) + qnorm(0.8)) * sqrt(200 / base$n_per_group))
})

test_that("evaluation means: panel, DiD and ANCOVA variance factors", {
  rho <- 0.4
  ind <- evaluation_sample_size(mean_args())
  pan <- evaluation_sample_size(mean_args(design = "panel", rho = rho))
  expect_equal(pan$terms$v1, 2 * 100 * (1 - rho))
  did <- evaluation_sample_size(mean_args(design = "did_panel", rho = rho))
  anc <- evaluation_sample_size(mean_args(design = "ancova", rho = rho))
  dcs <- evaluation_sample_size(mean_args(design = "did_cross"))
  expect_equal(did$terms$v1, 2 * 100 * (1 - rho) * 2)
  expect_equal(anc$terms$v1, 100 * (1 - rho^2) * 2)
  expect_equal(dcs$terms$v1, 2 * 100 * 2)
  expect_equal(anc$n_base / did$n_base, (1 - rho^2) / (2 * (1 - rho)))
  expect_equal(nrow(anc$groups), 2)
  expect_equal(anc$total_interviews, 2 * anc$total_sample)
  # Independent groups equal ANCOVA with rho = 0 (endline only)
  expect_equal(evaluation_sample_size(mean_args(design = "ancova"))$n_base, ind$n_base)
})

test_that("evaluation: panel DiD with rho < 0.5 shows the ANCOVA and endline-only alternatives", {
  r <- evaluation_sample_size(eval_args(design = "did_panel", p1 = 0.3, rho = 0.3))
  expect_equal(nrow(r$alternatives), 3)
  expect_equal(r$alternatives$Variance_Factor, c(1.4, 0.91, 1))
  expect_equal(r$alternatives$Sample_per_Arm[1], r$n_per_group)
  expect_lt(r$alternatives$Sample_per_Arm[2], r$alternatives$Sample_per_Arm[3])
  expect_lt(r$alternatives$Sample_per_Arm[3], r$alternatives$Sample_per_Arm[1])
  expect_true(any(grepl("ANCOVA", r$warnings)))
  expect_null(evaluation_sample_size(eval_args(design = "did_panel", rho = 0.6))$alternatives)
  m <- evaluation_mde(eval_args(design = "did_panel", rho = 0.2, n_per_group = 400))
  expect_true("MDE" %in% names(m$alternatives))
})

test_that("evaluation: several indicators use the largest sample; Bonferroni; MDE per indicator", {
  tab <- data.frame(Indicator = c("FCS", "Stunting", "Income"), Type = c("mean", "proportion", "mean"),
                    Baseline = c(40, 0.30, 5000), SD = c(12, NA, 3000), Change = c(3, -0.08, 600),
                    ICC = c(NA, 0.15, NA), stringsAsFactors = FALSE)
  a <- eval_args(clustered = TRUE, m = 10, icc = 0.05, response_rate = 0.9, indicators = tab)
  r <- evaluation_sample_size(a)
  single <- vapply(seq_len(nrow(tab)), function(i) {
    evaluation_sample_size(evaluation_indicator_args(a, tab[i, ], i))$n_per_group
  }, numeric(1))
  expect_equal(r$n_per_group, max(single))
  expect_equal(r$indicators$Sample_per_Group, single)
  expect_equal(r$indicator_name, tab$Indicator[which.max(single)])
  expect_equal(sum(r$indicators$Sets_the_Sample == "Yes"), 1)
  # The ICC of a row replaces the common ICC
  expect_equal(r$indicators$DEFF[2], 1 + 9 * 0.15)
  # Stunting is a decrease
  expect_lt(r$indicators$Change[2], 0)

  bon <- evaluation_sample_size(modifyList(a, list(bonferroni = TRUE)))
  expect_equal(bon$args$alpha, 0.05 / 3)
  expect_gt(bon$n_per_group, r$n_per_group)
  expect_true(any(grepl("Bonferroni", bon$steps$Parameter)))

  m <- evaluation_mde(modifyList(a, list(clusters_per_group = 40)))
  expect_equal(nrow(m$indicators), 3)
  one <- evaluation_mde(modifyList(evaluation_indicator_args(a, tab[2, ], 2), list(clusters_per_group = 40)))
  expect_equal(m$indicators$MDE[2], signif(one$mde, 4))
  no_change <- tab
  no_change$Change[2] <- NA
  expect_error(evaluation_sample_size(modifyList(a, list(indicators = no_change))), "Stunting: Enter the change")
})

test_that("evaluation: subgroups - within goal multiplies by 1 / smallest share, overall goal reports MDEs", {
  sg <- data.frame(Subgroup = c("Female-headed", "Male-headed"), Share = c(0.25, 0.75), stringsAsFactors = FALSE)
  base <- evaluation_sample_size(eval_args(response_rate = 0.9))
  w <- evaluation_sample_size(eval_args(response_rate = 0.9, subgroups = sg, subgroup_goal = "within"))
  expect_equal(w$factor, 4)
  expect_equal(w$groups$Completed_Needed[1], ceiling(base$n_base / 0.25 - 1e-9))
  expect_equal(w$n_per_group, ceiling(base$n_base / 0.25 / 0.9 - 1e-9))
  expect_true(all(w$subgroups$Detects_Target == "Yes"))
  expect_true(any(grepl("oversampling", w$warnings)))

  o <- evaluation_sample_size(eval_args(response_rate = 0.9, subgroups = sg, subgroup_goal = "overall"))
  expect_equal(o$n_per_group, base$n_per_group)
  expect_equal(o$subgroups$Sample_in_Subgroup, round(c(0.25, 0.75) * base$n_per_group))
  expect_true(all(o$subgroups$MDE > 0.10))
  expect_equal(o$subgroups$Detects_Target, c("No", "No"))

  # A fixed (oversampled) subgroup does not drive the total
  fx <- sg
  fx$Fixed <- c(500, NA)
  f <- evaluation_sample_size(eval_args(response_rate = 0.9, subgroups = fx, subgroup_goal = "within"))
  expect_equal(f$factor, 1 / 0.75)
  expect_equal(f$subgroups$Sample_in_Subgroup[1], 500)
  bad <- sg
  bad$Share[1] <- 0
  expect_error(evaluation_sample_size(eval_args(subgroups = bad, subgroup_goal = "within")), "Female-headed")
})

test_that("evaluation: the three DEFF entry methods agree when CV = 0; Kish weighting effect", {
  a <- eval_args(clustered = TRUE, m = 8, icc = 0.07, response_rate = 0.9)
  r_icc <- evaluation_sample_size(a)
  r_cv <- evaluation_sample_size(modifyList(a, list(deff_method = "cv", cv_cluster = 0)))
  r_known <- evaluation_sample_size(modifyList(a, list(deff_method = "known", deff_known = 1 + 7 * 0.07)))
  expect_equal(r_cv$adj$deff, r_icc$adj$deff)
  expect_equal(r_known$adj$deff, r_icc$adj$deff)
  expect_equal(r_cv$n_per_group, r_icc$n_per_group)
  expect_equal(r_known$n_per_group, r_icc$n_per_group)
  expect_equal(evaluation_adjustments(modifyList(a, list(deff_method = "cv", cv_cluster = 0.5)))$deff,
               1 + ((0.25 + 1) * 8 - 1) * 0.07)

  # Kish: 1 + CV^2 of the Sample Generator's base weights = n sum(w^2) / (sum w)^2
  set.seed(3)
  frame <- data.frame(id = 1:1000, region = rep(c("A", "B", "C"), c(700, 200, 100)), stringsAsFactors = FALSE)
  s <- stratified_sampling(frame, "region", data.frame(stratum = c("A", "B", "C"), n_allocated = c(40, 40, 40), seed = 1:3, buffer = 0))
  w <- s$base_weight[s$sample_type == "Sample"]
  expect_gt(length(unique(round(w, 6))), 1)
  cv_w <- sqrt(mean((w - mean(w))^2)) / mean(w)
  adj <- evaluation_adjustments(modifyList(a, list(weighting = TRUE, cv_weights = cv_w)))
  expect_equal(adj$deff_weight, length(w) * sum(w^2) / sum(w)^2)
  expect_equal(adj$deff, r_icc$adj$deff * adj$deff_weight)
  r_w <- evaluation_sample_size(modifyList(a, list(weighting = TRUE, cv_weights = cv_w)))
  expect_gt(r_w$n_per_group, r_icc$n_per_group)
  expect_true(all(c("Weighting Effect (Kish)", "Design Effect, Clustering") %in% r_w$steps$Parameter))
})

test_that("evaluation field adjustments: FPC, inaccessible clusters, matching factor (worked examples)", {
  # FPC: n0 = (1.96 + 0.8416)^2 * 2 / 0.2^2 = 392.4; with N = 500: 392.4 / (1 + 391.4 / 500) = 220.1
  r <- evaluation_sample_size(mean_args(sd = 1, effect = 0.2, population = 500))
  n0 <- (qnorm(0.975) + qnorm(0.8))^2 * 2 / 0.04
  expect_equal(r$n_per_group, ceiling(n0 / (1 + (n0 - 1) / 500)))
  expect_equal(r$n_per_group, 221)
  expect_false(any(grepl("census", r$warnings)))
  big <- evaluation_sample_size(mean_args(sd = 1, effect = 0.2, population = 300))
  expect_true(any(grepl("census", big$warnings)))

  # Inaccessible clusters: 78 clusters needed, 15% inaccessible -> ceiling(78 / 0.85) = 92
  a <- eval_args(response_rate = 0.80, clustered = TRUE, icc = 0.10, m = 6)
  inacc <- evaluation_sample_size(modifyList(a, list(inaccessible = 0.15)))
  expect_equal(inacc$groups$Clusters_Needed[1], 78)
  expect_equal(inacc$groups$Clusters[1], 92)
  expect_equal(inacc$n_per_group, 92 * 6)

  # Matching (PSM) factor multiplies the completed interviews
  psm <- evaluation_sample_size(modifyList(a, list(matching_factor = 1.3)))
  base <- evaluation_sample_size(a)
  expect_equal(psm$groups$Completed_Needed[1], ceiling(base$n_base * 1.3 - 1e-9))
})

test_that("evaluation: fewer than 20 clusters per group switches to t-values", {
  a <- eval_args(p1 = 0.3, effect = 0.25, response_rate = 0.9, clustered = TRUE, icc = 0.05, m = 15)
  z_only <- evaluation_sample_size(modifyList(a, list(small_cluster_t = FALSE)))
  expect_lt(z_only$groups$Clusters_Needed[1], 20)
  r <- evaluation_sample_size(a)
  expect_false(is.null(r$df))
  expect_equal(r$df, sum(r$groups$Clusters_Needed) - nrow(r$groups))
  expect_gte(r$n_per_group, z_only$n_per_group)
  expect_equal(r$z[["alpha"]], qt(0.975, r$df))
  expect_gte(r$power_achieved, 0.80)
  expect_true(any(grepl("t-values", r$warnings)))
  expect_null(evaluation_sample_size(eval_args(clustered = TRUE, icc = 0.05, m = 10))$df)
})

test_that("evaluation: MDE inverts the sample size with every adjustment switched on", {
  for (d in c("independent", "panel", "did_cross", "did_panel", "ancova")) {
    for (outcome in c("proportion", "mean")) {
      a <- eval_args(design = d, outcome = outcome, p1 = if (outcome == "mean") 20 else 0.35, sd = 6,
                     effect = if (outcome == "mean") 1.5 else 0.10, rho = 0.3, attrition = 0.1, response_rate = 0.85,
                     clustered = TRUE, icc = 0.08, m = 8, inaccessible = 0.1, weighting = TRUE, cv_weights = 0.4,
                     population = 5000, matching_factor = 1.2, control_change = if (outcome == "mean") 0.5 else 0.03)
      r <- evaluation_sample_size(a)
      m <- evaluation_mde(modifyList(a, list(clusters_per_group = r$groups$Clusters[1])))
      target <- if (outcome == "mean") 1.5 else 0.10
      expect_lte(m$mde, target + 1e-9)
      expect_gt(m$mde, target * 0.9)
    }
  }
})

test_that("evaluation: relative change and rare outcomes", {
  rel <- evaluation_sample_size(eval_args(p1 = 0.30, effect_scale = "relative", effect_relative = 0.2, direction = "decrease"))
  abs_r <- evaluation_sample_size(eval_args(p1 = 0.30, effect = 0.06, direction = "decrease"))
  expect_equal(rel$n_base, abs_r$n_base)
  expect_true("Relative Change" %in% rel$steps$Parameter)

  skip_if_not_installed("pwr")
  rare <- evaluation_sample_size(eval_args(p1 = 0.05, effect = 0.05))
  expect_true(any(grepl("below 0.10", rare$warnings)))
  h <- pwr::ES.h(0.10, 0.05)
  expect_equal(rare$n_arcsine, pwr::pwr.2p.test(h = h, power = 0.80, sig.level = 0.05)$n, tolerance = 1e-4)   # pwr adds the far tail
  expect_length(evaluation_sample_size(eval_args(p1 = 0.3))$warnings, 0)
})

test_that("evaluation: methods paragraph for the report", {
  a <- eval_args(response_rate = 0.80, clustered = TRUE, icc = 0.10, m = 6, indicator_name = "Food security")
  txt <- evaluation_report_text(evaluation_sample_size(a), study = "PDM 2026")
  expect_match(txt, "PDM 2026")
  expect_match(txt, "Food security is a proportion with a baseline of 0.15")
  expect_match(txt, "10 percentage points")
  expect_match(txt, "design effect of 1.5")
  expect_match(txt, "80% response rate")
  expect_match(txt, "468 units in group 1")
  expect_match(txt, "78 clusters")
  mt <- evaluation_report_text(evaluation_mde(modifyList(a, list(clusters_per_group = 78))))
  expect_match(mt, "minimum detectable difference is")
  mm <- evaluation_report_text(evaluation_sample_size(mean_args(design = "ancova", rho = 0.3, attrition = 0.1)))
  expect_match(mm, "ANCOVA")
  expect_match(mm, "Cohen's d = 0.3")
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
