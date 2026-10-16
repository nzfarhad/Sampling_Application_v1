# Validation of the evaluation sample size engine (sample_size_functions.R):
# reference functions in R, simulation of the analyses, properties on random
# scenarios, the reference spreadsheet and reported cases.

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(root, "sampling_generation_functions.R"))
source(file.path(root, "sample_size_functions.R"))

base_args <- function(...) {
  modifyList(list(design = "independent", outcome = "proportion", p1 = 0.3, sd = 1, effect = 0.1, direction = "increase",
                  alpha = 0.05, power = 0.8, sided = 2, pooled = FALSE, ratio = 1, rho = 0, response_rate = 1,
                  attrition = 0, clustered = FALSE, icc = 0, m = 1), list(...))
}
n_of <- function(...) evaluation_sample_size(base_args(...))

# ---- Reference spreadsheet (resources_temp/evaluation_sample_size_calculator.xlsx) ----

test_that("validation: both cases of the reference spreadsheet", {
  r1 <- n_of(p1 = 0.15, effect = 0.10, response_rate = 0.8, clustered = TRUE, icc = 0.1, m = 6)
  expect_equal(r1$groups$After_Non_Response[1], 310)
  expect_equal(r1$adj$deff, 1.5)
  expect_equal(r1$groups$Clusters[1], 78)     # sheet: 77.5
  r2 <- n_of(p1 = 0.15, effect = 0.134, response_rate = 0.8, clustered = TRUE, icc = 0.1, m = 2)
  expect_equal(r2$groups$After_Non_Response[1], 181)
  expect_equal(r2$adj$deff, 1.1)
  expect_equal(r2$groups$Clusters[1], 100)    # sheet: 99.55
})

# ---- Against R's power functions, over grids ----------------------------------------------

test_that("validation: pooled two proportions equal power.prop.test on a grid (two- and one-sided)", {
  grid <- expand.grid(p1 = c(0.05, 0.2, 0.5, 0.8), d = c(0.05, 0.1, 0.2), power = c(0.8, 0.9), alpha = c(0.01, 0.05, 0.1),
                      sided = c(1, 2))
  grid <- grid[grid$p1 + grid$d < 1, ]
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    r <- n_of(p1 = g$p1, effect = g$d, power = g$power, alpha = g$alpha, sided = g$sided, pooled = TRUE)
    ref <- power.prop.test(p1 = g$p1, p2 = g$p1 + g$d, power = g$power, sig.level = g$alpha,
                           alternative = if (g$sided == 1) "one.sided" else "two.sided")$n
    expect_equal(r$n_base, ref, tolerance = 1e-4, info = paste(unlist(g), collapse = " "))
  }
})

test_that("validation: unpooled proportions and means match the closed-form z formula on a grid", {
  for (p1 in c(0.1, 0.3, 0.6)) for (d in c(0.05, 0.15)) for (k in c(0.5, 1, 2)) {
    r <- n_of(p1 = p1, effect = d, ratio = k)
    expect_equal(r$n_base, (qnorm(0.975) + qnorm(0.8))^2 * (p1 * (1 - p1) + (p1 + d) * (1 - p1 - d) / k) / d^2)
  }
  for (sd in c(1, 7, 300)) for (dd in c(0.1, 0.25, 0.5)) for (k in c(0.5, 1, 3)) {
    r <- n_of(outcome = "mean", p1 = 100, sd = sd, effect = dd * sd, ratio = k)
    expect_equal(r$n_base, (qnorm(0.975) + qnorm(0.8))^2 * sd^2 * (1 + 1 / k) / (dd * sd)^2)
  }
})

test_that("validation: means against power.t.test (two-sample, one-sided, paired) on a grid", {
  for (d in c(0.15, 0.25, 0.4)) for (power in c(0.8, 0.9)) for (alpha in c(0.01, 0.05)) {
    r <- n_of(outcome = "mean", p1 = 50, sd = 10, effect = 10 * d, power = power, alpha = alpha)
    ref <- power.t.test(delta = d, sd = 1, power = power, sig.level = alpha)$n
    expect_gt(ref, r$n_base)            # t needs slightly more than z
    expect_lt(ref - r$n_base, 4.5)
    one <- n_of(outcome = "mean", p1 = 50, sd = 10, effect = 10 * d, power = power, alpha = alpha, sided = 1)
    ref1 <- power.t.test(delta = d, sd = 1, power = power, sig.level = alpha, alternative = "one.sided")$n
    expect_lt(abs(ref1 - one$n_base), 4.5)
    for (rho in c(0, 0.3, 0.6)) {
      pan <- n_of(design = "panel", outcome = "mean", p1 = 50, sd = 10, effect = 10 * d, power = power, alpha = alpha, rho = rho)
      refp <- power.t.test(delta = d, sd = sqrt(2 * (1 - rho)), power = power, sig.level = alpha, type = "paired")$n
      expect_lt(abs(refp - pan$n_base), 4.5)   # t adds about z_alpha^2 / 2 + 1 for a paired test
    }
  }
})

test_that("validation: unequal groups reach the target power in pwr.t2n.test", {
  skip_if_not_installed("pwr")
  for (k in c(0.5, 2, 3)) {
    r <- n_of(outcome = "mean", p1 = 50, sd = 10, effect = 2.5, ratio = k)
    n1 <- r$groups$Sample_Size[1]
    pw <- pwr::pwr.t2n.test(n1 = n1, n2 = ceiling(k * n1), d = 0.25, sig.level = 0.05)$power
    expect_gt(pw, 0.795)
    expect_lt(pw, 0.82)
  }
})

# ---- Simulation of the analyses -------------------------------------------------------

# Share of significant results; the computed sample should give about 80%
reject_rate <- function(reps, fun) mean(replicate(reps, fun()))

test_that("validation (simulation): ANCOVA, panel DiD and cross-section DiD reach 80% power", {
  set.seed(2026)
  rho <- 0.5; tau <- 0.3
  anc <- n_of(design = "ancova", outcome = "mean", p1 = 0, sd = 1, effect = tau, rho = rho)$n_per_group
  power_anc <- reject_rate(2000, function() {
    treat <- rep(0:1, each = anc)
    y1 <- rnorm(2 * anc)
    y2 <- rho * y1 + sqrt(1 - rho^2) * rnorm(2 * anc) + tau * treat
    X <- cbind(1, treat, y1)
    fit <- lm.fit(X, y2)
    s2 <- sum(fit$residuals^2) / (length(y2) - 3)
    se <- sqrt(s2 * solve(crossprod(X))[2, 2])
    abs(fit$coefficients[2] / se) > qnorm(0.975)
  })
  expect_equal(power_anc, 0.80, tolerance = 0.035)

  rho <- 0.3
  did <- n_of(design = "did_panel", outcome = "mean", p1 = 0, sd = 1, effect = tau, rho = rho)$n_per_group
  power_did <- reject_rate(2000, function() {
    y1 <- matrix(rnorm(2 * did), ncol = 2)
    y2 <- rho * y1 + sqrt(1 - rho^2) * matrix(rnorm(2 * did), ncol = 2)
    y2[, 1] <- y2[, 1] + tau
    ch <- y2 - y1
    abs(diff(rev(colMeans(ch))) / sqrt(sum(apply(ch, 2, var)) / did)) > qnorm(0.975)
  })
  expect_equal(power_did, 0.80, tolerance = 0.035)

  dcs <- n_of(design = "did_cross", outcome = "mean", p1 = 0, sd = 1, effect = tau)$n_per_group
  power_dcs <- reject_rate(2000, function() {
    cells <- matrix(rnorm(4 * dcs), ncol = 4)      # T1, T2, C1, C2
    cells[, 2] <- cells[, 2] + tau
    m <- colMeans(cells)
    abs(((m[2] - m[1]) - (m[4] - m[3])) / sqrt(sum(apply(cells, 2, var)) / dcs)) > qnorm(0.975)
  })
  expect_equal(power_dcs, 0.80, tolerance = 0.035)
  # And the ordering the McKenzie note relies on (rho = 0.3): ANCOVA < endline only < DiD
  r <- n_of(design = "did_panel", outcome = "mean", p1 = 0, sd = 1, effect = tau, rho = 0.3)
  expect_true(all(diff(r$alternatives$Sample_per_Arm[c(2, 3, 1)]) > 0))
})

test_that("validation (simulation): clustered samples reach 80% power, with t-values when clusters are few", {
  set.seed(7)
  icc <- 0.1; m <- 10
  sim_clusters <- function(J, delta, crit) {
    reject_rate(2000, function() {
      cm <- function(shift) shift + rnorm(J, 0, sqrt(icc)) + rnorm(J, 0, sqrt((1 - icc) / m))   # cluster means
      a <- cm(0); b <- cm(delta)
      abs((mean(b) - mean(a)) / sqrt(var(a) / J + var(b) / J)) > crit(2 * J - 2)
    })
  }
  many <- n_of(outcome = "mean", p1 = 0, sd = 1, effect = 0.3, clustered = TRUE, icc = icc, m = m)
  expect_gt(many$groups$Clusters[1], 20)
  expect_equal(sim_clusters(many$groups$Clusters[1], 0.3, function(df) qnorm(0.975)), 0.80, tolerance = 0.035)

  few <- n_of(outcome = "mean", p1 = 0, sd = 1, effect = 0.7, clustered = TRUE, icc = icc, m = m)
  z_only <- n_of(outcome = "mean", p1 = 0, sd = 1, effect = 0.7, clustered = TRUE, icc = icc, m = m, small_cluster_t = FALSE)
  expect_lt(few$groups$Clusters[1], 20)
  expect_gt(few$groups$Clusters[1], z_only$groups$Clusters[1])
  # Analysed with a t-test on cluster means: t-based clusters reach the target, z-based ones fall short
  t_crit <- function(df) qt(0.975, df)
  p_t <- sim_clusters(few$groups$Clusters[1], 0.7, t_crit)
  p_z <- sim_clusters(z_only$groups$Clusters[1], 0.7, t_crit)
  expect_gt(p_t, 0.78)
  expect_lt(p_z, p_t)
})

test_that("validation (simulation): two proportions with unequal groups and response loss", {
  set.seed(19)
  r <- n_of(p1 = 0.25, effect = 0.10, ratio = 2, response_rate = 0.85)
  n1 <- r$groups$Sample_Size[1]; n2 <- r$groups$Sample_Size[2]
  pw <- reject_rate(4000, function() {
    c1 <- rbinom(1, n1, 0.85); c2 <- rbinom(1, n2, 0.85)   # completed interviews
    x1 <- rbinom(1, c1, 0.25) / c1; x2 <- rbinom(1, c2, 0.35) / c2
    abs((x2 - x1) / sqrt(x1 * (1 - x1) / c1 + x2 * (1 - x2) / c2)) > qnorm(0.975)
  })
  expect_equal(pw, 0.80, tolerance = 0.03)
})

# ---- Properties on random scenarios -----------------------------------------------------

random_scenario <- function() {
  design <- sample(c("independent", "panel", "did_cross", "did_panel", "ancova"), 1)
  is_mean <- runif(1) < 0.5
  clustered <- runif(1) < 0.7
  sd <- runif(1, 2, 30)
  base_args(
    design = design, outcome = if (is_mean) "mean" else "proportion",
    p1 = if (is_mean) runif(1, 10, 100) else runif(1, 0.1, 0.75), sd = sd,
    effect = if (is_mean) runif(1, 0.1, 0.5) * sd else runif(1, 0.04, 0.15),
    direction = sample(c("increase", "decrease"), 1), p_control = if (is_mean) runif(1, 10, 100) else runif(1, 0.15, 0.7),
    control_change = if (is_mean) runif(1, -1, 1) else runif(1, -0.02, 0.02),
    alpha = sample(c(0.01, 0.05, 0.1), 1), power = sample(c(0.8, 0.85, 0.9), 1), sided = sample(1:2, 1),
    ratio = sample(c(0.5, 1, 1.5, 2), 1), rho = runif(1, 0, 0.7), response_rate = runif(1, 0.7, 1),
    attrition = runif(1, 0, 0.25), clustered = clustered, m = sample(c(4, 8, 12, 20), 1), icc = runif(1, 0, 0.2),
    inaccessible = if (clustered) sample(c(0, 0.1), 1) else 0, weighting = runif(1) < 0.3, cv_weights = runif(1, 0, 0.6),
    matching_factor = sample(c(1, 1, 1.3), 1), population = sample(c(NA, NA, 3000, 20000), 1)
  )
}

# Proportion designs where the decrease or the control trend leaves the 0-1 range are skipped
safe_size <- function(a) tryCatch(evaluation_sample_size(a), error = function(e) NULL)

test_that("validation (random scenarios): power, rounding, totals and MDE inversion hold", {
  set.seed(42)
  checked <- 0
  for (i in 1:250) {
    a <- random_scenario()
    r <- safe_size(a)
    if (is.null(r)) next
    checked <- checked + 1
    info <- paste(names(a), unlist(a), sep = "=", collapse = " ")
    expect_gte(r$power_achieved, a$power - 1e-6)
    expect_equal(r$total_sample, sum(r$groups$Sample_Size), info = info)
    expect_equal(r$total_interviews, sum(r$groups$Sample_Size * r$groups$rounds), info = info)
    if (r$adj$clustered) expect_true(all(r$groups$Sample_Size %% a$m == 0), info = info)
    mde_args <- modifyList(a, if (r$adj$clustered) list(clusters_per_group = r$groups$Clusters[1]) else list(n_per_group = r$n_per_group))
    m <- tryCatch(evaluation_mde(mde_args), error = function(e) NULL)
    if (!is.null(m)) expect_lte(m$mde, abs(r$terms$effect) * (1 + 1e-6), label = info)
  }
  expect_gt(checked, 150)
})

test_that("validation (random scenarios): the sample moves the right way with each input", {
  set.seed(99)
  more <- function(a, change) modifyList(a, change)
  for (i in 1:120) {
    a <- random_scenario()
    r <- safe_size(a)
    if (is.null(r)) next
    n0 <- r$total_sample
    info <- paste(names(a), unlist(a), sep = "=", collapse = " ")
    up <- list(list(power = min(a$power + 0.05, 0.95)), list(alpha = a$alpha / 2), list(effect = a$effect * 0.9),
               list(response_rate = a$response_rate - 0.05), list(icc = a$icc + 0.03), list(matching_factor = a$matching_factor + 0.1),
               list(cv_weights = a$cv_weights + 0.2), list(inaccessible = if (a$clustered) a$inaccessible + 0.05 else 0))
    if (a$design %in% EVALUATION_PANEL_DESIGNS) {
      up <- c(up, list(list(attrition = a$attrition + 0.05), list(rho = max(a$rho - 0.1, 0))))
    }
    for (ch in up) {
      r2 <- safe_size(more(a, ch))
      if (!is.null(r2)) expect_gte(r2$total_sample, n0, label = paste(names(ch), "|", info))
    }
  }
})

test_that("validation: the sample never falls as the requirement rises across the 20-cluster boundary", {
  for (d in c(0.1, 0.25)) {
    effects <- seq(0.9, 0.2, by = -0.005)
    n <- vapply(effects, function(e) n_of(outcome = "mean", p1 = 0, sd = 1, effect = e, clustered = TRUE, icc = d, m = 10)$n_per_group,
                numeric(1))
    expect_true(all(diff(n) >= 0))
    pw <- vapply(effects, function(e) n_of(outcome = "mean", p1 = 0, sd = 1, effect = e, clustered = TRUE, icc = d, m = 10)$power_achieved,
                 numeric(1))
    expect_true(all(pw >= 0.8 - 1e-6))
  }
})

test_that("validation: symmetry and scale invariance", {
  # A proportion p rising by D needs the same sample as 1 - p falling by D
  for (p in c(0.1, 0.35, 0.6)) {
    expect_equal(n_of(p1 = p, effect = 0.08)$n_base, n_of(p1 = 1 - p, effect = 0.08, direction = "decrease")$n_base)
  }
  # Means: only D / SD matters
  for (design in c("independent", "panel", "did_cross", "did_panel", "ancova")) {
    a <- n_of(design = design, outcome = "mean", p1 = 40, sd = 12, effect = 3, rho = 0.3)
    b <- n_of(design = design, outcome = "mean", p1 = 4000, sd = 1200, effect = 300, rho = 0.3)
    expect_equal(a$n_per_group, b$n_per_group)
  }
  # Mean, d and relative entries give the same answer
  expect_equal(n_of(outcome = "mean", p1 = 40, sd = 12, effect = 3)$n_base,
               n_of(outcome = "mean", p1 = 40, sd = 12, effect_scale = "d", effect_d = 0.25)$n_base)
})

# ---- Change scale of the indicator table (reported case) ----------------------------------

test_that("validation: points read as % of baseline give an implausible sample, flagged by a warning", {
  tab <- data.frame(Indicator = c("FCS", "Poor FC"), Type = c("mean", "proportion"), Baseline = c(40, 0.3), SD = c(12, NA),
                    Change = c(3, -0.08), ICC = NA, stringsAsFactors = FALSE)
  a <- base_args(response_rate = 0.9, clustered = TRUE, deff_method = "known", deff_known = 1.5, m = 6,
                 matching_factor = 1.3, indicators = tab)
  right <- evaluation_sample_size(modifyList(a, list(effect_scale = "absolute")))
  expect_equal(right$indicators$Sample_per_Group, c(546, 1014))
  expect_length(right$warnings, 0)
  wrong <- evaluation_sample_size(modifyList(a, list(effect_scale = "relative")))
  expect_gt(wrong$n_per_group, 1e8)
  expect_true(any(grepl("very small", wrong$warnings)))
  expect_true(any(grepl("very large", wrong$warnings)))
  # Converting the table with the scale keeps the answer
  conv <- evaluation_convert_change(tab, "absolute", "relative")
  expect_equal(conv$Change, c(7.5, -0.08 / 0.3 * 100), tolerance = 1e-5)   # kept to 6 significant digits
  same <- evaluation_sample_size(modifyList(a, list(effect_scale = "relative", indicators = conv)))
  expect_equal(same$indicators$Sample_per_Group, right$indicators$Sample_per_Group)
})

test_that("validation: converting the change scale round-trips and leaves proportions out of d", {
  tab <- data.frame(Indicator = c("A", "B", "C"), Type = c("mean", "proportion", "mean"), Baseline = c(40, 0.3, 0),
                    SD = c(12, NA, 5), Change = c(3, -0.08, 1), stringsAsFactors = FALSE)
  for (to in c("relative", "d")) {
    back <- evaluation_convert_change(evaluation_convert_change(tab, "absolute", to), to, "absolute")
    ok <- !is.na(back$Change)
    expect_equal(back$Change[ok], tab$Change[ok], tolerance = 1e-5)
  }
  d <- evaluation_convert_change(tab, "absolute", "d")
  expect_equal(d$Change, c(0.25, -0.08, 0.2))       # the proportion is not converted
  # relative -> d -> absolute keeps the proportion right (it goes back to points on the d scale)
  chain <- evaluation_convert_change(evaluation_convert_change(evaluation_convert_change(tab[1:2, ], "absolute", "relative"),
                                                               "relative", "d"), "d", "absolute")
  expect_equal(chain$Change, tab$Change[1:2], tolerance = 1e-5)
  # On the d scale the engine reads a proportion's change in points
  a <- base_args(indicators = tab[1:2, ])
  expect_equal(evaluation_sample_size(modifyList(a, list(effect_scale = "d", indicators = d[1:2, ])))$indicators$Sample_per_Group,
               evaluation_sample_size(modifyList(a, list(effect_scale = "absolute")))$indicators$Sample_per_Group)
  rel <- evaluation_convert_change(tab, "absolute", "relative")
  expect_true(is.na(rel$Change[3]))                 # no relative change from a baseline of 0
  expect_identical(evaluation_convert_change(tab, "value2", "absolute"), tab)
})
