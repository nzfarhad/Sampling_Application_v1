# Sample Size Functions
# Precision (monitoring) and power (evaluation) formulas used by the two
# calculator modules. No Shiny code here, so every function can be tested
# directly (see tests/testthat/test-sample-size.R).
#
# Needs allocate_sample() from sampling_generation_functions.R for the
# "precision for the total" stratified allocation.

# ---------------------------------------------------------------------------
# Monitoring: sample size for a target margin of error
# ---------------------------------------------------------------------------

#' Variance term of the indicator: p(1-p) for a proportion, sd^2 for a mean
indicator_variance <- function(indicator = "proportion", p = 0.5, sd = NA) {
  if (identical(indicator, "mean")) sd^2 else p * (1 - p)
}

calculate_simple_random_sample_size <- function(N, p = 0.5, confidence_level = 0.95, margin_error = 0.05,
                                                non_response_rate = 0.05, variance = p * (1 - p)) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  n <- z^2 * variance / margin_error^2
  n_adjusted <- n / (1 + (n - 1) / N)
  # Apply non-response rate adjustment
  ceiling(n_adjusted / (1 - non_response_rate))
}

calculate_cluster_sample_size <- function(N, p = 0.5, confidence_level = 0.95, margin_error = 0.05,
                                          cluster_size = 10, icc = 0.06, non_response_rate = 0.05,
                                          variance = p * (1 - p)) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  n <- z^2 * variance / margin_error^2
  n_cluster <- n * (1 + (cluster_size - 1) * icc)
  n_adjusted <- n_cluster / (1 + (n_cluster - 1) / N)
  n_final <- n_adjusted / (1 - non_response_rate)
  # Round up to whole clusters of cluster_size interviews each
  ceiling(n_final / cluster_size) * cluster_size
}

# Margin of error achieved by a sample of n units (before non-response):
# completed interviews n(1 - nr), deflated by the design effect, with finite
# population correction. Inverse of the sample size formulas above.
calculate_margin_of_error <- function(n, N, p = 0.5, confidence_level = 0.95, non_response_rate = 0, deff = 1,
                                      variance = p * (1 - p)) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  n_completed <- n * (1 - non_response_rate)
  fpc <- pmax(0, (N - n_completed) / (N - 1))
  z * sqrt(deff * variance / n_completed * fpc)
}

# Kept for test_app.R: total of the per-stratum sample sizes
calculate_stratified_sample_size <- function(strata_data, p = 0.5, confidence_level = 0.95, margin_error = 0.05,
                                             non_response_rate = 0.05) {
  sum(vapply(strata_data$Population, calculate_simple_random_sample_size, numeric(1),
             p = p, confidence_level = confidence_level, margin_error = margin_error,
             non_response_rate = non_response_rate))
}

#' Stratified Sample Size
#'
#' @param strata Data frame with Stratum, Population and optionally Variability
#'   (expected p, or sd for a mean, per stratum; NA uses the common value)
#' @param target "per_stratum": each stratum reaches the margin of error on its
#'   own (sample sizes add up). "total": the estimate for the whole population
#'   reaches it, and the sample is split across strata by `allocation`
#' @param allocation "proportional", "neyman" or "equal" (target = "total" only)
#' @param cluster_size Interviews per cluster (m); NULL for element sampling
#'
#' @return List with the total sample size, number of clusters, design effect,
#'   per-stratum breakdown and the expected margin of error for the total
stratified_sample_size <- function(strata, confidence_level = 0.95, margin_error = 0.05,
                                   indicator = "proportion", p = 0.5, sd = NA, non_response_rate = 0.05,
                                   target = "per_stratum", allocation = "proportional", min_per_stratum = 0,
                                   cluster_size = NULL, icc = 0) {
  N_h <- as.numeric(strata$Population)
  if (length(N_h) < 1 || any(is.na(N_h) | N_h <= 0)) stop("Every stratum needs a population above 0")
  z <- qnorm(1 - (1 - confidence_level) / 2)
  var_in <- if ("Variability" %in% names(strata)) as.numeric(strata$Variability) else rep(NA_real_, length(N_h))
  S2 <- ifelse(is.na(var_in), indicator_variance(indicator, p, sd), indicator_variance(indicator, var_in, var_in))
  clustered <- !is.null(cluster_size)
  deff <- if (clustered) 1 + (cluster_size - 1) * icc else 1
  rr <- 1 - non_response_rate
  W <- N_h / sum(N_h)
  share <- rep(NA_real_, length(N_h))
  n_required <- NA_real_

  if (target == "per_stratum") {
    n0 <- z^2 * S2 * deff / margin_error^2
    n_completed <- n0 / (1 + (n0 - 1) / N_h)
  } else {
    S <- sqrt(S2)
    share <- switch(allocation,
      proportional = W,
      equal = rep(1 / length(N_h), length(N_h)),
      neyman = if (sum(N_h * S) > 0) N_h * S / sum(N_h * S) else W,
      stop("Unknown allocation method: ", allocation)
    )
    # Var(stratified mean) = sum W_h^2 DEFF S_h^2 / n_h - sum W_h DEFF S_h^2 / N, with n_h = n * share_h
    V <- (margin_error / z)^2
    n_required <- sum(W^2 * deff * S2 / share) / (V + sum(W * deff * S2) / sum(N_h))
    n_completed <- allocate_sample(N_h, min(ceiling(n_required - 1e-9), sum(N_h)), allocation,
                                   min_n = min_per_stratum, values = S)
  }

  n_inflated <- n_completed / rr
  if (clustered) {
    clusters_h <- ceiling(n_inflated / cluster_size - 1e-9)
    n_h <- clusters_h * cluster_size
  } else {
    clusters_h <- NULL
    n_h <- pmin(ceiling(n_inflated - 1e-9), N_h)
  }

  # Expected precision after non-response, design effect and FPC
  n_c <- n_h * rr
  moe_h <- z * sqrt(pmax(0, deff * S2 / n_c * (N_h - n_c) / pmax(N_h - 1, 1)))
  moe_total <- z * sqrt(max(0, sum(W^2 * deff * S2 / n_c * (1 - n_c / N_h))))

  breakdown <- data.frame(
    Stratum = as.character(strata$Stratum),
    Population_Size = N_h,
    Expected_Value = if (indicator == "mean") sqrt(S2) else ifelse(is.na(var_in), p, var_in),
    stringsAsFactors = FALSE
  )
  names(breakdown)[3] <- if (indicator == "mean") "Standard_Deviation" else "Expected_Proportion"
  if (target == "total") breakdown$Allocation_Share_pct <- round(share * 100, 1)
  breakdown$Completed_Interviews_Needed <- ceiling(n_completed - 1e-9)
  breakdown$Sample_Size <- n_h
  if (clustered) breakdown$Clusters <- clusters_h
  breakdown$Sampling_Fraction_pct <- round(n_h / N_h * 100, 2)
  breakdown$Expected_Margin_of_Error <- moe_h

  list(
    total = sum(n_h),
    n_clusters = if (clustered) sum(clusters_h) else NULL,
    deff = deff,
    n_required = n_required,
    breakdown = breakdown,
    moe_total = moe_total
  )
}

#' Monitoring Sample Size Calculation
#'
#' Single entry point for the Sample Calculator. Builds the step-by-step table
#' shown on screen and in the exports, so every output reports the same numbers.
#'
#' @param spec List: method ("simple_random", "systematic", "cluster",
#'   "stratified", "stratified_cluster"), N, confidence_level, margin_error,
#'   indicator ("proportion" or "mean"), p, sd, non_response_rate (0-1),
#'   cluster_size, icc, strata (data frame), target, allocation, min_per_stratum
#'
#' @return List with sample_size, n_clusters, deff, steps (Parameter, Value,
#'   Formula), breakdown (stratified only), moe_achieved and explanation
monitoring_calculation <- function(spec) {
  method <- spec$method
  indicator <- if (is.null(spec$indicator)) "proportion" else spec$indicator
  is_mean <- indicator == "mean"
  conf <- spec$confidence_level
  e <- spec$margin_error
  nr <- spec$non_response_rate
  p <- if (is.null(spec$p)) 0.5 else spec$p
  sd <- if (is.null(spec$sd)) NA else spec$sd
  variance <- indicator_variance(indicator, p, sd)
  is_cluster <- method %in% c("cluster", "stratified_cluster")
  is_strat <- method %in% c("stratified", "stratified_cluster")
  m <- if (is_cluster) spec$cluster_size else NULL
  icc <- if (is_cluster) spec$icc else 0
  deff <- if (is_cluster) 1 + (m - 1) * icc else 1
  z <- qnorm(1 - (1 - conf) / 2)

  if (is.null(e) || is.na(e) || e <= 0) stop("Margin of error must be above 0")
  if (is_mean && (is.na(sd) || sd <= 0)) stop("Enter a standard deviation above 0")
  if (!is_mean && (is.na(p) || p <= 0 || p >= 1)) stop("Expected proportion must be between 0 and 1")
  if (is.na(nr) || nr < 0 || nr >= 1) stop("Non-response rate must be from 0 to 99%")
  if (is_cluster && (is.null(m) || is.na(m) || m < 1)) stop("Interviews per cluster (m) must be 1 or more")

  fmt_e <- function(x) if (is_mean) format(signif(x, 4)) else paste0(round(x * 100, 2), "%")
  fmt_n <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",")
  steps <- data.frame(Parameter = character(0), Value = character(0), Formula = character(0), stringsAsFactors = FALSE)
  add <- function(parameter, value, formula = "Given") {
    steps[nrow(steps) + 1, ] <<- list(parameter, as.character(value), formula)
  }
  method_name <- c(simple_random = "Simple Random Sampling", systematic = "Systematic Sampling",
                   cluster = "Cluster Sampling", stratified = "Stratified Sampling",
                   stratified_cluster = "Stratified Cluster Sampling")[[method]]

  if (is_strat) {
    strata <- spec$strata
    st <- stratified_sample_size(strata, conf, e, indicator, p, sd, nr,
                                 target = spec$target, allocation = spec$allocation,
                                 min_per_stratum = if (is.null(spec$min_per_stratum)) 0 else spec$min_per_stratum,
                                 cluster_size = m, icc = icc)
    N <- sum(strata$Population)
    n <- st$total
    n_clusters <- st$n_clusters
    moe <- st$moe_total
    breakdown <- st$breakdown
  } else {
    N <- spec$N
    if (is.null(N) || is.na(N) || N < 1) stop("Population size must be 1 or more")
    n0 <- z^2 * variance / e^2
    n_deff <- n0 * deff
    fpc <- 1 + (n_deff - 1) / N
    n_adj <- n_deff / fpc
    n_inflated <- n_adj / (1 - nr)
    if (is_cluster) {
      n_clusters <- ceiling(n_inflated / m - 1e-9)
      n <- n_clusters * m
    } else {
      n_clusters <- NULL
      n <- ceiling(n_inflated - 1e-9)
    }
    moe <- calculate_margin_of_error(n, N, confidence_level = conf, non_response_rate = nr,
                                     deff = deff, variance = variance)
    breakdown <- NULL
  }

  # Steps table
  add("Population Size (N)", format(N, big.mark = ","), if (is_strat) "Sum of stratum populations" else "Given")
  if (is_strat) add("Number of Strata", nrow(spec$strata))
  add("Indicator", if (is_mean) "Mean (continuous)" else "Proportion")
  add("Confidence Level", paste0(conf * 100, "%"))
  add("Margin of Error (e)", fmt_e(e), if (is_mean) "Given, in the indicator's units" else "Given")
  if (is_mean) {
    add("Standard Deviation (σ)", sd)
  } else {
    add("Expected Proportion (p)", p)
  }
  add("Non-Response Rate", paste0(round(nr * 100, 2), "%"))
  add("Z-score (α/2)", round(z, 3), "qnorm(1 - α/2)")
  add("Initial Sample Size (n₀)", fmt_n(z^2 * variance / e^2), if (is_mean) "z²σ²/e²" else "z²p(1-p)/e²")
  if (is_cluster) {
    add("Interviews per Cluster (m)", m)
    add("Intraclass Correlation (ICC)", round(icc, 3))
    add("Design Effect (DEFF)", round(deff, 3), "1 + (m-1) × ICC")
  }

  if (!is_strat) {
    if (is_cluster) add("Sample Size × DEFF", fmt_n(n_deff), "n₀ × DEFF")
    add("Finite Population Correction", round(fpc, 4), if (is_cluster) "1 + (n₀ × DEFF - 1)/N" else "1 + (n₀ - 1)/N")
    add("Completed Interviews Needed", fmt_n(n_adj), if (is_cluster) "n₀ × DEFF / FPC" else "n₀ / FPC")
    add("Adjusted for Non-Response", fmt_n(n_inflated), "Completed interviews / (1 - non-response rate)")
    if (is_cluster) {
      add("Number of Clusters", n_clusters, "ceiling(adjusted n / m)")
      add("Final Sample Size (n)", n, "Number of clusters × m")
    } else {
      add("Final Sample Size (n)", n, "Adjusted n, rounded up")
    }
  } else if (spec$target == "per_stratum") {
    add("Precision Target", "Each stratum", "Every stratum reaches the margin of error on its own")
    add("Sample Size per Stratum", "See breakdown",
        paste0(if (is_cluster) "n₀ × DEFF" else "n₀", " / FPCₕ / (1 - non-response)",
               if (is_cluster) ", rounded up to whole clusters of m" else ", using each stratum's Nₕ"))
    if (is_cluster) add("Number of Clusters", n_clusters, "Sum of clusters over strata")
    add("Final Sample Size (n)", n, "Sum of stratum sample sizes")
  } else {
    alloc_label <- c(proportional = "Proportional to size", neyman = "Neyman (optimal)", equal = "Equal")[[spec$allocation]]
    add("Precision Target", "Whole population", "Only the overall estimate reaches the margin of error")
    add("Allocation", alloc_label,
        c(proportional = "aₕ = Nₕ/N", neyman = "aₕ = NₕSₕ / Σ NₕSₕ", equal = "aₕ = 1/H")[[spec$allocation]])
    add("Target Variance (V)", signif((e / z)^2, 4), "(e / z)²")
    add("Completed Interviews Needed", fmt_n(st$n_required),
        paste0("Σ Wₕ²", if (is_cluster) "·DEFF" else "", "·Sₕ²/aₕ / (V + Σ Wₕ", if (is_cluster) "·DEFF" else "", "·Sₕ²/N)"))
    add("Allocated Completed Interviews", sum(breakdown$Completed_Interviews_Needed),
        "n × aₕ per stratum (rounded, capped at Nₕ, minimum per stratum applied)")
    if (is_cluster) add("Number of Clusters", n_clusters, "Per stratum: ceiling(nₕ / (1 - non-response) / m)")
    add("Final Sample Size (n)", n, if (is_cluster) "Sum of clusters × m" else "Sum of nₕ / (1 - non-response), rounded up")
  }
  add("Expected Margin of Error", fmt_e(moe),
      paste0(if (is_strat) "For the total, after" else "After", " non-response", if (is_cluster) ", DEFF" else "", " and FPC"))

  # Plain-language explanation
  explanation <- paste0(
    "For a population of ", format(N, big.mark = ","), " with ", conf * 100, "% confidence and a ",
    fmt_e(e), " margin of error", if (is_mean) paste0(" (standard deviation ", sd, ")") else "",
    ", you need a sample size of ", n, " using ", method_name,
    " (adjusted for ", round(nr * 100, 2), "% non-response)."
  )
  if (is_strat) {
    explanation <- paste0(explanation, if (spec$target == "per_stratum") {
      " Each stratum is sized to reach the margin of error on its own, so the stratum sample sizes add up."
    } else {
      paste0(" The margin of error applies to the whole-population estimate; the sample is split across strata (",
             c(proportional = "proportional", neyman = "Neyman", equal = "equal")[[spec$allocation]],
             " allocation), so single strata are less precise.")
    })
  }
  if (is_cluster) {
    explanation <- paste0(explanation, " Design effect (DEFF): ", round(deff, 3), " (", m, " interviews per cluster, ICC ",
                          round(icc, 3), "). This is ", n_clusters, " clusters of ", m, " interviews.")
  }
  explanation <- paste0(explanation, " Expected margin of error", if (is_strat) " for the total" else "", ": ", fmt_e(moe), ".")

  list(
    sample_size = n,
    method_name = method_name,
    population_size = N,
    confidence_level = conf,
    margin_error = e,
    indicator = indicator,
    expected_proportion = p,
    sd = sd,
    variance = variance,
    non_response_rate = nr,
    cluster_size = m,
    icc = if (is_cluster) icc else NULL,
    deff = deff,
    n_clusters = n_clusters,
    moe_achieved = moe,
    breakdown = breakdown,
    steps = steps,
    explanation = explanation
  )
}

# ---------------------------------------------------------------------------
# Small helpers for the calculator modules
# ---------------------------------------------------------------------------

#' TRUE for a missing, empty or unselected Shiny input
is_blank_input <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[1]) || trimws(as.character(x[1])) == ""
}

#' Replace symbols that the default PDF (LaTeX) engine cannot print
ascii_text <- function(x) {
  map <- c("₀" = "0", "₁" = "1", "₂" = "2", "ₕ" = "_h", "²" = "^2", "σ" = "sigma", "α" = "alpha",
           "β" = "beta", "ρ" = "rho", "Σ" = "sum ", "Φ" = "Phi", "√" = "sqrt", "×" = "x", "→" = "->",
           "±" = "+/-", "p̄" = "p-bar", "·" = "*", "–" = "-", "≥" = ">=", "≤" = "<=")
  for (s in names(map)) x <- gsub(s, map[[s]], x, fixed = TRUE)
  x
}

#' Data frame as a pipe table for R Markdown reports
markdown_table <- function(df) {
  cells <- as.data.frame(lapply(df, function(col) gsub("|", "/", ascii_text(as.character(col)), fixed = TRUE)),
                         stringsAsFactors = FALSE)
  header <- ascii_text(names(df))
  paste0(
    "| ", paste(header, collapse = " | "), " |\n",
    "|", paste(rep("---", length(header)), collapse = "|"), "|\n",
    paste(apply(cells, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")), collapse = "\n")
  )
}

# ---------------------------------------------------------------------------
# Evaluation: sample size and minimum detectable difference for comparing
# proportions (two groups, panel, difference-in-differences)
# ---------------------------------------------------------------------------

EVALUATION_DESIGNS <- c(
  "Two independent groups (treatment vs control, or baseline vs endline cross-sections)" = "independent",
  "Panel: same households at baseline and endline" = "panel",
  "Difference-in-differences, repeated cross-sections" = "did_cross",
  "Difference-in-differences, panel" = "did_panel"
)

#' Expected proportions for an evaluation scenario
#'
#' Independent and panel designs: p1 (baseline or comparison group) and p2,
#' given directly or as p1 +/- effect. DiD: treatment and control at baseline
#' and endline; both arms move by control_change, and the treatment arm moves
#' by the effect on top of that.
evaluation_proportions <- function(a, effect = a$effect) {
  sgn <- if (identical(a$direction, "decrease")) -1 else 1
  if (a$design %in% c("independent", "panel")) {
    p2 <- if (!is.null(a$p2) && !is.na(a$p2) && missing(effect)) a$p2 else a$p1 + sgn * effect
    p <- c(p1 = a$p1, p2 = p2)
  } else {
    pc <- if (is.null(a$p_control) || is.na(a$p_control)) a$p1 else a$p_control
    change <- if (is.null(a$control_change) || is.na(a$control_change)) 0 else a$control_change
    p <- c(t1 = a$p1, t2 = a$p1 + change + sgn * effect, c1 = pc, c2 = pc + change)
  }
  if (any(is.na(p) | p <= 0 | p >= 1)) {
    stop("Expected proportions must be between 0 and 1 (", paste(names(p), round(p, 3), sep = " = ", collapse = ", "), ")")
  }
  p
}

#' Effect and variance terms: n per group = (z_a sqrt(v0) + z_b sqrt(v1))^2 / effect^2
evaluation_terms <- function(a, p) {
  pq <- p * (1 - p)
  k <- if (is.null(a$ratio) || is.na(a$ratio)) 1 else a$ratio
  rho <- if (is.null(a$rho) || is.na(a$rho)) 0 else a$rho
  if (a$design == "independent") {
    effect <- p[["p2"]] - p[["p1"]]
    v1 <- pq[["p1"]] + pq[["p2"]] / k
    v0 <- v1
    if (isTRUE(a$pooled)) {
      pbar <- (p[["p1"]] + k * p[["p2"]]) / (1 + k)
      v0 <- pbar * (1 - pbar) * (1 + 1 / k)
    }
  } else if (a$design == "panel") {
    effect <- p[["p2"]] - p[["p1"]]
    v1 <- pq[["p1"]] + pq[["p2"]] - 2 * rho * sqrt(pq[["p1"]] * pq[["p2"]])
    v0 <- v1
  } else {
    effect <- (p[["t2"]] - p[["t1"]]) - (p[["c2"]] - p[["c1"]])
    r <- if (a$design == "did_panel") rho else 0
    vT <- pq[["t1"]] + pq[["t2"]] - 2 * r * sqrt(pq[["t1"]] * pq[["t2"]])
    vC <- pq[["c1"]] + pq[["c2"]] - 2 * r * sqrt(pq[["c1"]] * pq[["c2"]])
    v1 <- vT + vC / k
    v0 <- v1
  }
  list(effect = effect, v0 = v0, v1 = v1)
}

evaluation_z <- function(a) {
  sided <- if (is.null(a$sided)) 2 else a$sided
  c(alpha = qnorm(1 - a$alpha / sided), beta = qnorm(a$power))
}

#' Completed interviews per group (group 1 / treatment cell), before clustering
evaluation_base_n <- function(a, terms) {
  z <- evaluation_z(a)
  (z[["alpha"]] * sqrt(terms$v0) + z[["beta"]] * sqrt(terms$v1))^2 / terms$effect^2
}

#' Power for a given number of effective completed interviews in group 1
evaluation_power <- function(a, terms, n_eff) {
  z <- evaluation_z(a)
  pnorm((abs(terms$effect) * sqrt(n_eff) - z[["alpha"]] * sqrt(terms$v0)) / sqrt(terms$v1))
}

#' Share of sampled units that end up as usable interviews, and the design effect
evaluation_adjustments <- function(a) {
  panel <- a$design %in% c("panel", "did_panel")
  attrition <- if (panel && !is.null(a$attrition) && !is.na(a$attrition)) a$attrition else 0
  clustered <- isTRUE(a$clustered) && !is.null(a$m) && !is.na(a$m) && a$m > 1
  list(
    panel = panel,
    attrition = attrition,
    retention = a$response_rate * (1 - attrition),
    clustered = clustered,
    m = if (clustered) a$m else 1,
    icc = if (clustered) a$icc else 0,
    deff = if (clustered) 1 + (a$m - 1) * a$icc else 1
  )
}

#' Groups that are sampled under each design, with their size relative to group 1
evaluation_groups <- function(a) {
  k <- if (is.null(a$ratio) || is.na(a$ratio)) 1 else a$ratio
  switch(a$design,
    independent = data.frame(Group = c("Group 1 (comparison / baseline)", "Group 2 (treatment / endline)"),
                             multiplier = c(1, k), rounds = 1),
    panel = data.frame(Group = "Panel households (interviewed at baseline and endline)", multiplier = 1, rounds = 2),
    did_cross = data.frame(Group = c("Treatment - baseline", "Treatment - endline", "Control - baseline", "Control - endline"),
                           multiplier = c(1, 1, k, k), rounds = 1),
    did_panel = data.frame(Group = c("Treatment panel households", "Control panel households"),
                           multiplier = c(1, k), rounds = 2)
  )
}

evaluation_check <- function(a) {
  if (is.na(a$alpha) || a$alpha <= 0 || a$alpha >= 0.5) stop("Significance level must be between 0 and 0.5")
  if (is.na(a$power) || a$power <= 0.5 || a$power >= 1) stop("Power must be between 0.5 and 1")
  if (is.na(a$response_rate) || a$response_rate <= 0 || a$response_rate > 1) stop("Response rate must be above 0% and at most 100%")
  if (!is.null(a$attrition) && !is.na(a$attrition) && (a$attrition < 0 || a$attrition >= 1)) stop("Attrition must be from 0 to 99%")
  if (!is.null(a$rho) && !is.na(a$rho) && (a$rho < 0 || a$rho >= 1)) stop("Correlation must be from 0 to 0.99")
  if (!is.null(a$ratio) && !is.na(a$ratio) && a$ratio <= 0) stop("Allocation ratio must be above 0")
  if (isTRUE(a$clustered) && (is.na(a$m) || a$m < 1 || is.na(a$icc) || a$icc < 0 || a$icc > 1)) {
    stop("Enter interviews per cluster (1 or more) and an ICC from 0 to 1")
  }
}

#' Sample size for detecting a difference in proportions
#'
#' @param a Scenario list: design (see EVALUATION_DESIGNS), p1, p2 (optional),
#'   effect, direction ("increase"/"decrease"), p_control, control_change (DiD),
#'   alpha, power, sided (1 or 2), pooled (independent only), ratio (group 2 or
#'   control : group 1 or treatment), rho (panel), response_rate (0-1),
#'   attrition (panel, 0-1), clustered, icc, m
#'
#' @return List with proportions, terms, adjustments, groups table, total
#'   interviews, power achieved and a step-by-step table
evaluation_sample_size <- function(a) {
  evaluation_check(a)
  p <- evaluation_proportions(a)
  terms <- evaluation_terms(a, p)
  if (abs(terms$effect) < 1e-9) stop("The detectable difference must not be 0")
  adj <- evaluation_adjustments(a)
  z <- evaluation_z(a)
  n_base <- evaluation_base_n(a, terms)

  groups <- evaluation_groups(a)
  need <- n_base * groups$multiplier
  groups$Completed_Needed <- ceiling(need - 1e-9)
  groups$After_Non_Response <- ceiling(need / adj$retention - 1e-9)
  if (adj$clustered) {
    groups$Clusters <- ceiling(need / adj$retention * adj$deff / adj$m - 1e-9)
    groups$Sample_Size <- groups$Clusters * adj$m
  } else {
    groups$Sample_Size <- groups$After_Non_Response
  }
  groups$Interviews <- groups$Sample_Size * groups$rounds
  power_achieved <- evaluation_power(a, terms, min(groups$Sample_Size / groups$multiplier) * adj$retention / adj$deff)

  res <- list(mode = "sample_size", args = a, p = p, terms = terms, adj = adj, z = z, n_base = n_base,
              groups = groups, n_per_group = groups$Sample_Size[1], total_sample = sum(groups$Sample_Size),
              total_interviews = sum(groups$Interviews), power_achieved = power_achieved)
  res$steps <- evaluation_steps(res)
  res
}

#' Minimum detectable difference for a fixed sample
#'
#' @param a Scenario list as for evaluation_sample_size; the sample is
#'   a$clusters_per_group (with a$m) when clustered, otherwise a$n_per_group,
#'   both for group 1 / the treatment cell
evaluation_mde <- function(a) {
  evaluation_check(a)
  adj <- evaluation_adjustments(a)
  n_group <- if (adj$clustered) a$clusters_per_group * adj$m else a$n_per_group
  if (is.null(n_group) || is.na(n_group) || n_group < 2) stop("Enter the sample per group (at least 2 interviews)")
  n_eff <- n_group * adj$retention / adj$deff

  sgn <- if (identical(a$direction, "decrease")) -1 else 1
  start <- if (a$design %in% c("did_cross", "did_panel")) {
    a$p1 + (if (is.null(a$control_change) || is.na(a$control_change)) 0 else a$control_change)
  } else a$p1
  hi <- (if (sgn > 0) 1 - start else start) - 1e-6
  if (hi <= 1e-6) stop("The baseline proportion leaves no room for a change in that direction")
  gap <- function(d) evaluation_base_n(a, evaluation_terms(a, evaluation_proportions(a, d))) - n_eff
  if (gap(hi) > 0) {
    stop("This sample cannot detect any change in that direction with the requested power. Increase the sample.")
  }
  mde <- uniroot(gap, c(1e-6, hi), tol = 1e-10)$root
  p <- evaluation_proportions(a, mde)
  terms <- evaluation_terms(a, p)

  groups <- evaluation_groups(a)
  groups$Sample_Size <- n_group * groups$multiplier
  if (adj$clustered) groups$Clusters <- a$clusters_per_group * groups$multiplier
  groups$Expected_Completed <- floor(groups$Sample_Size * adj$retention + 1e-9)
  groups$Interviews <- groups$Sample_Size * groups$rounds

  res <- list(mode = "mde", args = a, p = p, terms = terms, adj = adj, z = evaluation_z(a), mde = mde,
              n_eff = n_eff, groups = groups, n_per_group = n_group, total_sample = sum(groups$Sample_Size),
              total_interviews = sum(groups$Interviews), power_achieved = evaluation_power(a, terms, n_eff))
  res$steps <- evaluation_steps(res)
  res
}

#' Power curve for the current scenario: power against n per group (sample
#' size mode) or against the detectable difference (MDE mode)
evaluation_power_curve <- function(res, points = 60) {
  a <- res$args
  adj <- res$adj
  if (res$mode == "sample_size") {
    step <- if (adj$clustered) adj$m else 1
    upper <- max(res$n_per_group * 2.5, 20 * step)
    n <- unique(pmax(step, round(seq(step, upper, length.out = points) / step) * step))
    data.frame(x = n, power = evaluation_power(a, res$terms, n * adj$retention / adj$deff))
  } else {
    sgn <- if (identical(a$direction, "decrease")) -1 else 1
    start <- if (a$design %in% c("did_cross", "did_panel")) a$p1 + (if (is.null(a$control_change)) 0 else a$control_change) else a$p1
    hi <- (if (sgn > 0) 1 - start else start) - 1e-6
    d <- seq(res$mde * 0.2, min(res$mde * 2.5, hi), length.out = points)
    pw <- vapply(d, function(x) evaluation_power(a, evaluation_terms(a, evaluation_proportions(a, x)), res$n_eff), numeric(1))
    data.frame(x = d, power = pw)
  }
}

evaluation_steps <- function(res) {
  a <- res$args
  adj <- res$adj
  steps <- data.frame(Parameter = character(0), Value = character(0), Formula = character(0), stringsAsFactors = FALSE)
  add <- function(parameter, value, formula = "Given") {
    steps[nrow(steps) + 1, ] <<- list(parameter, as.character(value), formula)
  }
  fmt_n <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",")
  pct <- function(x) paste0(round(x * 100, 2), "%")
  sided <- if (is.null(a$sided)) 2 else a$sided
  p <- res$p
  did <- a$design %in% c("did_cross", "did_panel")

  add("Design", names(EVALUATION_DESIGNS)[EVALUATION_DESIGNS == a$design])
  add("Significance Level (α)", a$alpha, if (sided == 2) "Two-sided test" else "One-sided test")
  add("Power (1 - β)", pct(a$power))
  add("z for α", round(res$z[["alpha"]], 4), if (sided == 2) "qnorm(1 - α/2)" else "qnorm(1 - α)")
  add("z for power", round(res$z[["beta"]], 4), "qnorm(power)")
  if (did) {
    add("Treatment: baseline → endline", paste0(round(p[["t1"]], 4), " → ", round(p[["t2"]], 4)),
        "Baseline + control change ± effect")
    add("Control: baseline → endline", paste0(round(p[["c1"]], 4), " → ", round(p[["c2"]], 4)), "Baseline + control change")
  } else {
    add("Proportion 1 (p₁)", round(p[["p1"]], 4))
    add("Proportion 2 (p₂)", round(p[["p2"]], 4), if (res$mode == "mde") "p₁ ± MDE" else "Given, or p₁ ± D")
  }
  effect_formula <- if (did) "(p_T2 - p_T1) - (p_C2 - p_C1)" else "p₂ - p₁"
  if (res$mode == "sample_size") {
    add("Detectable Difference (D)", round(res$terms$effect, 4), effect_formula)
  }
  if (a$design %in% c("panel", "did_panel")) add("Baseline-Endline Correlation (ρ)", if (is.null(a$rho)) 0 else a$rho)
  if (a$design != "panel") add("Allocation Ratio (k)", if (is.null(a$ratio)) 1 else a$ratio,
                               if (did) "Control : treatment" else "Group 2 : group 1")
  v_formula <- switch(a$design,
    independent = "p₁(1-p₁) + p₂(1-p₂)/k",
    panel = "p₁(1-p₁) + p₂(1-p₂) - 2ρ√(p₁(1-p₁)p₂(1-p₂))",
    did_cross = "Σ p(1-p) over treatment cells + Σ p(1-p) over control cells / k",
    did_panel = "Per arm: p₁q₁ + p₂q₂ - 2ρ√(p₁q₁p₂q₂); treatment + control / k"
  )
  add("Variance Term (V)", round(res$terms$v1, 5), v_formula)
  if (isTRUE(a$pooled) && a$design == "independent") {
    add("Variance Under No Difference (V₀)", round(res$terms$v0, 5), "p̄(1-p̄)(1 + 1/k), p̄ = (p₁ + k·p₂)/(1 + k)")
  }
  base_formula <- if (isTRUE(a$pooled) && a$design == "independent") "(z_α√V₀ + z_β√V)² / D²" else "(z_α + z_β)² × V / D²"
  add("Response Rate", pct(a$response_rate))
  if (adj$panel) add("Attrition Between Rounds", pct(adj$attrition))
  if (adj$clustered) {
    add("Interviews per Cluster (m)", adj$m)
    add("Intraclass Correlation (ICC)", adj$icc)
    add("Design Effect (DEFF)", round(adj$deff, 3), "1 + (m-1) × ICC")
  }
  first <- res$groups$Group[1]

  if (res$mode == "sample_size") {
    add("Completed Interviews per Group (no clustering)", fmt_n(res$n_base), base_formula)
    add(paste0("Adjusted for Non-Response", if (adj$panel) " and Attrition" else ""),
        res$groups$After_Non_Response[1],
        paste0("n / response rate", if (adj$panel) " / (1 - attrition)" else "", ", rounded up"))
    if (adj$clustered) {
      add("Adjusted n × DEFF", fmt_n(res$n_base / adj$retention * adj$deff), "Adjusted n × DEFF")
      add("Clusters per Group", res$groups$Clusters[1], "ceiling(adjusted n × DEFF / m)")
      add("Final n per Group", res$n_per_group, "Clusters × m")
    } else {
      add("Final n per Group", res$n_per_group, "Adjusted n")
    }
    add("Total Sample", res$total_sample, "Sum over groups (see the group table)")
    if (adj$panel) add("Total Interviews (both rounds)", res$total_interviews, "Panel households × 2 rounds")
    add("Power at Final n", pct(res$power_achieved), "Φ((|D|√n_eff - z_α√V₀) / √V)")
  } else {
    add(paste0("Sample per Group (", first, ")"), res$n_per_group, if (adj$clustered) "Clusters × m" else "Given")
    add("Effective Completed Interviews", fmt_n(res$n_eff),
        paste0("n × response rate", if (adj$panel) " × (1 - attrition)" else "", if (adj$clustered) " / DEFF" else ""))
    add("Minimum Detectable Difference (MDE)", paste0(round(res$mde, 4), " (", round(res$mde * 100, 1), " points)"),
        paste0("Solves ", base_formula, " = effective n for D"))
    add("Power at MDE", pct(res$power_achieved), "Equals the target power")
  }
  steps
}
