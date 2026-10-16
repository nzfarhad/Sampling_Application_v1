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
           "±" = "+/-", "p̄" = "p-bar", "m̄" = "m-bar", "·" = "*", "–" = "-", "≥" = ">=", "≤" = "<=")
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
# Evaluation: sample size and minimum detectable difference when comparing
# groups or survey rounds. Outcomes are proportions or means; designs are two
# independent groups, panel, difference-in-differences and ANCOVA. Optional:
# several indicators, subgroups, design effect entry methods and TPM field
# adjustments (inaccessible clusters, few clusters, small populations,
# matched comparisons).
# ---------------------------------------------------------------------------

EVALUATION_DESIGNS <- c(
  "Two independent groups (treatment vs control, or baseline vs endline cross-sections)" = "independent",
  "Panel: same households at baseline and endline" = "panel",
  "Difference-in-differences, repeated cross-sections" = "did_cross",
  "Difference-in-differences, panel" = "did_panel",
  "Panel, endline comparison controlling for baseline (ANCOVA)" = "ancova"
)

# Designs with a treatment and a control arm, each measured at baseline and endline
EVALUATION_ARM_DESIGNS <- c("did_cross", "did_panel", "ancova")
# Designs that interview the same households in two rounds
EVALUATION_PANEL_DESIGNS <- c("panel", "did_panel", "ancova")

#' Value of an optional scenario field, or the default when it is missing or NA
eval_opt <- function(x, default) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) default else x
}

eval_is_mean <- function(a) identical(a$outcome, "mean")

#' Size of the change to detect, in outcome units (positive)
#'
#' a$effect_scale: "absolute" (a$effect, e.g. 0.10 = 10 points, or units of a
#' mean), "relative" (a$effect_relative, a share of the baseline a$p1, e.g.
#' 0.2 for 20%) or "d" (a$effect_d standard deviations; means only)
evaluation_effect <- function(a) {
  d <- switch(eval_opt(a$effect_scale, "absolute"),
    relative = eval_opt(a$effect_relative, NA) * abs(a$p1),
    d = eval_opt(a$effect_d, NA) * eval_opt(a$sd, NA),
    eval_opt(a$effect, NA)
  )
  abs(d)
}

#' Expected values (proportions, or means when a$outcome is "mean")
#'
#' Independent and panel designs: value 1 (baseline or comparison group) and
#' value 2, given directly (a$p2) or as value 1 +/- the effect. Arm designs:
#' treatment and control at baseline and endline; both arms move by
#' control_change, and the treatment arm moves by the effect on top of that.
evaluation_proportions <- function(a, effect = NULL) {
  sgn <- if (identical(a$direction, "decrease")) -1 else 1
  two_values <- a$design %in% c("independent", "panel")
  given_p2 <- is.null(effect) && two_values && !is.null(a$p2) && !is.na(a$p2)
  if (is.null(effect) && !given_p2) effect <- evaluation_effect(a)
  if (two_values) {
    p <- c(p1 = a$p1, p2 = if (given_p2) a$p2 else a$p1 + sgn * effect)
  } else {
    pc <- eval_opt(a$p_control, a$p1)
    change <- eval_opt(a$control_change, 0)
    p <- c(t1 = a$p1, t2 = a$p1 + change + sgn * effect, c1 = pc, c2 = pc + change)
  }
  if (eval_is_mean(a)) {
    if (any(is.na(p) | !is.finite(p))) stop("Enter the baseline mean and the change to detect")
  } else if (any(is.na(p) | p <= 0 | p >= 1)) {
    stop("Expected proportions must be between 0 and 1 (", paste(names(p), round(p, 3), sep = " = ", collapse = ", "), ")")
  }
  p
}

#' Variance of one unit in each group and round: p(1-p), or sd^2 for a mean
evaluation_cell_variance <- function(a, p) {
  if (eval_is_mean(a)) setNames(rep(a$sd^2, length(p)), names(p)) else p * (1 - p)
}

#' Effect and variance terms: n per group = (z_a sqrt(v0) + z_b sqrt(v1))^2 / effect^2
evaluation_terms <- function(a, p) {
  s2 <- evaluation_cell_variance(a, p)
  k <- eval_opt(a$ratio, 1)
  rho <- eval_opt(a$rho, 0)
  change_var <- function(x, y, r) s2[[x]] + s2[[y]] - 2 * r * sqrt(s2[[x]] * s2[[y]])
  if (a$design == "independent") {
    effect <- p[["p2"]] - p[["p1"]]
    v1 <- s2[["p1"]] + s2[["p2"]] / k
    v0 <- v1
    if (isTRUE(a$pooled) && !eval_is_mean(a)) {
      pbar <- (p[["p1"]] + k * p[["p2"]]) / (1 + k)
      v0 <- pbar * (1 - pbar) * (1 + 1 / k)
    }
  } else if (a$design == "panel") {
    effect <- p[["p2"]] - p[["p1"]]
    v1 <- change_var("p1", "p2", rho)
    v0 <- v1
  } else {
    effect <- (p[["t2"]] - p[["t1"]]) - (p[["c2"]] - p[["c1"]])
    v1 <- if (a$design == "ancova") {
      # Endline comparison adjusted for baseline: variance factor 1 - rho^2 (McKenzie 2012)
      (s2[["t2"]] + s2[["c2"]] / k) * (1 - rho^2)
    } else {
      r <- if (a$design == "did_panel") rho else 0
      change_var("t1", "t2", r) + change_var("c1", "c2", r) / k
    }
    v0 <- v1
  }
  list(effect = effect, v0 = v0, v1 = v1)
}

#' Critical values for the test and the power: normal, or t with df degrees of
#' freedom when there are few clusters
evaluation_z <- function(a, df = NULL) {
  sided <- eval_opt(a$sided, 2)
  if (is.null(df)) {
    c(alpha = qnorm(1 - a$alpha / sided), beta = qnorm(a$power))
  } else {
    c(alpha = qt(1 - a$alpha / sided, df), beta = qt(a$power, df))
  }
}

#' Completed interviews per group (group 1 / treatment cell), before any adjustment
evaluation_base_n <- function(a, terms, df = NULL) {
  z <- evaluation_z(a, df)
  (z[["alpha"]] * sqrt(terms$v0) + z[["beta"]] * sqrt(terms$v1))^2 / terms$effect^2
}

#' Power for a given number of effective completed interviews in group 1
evaluation_power <- function(a, terms, n_eff, df = NULL) {
  z <- evaluation_z(a, df)
  x <- (abs(terms$effect) * sqrt(n_eff) - z[["alpha"]] * sqrt(terms$v0)) / sqrt(terms$v1)
  if (is.null(df)) pnorm(x) else pt(x, df)
}

#' Share of sampled units that end up as usable interviews, the design effect
#' and the field adjustments
evaluation_adjustments <- function(a) {
  panel <- a$design %in% EVALUATION_PANEL_DESIGNS
  attrition <- if (panel) eval_opt(a$attrition, 0) else 0
  m <- eval_opt(a$m, 1)
  clustered <- isTRUE(a$clustered) && m > 1
  method <- if (clustered) eval_opt(a$deff_method, "icc") else "none"
  icc <- if (clustered) eval_opt(a$icc, 0) else 0
  cv <- if (method == "cv") eval_opt(a$cv_cluster, 0) else 0
  deff_cluster <- switch(method,
    none = 1,
    icc = 1 + (m - 1) * icc,
    cv = 1 + ((cv^2 + 1) * m - 1) * icc,
    known = eval_opt(a$deff_known, 1)
  )
  cv_w <- if (isTRUE(a$weighting)) eval_opt(a$cv_weights, 0) else 0
  deff_weight <- 1 + cv_w^2
  population <- eval_opt(a$population, Inf)
  list(
    panel = panel,
    attrition = attrition,
    retention = a$response_rate * (1 - attrition),
    clustered = clustered,
    m = if (clustered) m else 1,
    icc = icc,
    deff_method = method,
    cv_cluster = cv,
    deff_cluster = deff_cluster,
    cv_weights = cv_w,
    deff_weight = deff_weight,
    deff = deff_cluster * deff_weight,
    inaccessible = if (clustered) eval_opt(a$inaccessible, 0) else 0,
    population = population,
    fpc = is.finite(population),
    matching = eval_opt(a$matching_factor, 1),
    use_t = !isFALSE(a$small_cluster_t)
  )
}

#' Groups that are sampled under each design, with their size relative to group 1
evaluation_groups <- function(a) {
  k <- eval_opt(a$ratio, 1)
  switch(a$design,
    independent = data.frame(Group = c("Group 1 (comparison / baseline)", "Group 2 (treatment / endline)"),
                             multiplier = c(1, k), rounds = 1),
    panel = data.frame(Group = "Panel households (interviewed at baseline and endline)", multiplier = 1, rounds = 2),
    did_cross = data.frame(Group = c("Treatment - baseline", "Treatment - endline", "Control - baseline", "Control - endline"),
                           multiplier = c(1, 1, k, k), rounds = 1),
    did_panel = ,
    ancova = data.frame(Group = c("Treatment panel households", "Control panel households"),
                        multiplier = c(1, k), rounds = 2)
  )
}

#' Finite population correction of completed interviews, and its inverse
eval_fpc <- function(x, N) if (is.finite(N)) x / (1 + (x - 1) / N) else x
eval_fpc_inverse <- function(x, N) {
  if (!is.finite(N)) return(x)
  ifelse(x >= N, Inf, x * (N - 1) / (N - x))
}

#' Effective completed interviews (no design effect, no FPC) from a planned
#' sample of group 1 units: the inverse of evaluation_size_groups()
evaluation_effective_n <- function(adj, sample) {
  completed <- sample * (1 - adj$inaccessible) * adj$retention
  eval_fpc_inverse(completed, adj$population) / adj$deff / adj$matching
}

#' Sample needed in each group for n_base completed interviews in group 1:
#' matching and subgroup factors, then DEFF, FPC, non-response and attrition,
#' whole clusters and inaccessible clusters
evaluation_size_groups <- function(a, n_base, adj, factor = 1) {
  groups <- evaluation_groups(a)
  need <- n_base * groups$multiplier * adj$matching * factor
  groups$Completed_Needed <- ceiling(need - 1e-9)
  groups$After_Non_Response <- ceiling(need / adj$retention - 1e-9)
  sample <- eval_fpc(need * adj$deff, adj$population) / adj$retention
  if (adj$clustered) {
    groups$Clusters_Needed <- ceiling(sample / adj$m - 1e-9)
    groups$Clusters <- ceiling(groups$Clusters_Needed / (1 - adj$inaccessible) - 1e-9)
    groups$Sample_Size <- groups$Clusters * adj$m
  } else {
    groups$Sample_Size <- ceiling(sample - 1e-9)
  }
  groups$Interviews <- groups$Sample_Size * groups$rounds
  groups
}

#' Degrees of freedom for few clusters: clusters reached in all groups minus
#' the number of groups. NULL (normal values) unless group 1 has fewer than 20
evaluation_df <- function(adj, clusters) {
  if (!adj$clustered || !adj$use_t || clusters[1] >= 20) return(NULL)
  max(1, sum(clusters) - length(clusters))
}

evaluation_check <- function(a) {
  if (is.null(a$alpha) || is.na(a$alpha) || a$alpha <= 0 || a$alpha >= 0.5) stop("Significance level must be between 0 and 0.5")
  if (is.null(a$power) || is.na(a$power) || a$power <= 0.5 || a$power >= 1) stop("Power must be between 0.5 and 1")
  if (is.null(a$response_rate) || is.na(a$response_rate) || a$response_rate <= 0 || a$response_rate > 1) {
    stop("Response rate must be above 0% and at most 100%")
  }
  if (!is.na(eval_opt(a$attrition, NA)) && (a$attrition < 0 || a$attrition >= 1)) stop("Attrition must be from 0 to 99%")
  if (!is.na(eval_opt(a$rho, NA)) && (a$rho < 0 || a$rho >= 1)) stop("Correlation must be from 0 to 0.99")
  if (!is.na(eval_opt(a$ratio, NA)) && a$ratio <= 0) stop("Allocation ratio must be above 0")
  if (isTRUE(a$clustered)) {
    method <- eval_opt(a$deff_method, "icc")
    if (is.na(eval_opt(a$m, NA)) || a$m < 1) stop("Enter interviews per cluster (1 or more)")
    if (method == "known") {
      if (is.na(eval_opt(a$deff_known, NA)) || a$deff_known < 1) stop("Enter a design effect of 1 or more")
    } else if (is.na(eval_opt(a$icc, NA)) || a$icc < 0 || a$icc > 1) {
      stop("Enter interviews per cluster (1 or more) and an ICC from 0 to 1")
    }
    if (method == "cv" && (is.na(eval_opt(a$cv_cluster, NA)) || a$cv_cluster < 0)) stop("Enter a CV of cluster sizes of 0 or more")
    if (!is.na(eval_opt(a$inaccessible, NA)) && (a$inaccessible < 0 || a$inaccessible > 0.9)) {
      stop("Inaccessible clusters must be from 0 to 90%")
    }
  }
  if (isTRUE(a$weighting) && (is.na(eval_opt(a$cv_weights, NA)) || a$cv_weights < 0)) stop("Enter a CV of the weights of 0 or more")
  if (!is.na(eval_opt(a$population, NA)) && a$population < 2) stop("Population per group must be 2 or more")
  if (!is.na(eval_opt(a$matching_factor, NA)) && a$matching_factor < 1) stop("The matching factor must be 1 or more")
  sg <- a$subgroups
  if (!is.null(sg) && nrow(sg) > 0) {
    share <- suppressWarnings(as.numeric(sg$Share))
    fixed <- if ("Fixed" %in% names(sg)) suppressWarnings(as.numeric(sg$Fixed)) else rep(NA_real_, nrow(sg))
    bad <- is.na(fixed) & (is.na(share) | share <= 0 | share > 1)
    if (any(bad)) stop("Subgroup ", sg$Subgroup[bad][1], ": enter a share from 1 to 100% or a fixed sample")
    if (any(!is.na(fixed) & fixed < 2)) stop("A fixed subgroup sample must be 2 or more")
  }
}

#' Checks that depend on the indicator (each row of the indicators table)
evaluation_check_indicator <- function(a) {
  if (is.na(eval_opt(a$p1, NA))) stop("Enter the baseline value")
  if (eval_is_mean(a) && (is.na(eval_opt(a$sd, NA)) || a$sd <= 0)) stop("Enter a standard deviation above 0")
  if (identical(a$effect_scale, "d") && !eval_is_mean(a)) stop("Cohen's d applies to means only; give a proportion's change in points")
  if (identical(a$effect_scale, "relative") && a$p1 == 0) stop("A relative change needs a baseline other than 0")
  if (identical(a$mode, "sample_size") && is.na(eval_opt(a$p2, NA)) && is.na(evaluation_effect(a))) stop("Enter the change to detect")
}

#' Scenario for one row of the indicators table
#'
#' Columns: Indicator, Type ("proportion" or "mean"), Baseline, SD (means),
#' Change (signed; read on the scenario's effect scale: points or units,
#' percent of the baseline, or d), ICC (optional, replaces the common ICC),
#' Control_Baseline and Control_Trend (arm designs; blank means the same as
#' the baseline, and no trend)
evaluation_indicator_args <- function(a, row, i = 1) {
  num <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
  type <- tolower(trimws(as.character(eval_opt(row$Type, "proportion"))))
  b <- a
  b$indicators <- NULL
  b$indicator_name <- if (is.null(row$Indicator) || is.na(row$Indicator) || !nzchar(trimws(row$Indicator))) {
    paste("Indicator", i)
  } else trimws(as.character(row$Indicator))
  b$outcome <- if (startsWith(type, "m")) "mean" else "proportion"
  b$p1 <- num(row$Baseline)
  b$sd <- num(row$SD)
  b$p2 <- NA
  scale <- eval_opt(a$effect_scale, "absolute")
  if (scale == "value2") scale <- "absolute"
  b$effect_scale <- scale
  change <- num(row$Change)
  if (!is.na(change)) {
    b$direction <- if (change < 0) "decrease" else "increase"
    if (scale == "relative") b$effect_relative <- abs(change) / 100
    else if (scale == "d") b$effect_d <- abs(change)
    else b$effect <- abs(change)
  } else if (identical(a$mode, "sample_size")) {
    b$effect <- NA
    b$effect_relative <- NA
    b$effect_d <- NA
  }
  b$p_control <- num(row$Control_Baseline)
  b$control_change <- num(row$Control_Trend)
  icc <- num(row$ICC)
  if (!is.na(icc)) b$icc <- icc
  b
}

#' One scenario per key indicator (the scenario itself when there is no table)
evaluation_indicator_list <- function(a) {
  tab <- a$indicators
  if (is.null(tab) || nrow(tab) == 0) {
    a$indicator_name <- eval_opt(a$indicator_name, "")
    return(list(a))
  }
  lapply(seq_len(nrow(tab)), function(i) evaluation_indicator_args(a, tab[i, , drop = FALSE], i))
}

#' Standardised effect: Cohen's d for a mean, Cohen's h (arcsine) for a proportion
evaluation_standardised <- function(a, effect) {
  if (eval_is_mean(a)) return(list(value = abs(effect) / a$sd, label = "Standardised Effect (Cohen's d)", formula = "|D| / σ"))
  sgn <- if (identical(a$direction, "decrease")) -1 else 1
  p2 <- min(max(a$p1 + sgn * abs(effect), 1e-9), 1 - 1e-9)
  list(value = abs(2 * asin(sqrt(p2)) - 2 * asin(sqrt(a$p1))), label = "Standardised Effect (Cohen's h)",
       formula = "|2·asin√p₂ - 2·asin√p₁|, p₂ = baseline ± D")
}

#' Sample size for one indicator
evaluation_size_single <- function(a, factor = 1) {
  p <- evaluation_proportions(a)
  terms <- evaluation_terms(a, p)
  if (is.na(terms$effect) || abs(terms$effect) < 1e-12) stop("The detectable difference must not be 0")
  adj <- evaluation_adjustments(a)
  solve <- function(df) {
    n_base <- evaluation_base_n(a, terms, df)
    list(df = df, n_base = n_base, groups = evaluation_size_groups(a, n_base, adj, factor))
  }
  s <- solve(NULL)
  if (adj$clustered) {
    # Fewer than 20 clusters per group: t-values, iterated because df depends on the clusters
    df <- evaluation_df(adj, s$groups$Clusters_Needed)
    seen <- numeric(0)
    while (!is.null(df)) {
      s <- solve(df)
      df_new <- max(1, sum(s$groups$Clusters_Needed) - nrow(s$groups))
      if (df_new == df) break
      if (df_new %in% seen) {
        s <- solve(min(df, df_new))   # cycle: keep the larger sample
        break
      }
      seen <- c(seen, df)
      df <- df_new
    }
  }
  groups <- s$groups
  n_eff <- evaluation_effective_n(adj, min(groups$Sample_Size / groups$multiplier)) / factor

  res <- list(mode = "sample_size", args = a, p = p, terms = terms, adj = adj, z = evaluation_z(a, s$df), df = s$df,
              n_base = s$n_base, factor = factor, groups = groups, n_per_group = groups$Sample_Size[1],
              total_sample = sum(groups$Sample_Size), total_interviews = sum(groups$Interviews),
              n_eff = n_eff, power_achieved = evaluation_power(a, terms, n_eff, s$df))
  if (a$design == "independent" && !eval_is_mean(a)) {
    # Arcsine (Cohen's h) sample size, shown next to the normal approximation
    h <- abs(2 * asin(sqrt(p[["p2"]])) - 2 * asin(sqrt(p[["p1"]])))
    res$n_arcsine <- (sum(evaluation_z(a, s$df)))^2 * (1 + 1 / eval_opt(a$ratio, 1)) / h^2
  }
  res
}

#' Minimum detectable difference for one indicator and a fixed sample
#' (a$clusters_per_group with a$m when clustered, otherwise a$n_per_group,
#' both for group 1 / the treatment arm)
evaluation_mde_single <- function(a, factor = 1) {
  adj <- evaluation_adjustments(a)
  n_group <- if (adj$clustered) eval_opt(a$clusters_per_group, NA) * adj$m else eval_opt(a$n_per_group, NA)
  if (is.na(n_group) || n_group < 2) stop("Enter the sample per group (at least 2 interviews)")
  n_eff <- evaluation_effective_n(adj, n_group) / factor
  if (!is.finite(n_eff)) stop("The sample covers the whole population: there is no sampling error to plan for")
  groups <- evaluation_groups(a)
  df <- if (adj$clustered) evaluation_df(adj, a$clusters_per_group * groups$multiplier * (1 - adj$inaccessible)) else NULL
  z <- evaluation_z(a, df)

  if (eval_is_mean(a)) {
    t0 <- evaluation_terms(a, evaluation_proportions(a, 1))
    mde <- (z[["alpha"]] * sqrt(t0$v0) + z[["beta"]] * sqrt(t0$v1)) / sqrt(n_eff)
  } else {
    sgn <- if (identical(a$direction, "decrease")) -1 else 1
    start <- if (a$design %in% EVALUATION_ARM_DESIGNS) a$p1 + eval_opt(a$control_change, 0) else a$p1
    hi <- (if (sgn > 0) 1 - start else start) - 1e-6
    if (hi <= 1e-6) stop("The baseline proportion leaves no room for a change in that direction")
    gap <- function(d) evaluation_base_n(a, evaluation_terms(a, evaluation_proportions(a, d)), df) - n_eff
    if (gap(hi) > 0) {
      stop("This sample cannot detect any change in that direction with the requested power. Increase the sample.")
    }
    mde <- uniroot(gap, c(1e-6, hi), tol = 1e-10)$root
  }
  p <- evaluation_proportions(a, mde)
  terms <- evaluation_terms(a, p)

  groups$Sample_Size <- n_group * groups$multiplier
  if (adj$clustered) groups$Clusters <- a$clusters_per_group * groups$multiplier
  groups$Expected_Completed <- floor(groups$Sample_Size * (1 - adj$inaccessible) * adj$retention + 1e-9)
  groups$Interviews <- groups$Sample_Size * groups$rounds

  list(mode = "mde", args = a, p = p, terms = terms, adj = adj, z = z, df = df, mde = mde, n_eff = n_eff,
       factor = factor, groups = groups, n_per_group = n_group, total_sample = sum(groups$Sample_Size),
       total_interviews = sum(groups$Interviews), power_achieved = evaluation_power(a, terms, n_eff, df))
}

#' Subgroup factor: with the goal "within", every subgroup given by its share
#' must reach the full sample, so the total grows by 1 / the smallest share
evaluation_subgroup_factor <- function(a) {
  sg <- a$subgroups
  if (is.null(sg) || nrow(sg) == 0 || !identical(a$subgroup_goal, "within")) return(1)
  fixed <- if ("Fixed" %in% names(sg)) suppressWarnings(as.numeric(sg$Fixed)) else rep(NA_real_, nrow(sg))
  share <- suppressWarnings(as.numeric(sg$Share))[is.na(fixed)]
  if (length(share) == 0) 1 else max(1, 1 / min(share))
}

#' Sample and minimum detectable difference in each subgroup
#'
#' The subgroup's sample is its share of group 1's sample, or its fixed sample.
#' Clustered: the subgroup is spread over the same clusters, so it has fewer
#' interviews per cluster and a smaller design effect.
evaluation_subgroup_table <- function(res) {
  sg <- res$args$subgroups
  if (is.null(sg) || nrow(sg) == 0) return(NULL)
  a <- res$args
  adj <- res$adj
  fixed <- if ("Fixed" %in% names(sg)) suppressWarnings(as.numeric(sg$Fixed)) else rep(NA_real_, nrow(sg))
  share <- suppressWarnings(as.numeric(sg$Share))
  n_sub <- ifelse(is.na(fixed), share * res$n_per_group, fixed)
  clusters <- if (adj$clustered) res$n_per_group / adj$m else NA
  mde <- vapply(seq_len(nrow(sg)), function(i) {
    b <- a
    b$subgroups <- NULL
    m_sub <- if (adj$clustered) n_sub[i] / clusters else 1
    if (adj$clustered && m_sub > 1) {
      b$m <- m_sub
      b$clusters_per_group <- clusters
    } else {
      b$clustered <- FALSE
      b$n_per_group <- n_sub[i]
    }
    out <- tryCatch(evaluation_mde_single(b), error = function(e) NULL)
    if (is.null(out)) NA_real_ else out$mde
  }, numeric(1))
  target <- if (res$mode == "sample_size") abs(res$terms$effect) else NA
  out <- data.frame(
    Subgroup = as.character(sg$Subgroup),
    Share_pct = ifelse(is.na(fixed), round(share * 100, 1), NA),
    Sample_in_Subgroup = round(n_sub),
    MDE = mde,
    stringsAsFactors = FALSE
  )
  if (res$mode == "sample_size") {
    out$Detects_Target <- ifelse(is.na(mde), "No", ifelse(mde <= target + 1e-9, "Yes", "No"))
  }
  out
}

#' DiD panel with rho < 0.5: an ANCOVA or an endline-only comparison needs a
#' smaller sample (McKenzie 2012). Returns both, side by side, or NULL.
evaluation_alternatives <- function(a, res) {
  rho <- eval_opt(a$rho, 0)
  if (a$design != "did_panel" || rho >= 0.5) return(NULL)
  runner <- if (res$mode == "mde") evaluation_mde else evaluation_sample_size
  ancova <- tryCatch(runner(modifyList(a, list(design = "ancova"))), error = function(e) NULL)
  endline <- tryCatch(runner(modifyList(a, list(design = "ancova", rho = 0, attrition = 0))), error = function(e) NULL)
  pick <- function(r) if (is.null(r)) NA_real_ else if (res$mode == "mde") r$mde else r$n_per_group
  out <- data.frame(
    Analysis = c("Difference-in-differences, panel (selected)", "ANCOVA: endline controlling for baseline",
                 "Endline comparison only (no baseline)"),
    Variance_Factor = c(round(2 * (1 - rho), 3), round(1 - rho^2, 3), 1),
    Value = c(pick(res), pick(ancova), pick(endline)),
    stringsAsFactors = FALSE
  )
  names(out)[3] <- if (res$mode == "mde") "MDE" else "Sample_per_Arm"
  out
}

#' Run every indicator, pick the one that needs the largest sample, and add
#' the indicator, subgroup and alternatives tables, warnings and steps
evaluation_run <- function(a, mode) {
  a$mode <- mode
  evaluation_check(a)
  scenarios <- evaluation_indicator_list(a)
  K <- length(scenarios)
  bonferroni <- isTRUE(a$bonferroni) && K > 1
  if (bonferroni) scenarios <- lapply(scenarios, function(b) { b$alpha <- b$alpha / K; b })
  factor <- if (mode == "sample_size") evaluation_subgroup_factor(a) else 1
  runs <- lapply(scenarios, function(b) {
    tryCatch({
      evaluation_check_indicator(b)
      if (mode == "mde") evaluation_mde_single(b) else evaluation_size_single(b, factor)
    }, error = function(e) stop(if (K > 1) paste0(b$indicator_name, ": ") else "", conditionMessage(e), call. = FALSE))
  })
  pick <- if (mode == "sample_size") which.max(vapply(runs, function(r) r$n_per_group, numeric(1))) else 1
  res <- runs[[pick]]
  res$indicator_name <- res$args$indicator_name
  res$alpha_nominal <- a$alpha
  res$n_tests <- K
  res$bonferroni <- bonferroni
  res$driver <- pick
  if (K > 1) res$indicators <- evaluation_indicator_table(runs, pick)
  res$subgroups <- evaluation_subgroup_table(res)
  res$alternatives <- evaluation_alternatives(a, res)
  res$warnings <- evaluation_warnings(res, runs)
  res$steps <- evaluation_steps(res)
  res
}

#' Sample size for detecting a difference (proportion or mean)
#'
#' @param a Scenario list: design (see EVALUATION_DESIGNS), outcome
#'   ("proportion" or "mean"), p1 (baseline proportion or mean), sd (means),
#'   p2 (optional), effect / effect_relative / effect_d with effect_scale,
#'   direction ("increase"/"decrease"), p_control, control_change (arm
#'   designs), alpha, power, sided (1 or 2), pooled (independent proportions),
#'   ratio (group 2 or control : group 1 or treatment), rho (panel designs),
#'   response_rate (0-1), attrition (panel, 0-1), clustered, m, deff_method
#'   ("icc", "cv" or "known"), icc, cv_cluster, deff_known, weighting,
#'   cv_weights, inaccessible (0-1), population (per group), matching_factor,
#'   small_cluster_t (default TRUE), indicators (data frame, see
#'   evaluation_indicator_args), bonferroni, subgroups (data frame: Subgroup,
#'   Share 0-1, Fixed) and subgroup_goal ("within" or "overall")
#'
#' @return List with values, terms, adjustments, groups table, total sample
#'   and interviews, power achieved, optional indicator, subgroup and
#'   alternatives tables, warnings and a step-by-step table
evaluation_sample_size <- function(a) evaluation_run(a, "sample_size")

#' Minimum detectable difference for a fixed sample
#'
#' @param a Scenario list as for evaluation_sample_size; the sample is
#'   a$clusters_per_group (with a$m) when clustered, otherwise a$n_per_group,
#'   both for group 1 / the treatment arm
evaluation_mde <- function(a) evaluation_run(a, "mde")

#' One row per indicator: its assumptions and result
evaluation_indicator_table <- function(runs, pick) {
  rows <- lapply(seq_along(runs), function(i) {
    r <- runs[[i]]
    a <- r$args
    std <- evaluation_standardised(a, r$terms$effect)
    row <- data.frame(
      Indicator = a$indicator_name,
      Type = if (eval_is_mean(a)) "Mean" else "Proportion",
      Baseline = a$p1,
      SD = if (eval_is_mean(a)) a$sd else NA,
      ICC = if (r$adj$clustered && r$adj$deff_method != "known") r$adj$icc else NA,
      DEFF = round(r$adj$deff, 3),
      stringsAsFactors = FALSE
    )
    if (r$mode == "sample_size") {
      row$Change <- signif(r$terms$effect, 4)
      row$Sample_per_Group <- r$n_per_group
      if (r$adj$clustered) row$Clusters_per_Group <- r$groups$Clusters[1]
      row$Sets_the_Sample <- if (i == pick) "Yes" else ""
    } else {
      row$MDE <- signif(r$mde, 4)
      row$MDE_pct_of_Baseline <- if (a$p1 != 0) round(r$mde / abs(a$p1) * 100, 1) else NA
      row$Standardised <- round(std$value, 3)
    }
    row
  })
  do.call(rbind, rows)
}

evaluation_warnings <- function(res, runs) {
  out <- character(0)
  rare <- vapply(runs, function(r) !eval_is_mean(r$args) && any(r$p < 0.10 | r$p > 0.90), logical(1))
  if (any(rare)) {
    names_rare <- vapply(runs[rare], function(r) r$args$indicator_name, character(1))
    out <- c(out, paste0(
      "A proportion is below 0.10 or above 0.90", if (any(nzchar(names_rare))) paste0(" (", paste(names_rare[nzchar(names_rare)], collapse = ", "), ")") else "",
      ": the normal approximation is weak for rare outcomes. Treat the result as approximate",
      if (res$args$design == "independent" && !is.null(res$n_arcsine)) "; the arcsine (Cohen's h) sample size is shown in the calculation for comparison" else "",
      ", and consider a larger sample."))
  }
  if (!is.null(res$df)) {
    out <- c(out, paste0("Fewer than 20 clusters per group: t-values with ", round(res$df, 1),
                         " degrees of freedom are used in place of z values."))
  }
  if (res$adj$fpc && any(res$groups$Sample_Size > res$adj$population / 2)) {
    out <- c(out, "The sample is more than half of a group's population: a census of that group may be simpler.")
  }
  if (res$mode == "sample_size" && res$factor > 3) {
    out <- c(out, paste0("The smallest subgroup makes the total ", round(res$factor, 1),
                         " times the overall requirement. Consider oversampling that subgroup instead (enter a fixed sample for it, and weight the results).",
                         ""))
  }
  sg <- res$subgroups
  if (!is.null(sg) && res$mode == "sample_size" && identical(res$args$subgroup_goal, "within") && any(sg$Detects_Target == "No")) {
    out <- c(out, paste0("These subgroups cannot detect the target difference with their sample: ",
                         paste(sg$Subgroup[sg$Detects_Target == "No"], collapse = ", "), ". Increase their fixed sample."))
  }
  if (!is.null(res$alternatives)) {
    out <- c(out, paste0("With ρ below 0.5, a panel difference-in-differences needs more households than an ANCOVA or even an endline-only comparison. ",
                         "See the comparison table and consider analysing by ANCOVA."))
  }
  out
}

#' Power curve for the current scenario: power against n per group (sample
#' size mode) or against the detectable difference (MDE mode)
evaluation_power_curve <- function(res, points = 60) {
  a <- res$args
  adj <- res$adj
  factor <- eval_opt(res$factor, 1)
  if (res$mode == "sample_size") {
    step <- if (adj$clustered) adj$m else 1
    upper <- max(res$n_per_group * 2.5, 20 * step)
    n <- unique(pmax(step, round(seq(step, upper, length.out = points) / step) * step))
    data.frame(x = n, power = evaluation_power(a, res$terms, evaluation_effective_n(adj, n) / factor, res$df))
  } else {
    hi <- res$mde * 2.5
    if (!eval_is_mean(a)) {
      sgn <- if (identical(a$direction, "decrease")) -1 else 1
      start <- if (a$design %in% EVALUATION_ARM_DESIGNS) a$p1 + eval_opt(a$control_change, 0) else a$p1
      hi <- min(hi, (if (sgn > 0) 1 - start else start) - 1e-6)
    }
    d <- seq(res$mde * 0.2, hi, length.out = points)
    pw <- vapply(d, function(x) evaluation_power(a, evaluation_terms(a, evaluation_proportions(a, x)), res$n_eff, res$df), numeric(1))
    data.frame(x = d, power = pw)
  }
}

#' A difference in words: percentage points for a proportion, units for a mean
evaluation_format_effect <- function(a, x) {
  if (eval_is_mean(a)) paste0(format(signif(abs(x), 4)), " units") else paste0(round(abs(x) * 100, 1), " percentage points")
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
  sided <- eval_opt(a$sided, 2)
  p <- res$p
  is_mean <- eval_is_mean(a)
  arms <- a$design %in% EVALUATION_ARM_DESIGNS
  val <- function(x) if (is_mean) format(signif(x, 4)) else round(x, 4)
  scale <- eval_opt(a$effect_scale, "absolute")

  add("Design", names(EVALUATION_DESIGNS)[EVALUATION_DESIGNS == a$design])
  if (eval_opt(res$n_tests, 1) > 1) {
    add("Key Indicators", res$n_tests,
        if (res$mode == "sample_size") "Each sized separately; the largest sample is used" else "The MDE of each is in the indicator table")
  }
  if (nzchar(eval_opt(res$indicator_name, ""))) {
    add("Indicator", res$indicator_name,
        if (eval_opt(res$n_tests, 1) > 1 && res$mode == "sample_size") "Needs the largest sample" else
          if (eval_opt(res$n_tests, 1) > 1) "First indicator; the others are in the indicator table" else "Given")
  }
  add("Outcome", if (is_mean) "Mean (continuous)" else "Proportion")
  add("Significance Level (α)", eval_opt(res$alpha_nominal, a$alpha), if (sided == 2) "Two-sided test" else "One-sided test")
  if (isTRUE(res$bonferroni)) add("Bonferroni-Adjusted α", signif(a$alpha, 4), paste0("α / ", res$n_tests, " indicators"))
  add("Power (1 - β)", pct(a$power))
  if (!is.null(res$df)) {
    add("Degrees of Freedom (df)", round(res$df, 1), "Clusters in all groups - number of groups (fewer than 20 clusters per group)")
    add("t for α", round(res$z[["alpha"]], 4), if (sided == 2) "qt(1 - α/2, df)" else "qt(1 - α, df)")
    add("t for power", round(res$z[["beta"]], 4), "qt(power, df)")
  } else {
    add("z for α", round(res$z[["alpha"]], 4), if (sided == 2) "qnorm(1 - α/2)" else "qnorm(1 - α)")
    add("z for power", round(res$z[["beta"]], 4), "qnorm(power)")
  }
  if (arms) {
    add("Treatment: baseline → endline", paste0(val(p[["t1"]]), " → ", val(p[["t2"]])), "Baseline + control change ± effect")
    add("Control: baseline → endline", paste0(val(p[["c1"]]), " → ", val(p[["c2"]])), "Baseline + control change")
  } else {
    add(if (is_mean) "Mean 1" else "Proportion 1 (p₁)", val(p[["p1"]]))
    add(if (is_mean) "Mean 2" else "Proportion 2 (p₂)", val(p[["p2"]]), if (res$mode == "mde") "Value 1 ± MDE" else "Given, or value 1 ± D")
  }
  if (is_mean) add("Standard Deviation (σ)", a$sd, "Given; assumed the same in every group and round")
  if (res$mode == "sample_size") {
    effect_formula <- switch(scale,
      relative = paste0("Relative change ", round(eval_opt(a$effect_relative, 0) * 100, 2), "% × baseline"),
      d = paste0("d = ", eval_opt(a$effect_d, NA), " × σ"),
      if (arms) "(T endline - T baseline) - (C endline - C baseline)" else "Value 2 - value 1")
    add("Detectable Difference (D)", val(res$terms$effect), effect_formula)
    if (a$p1 != 0) add("Relative Change", pct(abs(res$terms$effect) / abs(a$p1)), "|D| / baseline")
    std <- evaluation_standardised(a, res$terms$effect)
    add(std$label, round(std$value, 3), std$formula)
  }
  if (a$design %in% EVALUATION_PANEL_DESIGNS) add("Baseline-Endline Correlation (ρ)", eval_opt(a$rho, 0))
  if (a$design != "panel") add("Allocation Ratio (k)", eval_opt(a$ratio, 1), if (arms) "Control : treatment" else "Group 2 : group 1")
  v_formula <- if (is_mean) {
    switch(a$design,
      independent = "σ²(1 + 1/k)",
      panel = "2σ²(1 - ρ)",
      did_cross = "2σ²(1 + 1/k)",
      did_panel = "2σ²(1 - ρ)(1 + 1/k)",
      ancova = "σ²(1 - ρ²)(1 + 1/k)")
  } else {
    switch(a$design,
      independent = "p₁(1-p₁) + p₂(1-p₂)/k",
      panel = "p₁(1-p₁) + p₂(1-p₂) - 2ρ√(p₁(1-p₁)p₂(1-p₂))",
      did_cross = "Σ p(1-p) over treatment cells + Σ p(1-p) over control cells / k",
      did_panel = "Per arm: p₁q₁ + p₂q₂ - 2ρ√(p₁q₁p₂q₂); treatment + control / k",
      ancova = "(p_T2·q_T2 + p_C2·q_C2/k)(1 - ρ²)")
  }
  add("Variance Term (V)", signif(res$terms$v1, 5), v_formula)
  pooled <- isTRUE(a$pooled) && a$design == "independent" && !is_mean
  if (pooled) add("Variance Under No Difference (V₀)", round(res$terms$v0, 5), "p̄(1-p̄)(1 + 1/k), p̄ = (p₁ + k·p₂)/(1 + k)")
  crit <- if (is.null(res$df)) "z" else "t"
  base_formula <- if (pooled) paste0("(", crit, "_α√V₀ + ", crit, "_β√V)² / D²") else paste0("(", crit, "_α + ", crit, "_β)² × V / D²")
  add("Response Rate", pct(a$response_rate))
  if (adj$panel) add("Attrition Between Rounds", pct(adj$attrition))

  if (adj$clustered) {
    if (adj$deff_method == "cv") {
      add("Mean Interviews per Cluster (m̄)", adj$m)
      add("CV of Cluster Sizes", adj$cv_cluster)
    } else {
      add("Interviews per Cluster (m)", adj$m)
    }
    if (adj$deff_method != "known") add("Intraclass Correlation (ICC)", adj$icc)
    cluster_formula <- switch(adj$deff_method,
      icc = "1 + (m-1) × ICC",
      cv = "1 + ((CV² + 1)·m̄ - 1) × ICC",
      known = "Given (from an earlier survey)")
    add(if (adj$deff_weight > 1) "Design Effect, Clustering" else "Design Effect (DEFF)", round(adj$deff_cluster, 3), cluster_formula)
  }
  if (adj$deff_weight > 1) {
    add("CV of the Weights", adj$cv_weights)
    add("Weighting Effect (Kish)", round(adj$deff_weight, 3), "1 + CV²")
    add("Design Effect (DEFF)", round(adj$deff, 3), if (adj$clustered) "Clustering × weighting" else "Weighting effect")
  }
  if (adj$matching != 1) add("Matched Comparison Factor", adj$matching, "Allows for units lost outside common support")
  if (adj$inaccessible > 0) add("Expected Inaccessible Clusters", pct(adj$inaccessible), "Separate from non-response")
  if (adj$fpc) add("Population per Group (N)", format(adj$population, big.mark = ","))
  if (eval_opt(res$factor, 1) != 1) add("Subgroup Factor", round(res$factor, 3), "1 / smallest subgroup share")
  first <- res$groups$Group[1]

  if (res$mode == "sample_size") {
    if (!is.null(res$n_arcsine)) add("Arcsine Check (Cohen's h)", fmt_n(res$n_arcsine), "(z_α + z_β)² (1 + 1/k) / h², for comparison only")
    add("Completed Interviews per Group (no clustering)", fmt_n(res$n_base), base_formula)
    need <- res$n_base * adj$matching * res$factor
    if (adj$matching != 1 || res$factor != 1) {
      add("Completed Interviews Needed", fmt_n(need),
          paste0("n", if (adj$matching != 1) " × matching factor" else "", if (res$factor != 1) " × subgroup factor" else ""))
    }
    add(paste0("Adjusted for Non-Response", if (adj$panel) " and Attrition" else ""),
        res$groups$After_Non_Response[1],
        paste0("n / response rate", if (adj$panel) " / (1 - attrition)" else "", ", rounded up"))
    if (adj$deff != 1) add("Adjusted n × DEFF", fmt_n(need / adj$retention * adj$deff), "Adjusted n × DEFF")
    if (adj$fpc) {
      add("After Finite Population Correction", fmt_n(eval_fpc(need * adj$deff, adj$population) / adj$retention),
          "(n × DEFF) / (1 + (n × DEFF - 1)/N), then / response rate")
    }
    if (adj$clustered) {
      add("Clusters per Group", res$groups$Clusters_Needed[1], "ceiling(adjusted n / m)")
      if (adj$inaccessible > 0) {
        add("Clusters to Select per Group", res$groups$Clusters[1], "ceiling(clusters / (1 - inaccessible share))")
      }
      add("Final n per Group", res$n_per_group, if (adj$inaccessible > 0) "Clusters to select × m" else "Clusters × m")
    } else {
      add("Final n per Group", res$n_per_group, "Adjusted n, rounded up")
    }
    add("Total Sample", res$total_sample, "Sum over groups (see the group table)")
    if (adj$panel) add("Total Interviews (both rounds)", res$total_interviews, "Panel households × 2 rounds")
    add("Power at Final n", pct(res$power_achieved), paste0(if (is.null(res$df)) "Φ" else "T_df", "((|D|√n_eff - ", crit, "_α√V₀) / √V)"))
  } else {
    add(paste0("Sample per Group (", first, ")"), res$n_per_group, if (adj$clustered) "Clusters × m" else "Given")
    add("Effective Completed Interviews", fmt_n(res$n_eff),
        paste0("n", if (adj$inaccessible > 0) " × (1 - inaccessible)" else "", " × response rate",
               if (adj$panel) " × (1 - attrition)" else "", if (adj$fpc) ", FPC removed" else "",
               if (adj$deff != 1) " / DEFF" else "", if (adj$matching != 1) " / matching factor" else ""))
    add("Minimum Detectable Difference (MDE)",
        if (is_mean) val(res$mde) else paste0(round(res$mde, 4), " (", round(res$mde * 100, 1), " points)"),
        paste0("Solves ", base_formula, " = effective n for D"))
    if (a$p1 != 0) add("MDE Relative to Baseline", pct(res$mde / abs(a$p1)), "MDE / baseline")
    std <- evaluation_standardised(a, res$mde)
    add(std$label, round(std$value, 3), sub("D", "MDE", std$formula, fixed = TRUE))
    add("Power at MDE", pct(res$power_achieved), "Equals the target power")
  }
  steps
}

#' Methods paragraph for an inception report: design, indicators, every
#' assumption, the design effect, adjustments and the final sample
evaluation_report_text <- function(res, study = "") {
  a <- res$args
  adj <- res$adj
  is_mean <- eval_is_mean(a)
  arms <- a$design %in% EVALUATION_ARM_DESIGNS
  num <- function(x) format(signif(x, 4), big.mark = ",")
  pct <- function(x) paste0(round(x * 100, 1), "%")
  design_txt <- c(
    independent = "a comparison of two independent groups",
    panel = "a panel design, with the same households interviewed at baseline and endline",
    did_cross = "a difference-in-differences design with repeated cross-sections in treatment and control areas",
    did_panel = "a difference-in-differences design with a panel of treatment and control households",
    ancova = "a panel of treatment and control households, analysed by ANCOVA (endline outcome controlling for baseline)"
  )[[a$design]]
  out <- paste0("The sample", if (nzchar(study)) paste0(" for ", study) else "", " was calculated for ", design_txt, ".")

  K <- eval_opt(res$n_tests, 1)
  if (K > 1) {
    out <- paste0(out, " ", K, " key indicators were sized separately",
                  if (res$mode == "sample_size") paste0("; the largest requirement, for ", res$indicator_name, ", sets the sample") else "",
                  if (isTRUE(res$bonferroni)) paste0(", with a Bonferroni correction (α = ", signif(a$alpha, 3), " for each indicator)") else "",
                  ".")
  }
  name <- if (nzchar(eval_opt(res$indicator_name, ""))) res$indicator_name else "The key indicator"
  out <- paste0(out, " ", if (K > 1 && res$mode == "mde") "The first indicator, " else "", name, if (K > 1 && res$mode == "mde") "," else "",
                " is ", if (is_mean) "a mean" else "a proportion", " with a baseline of ", num(a$p1),
                if (is_mean) paste0(" and a standard deviation of ", num(a$sd)) else "",
                if (arms) paste0(" (control baseline ", num(res$p[["c1"]]), ", and a change of ", num(res$p[["c2"]] - res$p[["c1"]]),
                                 " expected without the programme)") else "", ".")

  test <- paste0(round(a$power * 100), "% power in a ", if (eval_opt(a$sided, 2) == 2) "two" else "one", "-sided test at α = ",
                 signif(a$alpha, 3))
  t_txt <- if (!is.null(res$df)) paste0(", using t-values with ", round(res$df, 1), " degrees of freedom because there are fewer than 20 clusters per group") else ""
  if (res$mode == "sample_size") {
    D <- abs(res$terms$effect)
    std <- evaluation_standardised(a, D)
    out <- paste0(out, " The study is powered to detect ", if (identical(a$direction, "decrease")) "a decrease" else "an increase",
                  " of ", evaluation_format_effect(a, D),
                  if (a$p1 != 0) paste0(" (", pct(D / abs(a$p1)), " of the baseline; ", sub("Standardised Effect \\((.*)\\)", "\\1", std$label),
                                        " = ", round(std$value, 2), ")") else "",
                  " with ", test, t_txt, ".")
  } else {
    out <- paste0(out, " With ", format(res$n_per_group, big.mark = ","), " units in ", tolower(res$groups$Group[1]),
                  ", the minimum detectable difference is ", evaluation_format_effect(a, res$mde),
                  if (a$p1 != 0) paste0(" (", pct(res$mde / abs(a$p1)), " of the baseline)") else "",
                  " at ", test, t_txt, ".")
  }
  if (adj$panel) out <- paste0(out, " The correlation between a household's baseline and endline values is assumed to be ρ = ", eval_opt(a$rho, 0), ".")
  if (a$design != "panel" && !isTRUE(all.equal(eval_opt(a$ratio, 1), 1))) {
    out <- paste0(out, " The ", if (arms) "control arm" else "second group", " is ", eval_opt(a$ratio, 1), " times the size of the first.")
  }
  if (adj$clustered) {
    out <- paste0(out, " Interviews are clustered, ", switch(adj$deff_method,
      icc = paste0("with ", adj$m, " per cluster and an intraclass correlation of ", adj$icc, ", giving a design effect of ", round(adj$deff_cluster, 2)),
      cv = paste0("with a mean of ", adj$m, " per cluster, a coefficient of variation of cluster sizes of ", adj$cv_cluster,
                  " and an intraclass correlation of ", adj$icc, ", giving a design effect of ", round(adj$deff_cluster, 2)),
      known = paste0("with ", adj$m, " per cluster and a design effect of ", round(adj$deff_cluster, 2), " taken from an earlier survey")))
    out <- paste0(out, if (adj$deff_weight > 1) paste0("; unequal weights add a factor of ", round(adj$deff_weight, 2),
                                                      " (Kish), for a total design effect of ", round(adj$deff, 2)) else "", ".")
  } else if (adj$deff_weight > 1) {
    out <- paste0(out, " Unequal weights give a design effect of ", round(adj$deff_weight, 2), " (Kish).")
  }
  adjust <- c(
    paste0("a ", pct(a$response_rate), " response rate"),
    if (adj$panel) paste0(pct(adj$attrition), " attrition between rounds"),
    if (adj$inaccessible > 0) paste0(pct(adj$inaccessible), " of clusters being inaccessible"),
    if (adj$matching != 1) paste0("a matching factor of ", adj$matching, " for units outside common support"),
    if (adj$fpc) paste0("a finite population correction for ", format(adj$population, big.mark = ","), " units per group")
  )
  out <- paste0(out, " The ", if (res$mode == "sample_size") "calculation" else "effective sample", " allows for ",
                if (length(adjust) > 1) paste0(paste(adjust[-length(adjust)], collapse = ", "), " and ", adjust[length(adjust)]) else adjust, ".")
  sg <- res$subgroups
  if (!is.null(sg)) {
    if (res$mode == "sample_size" && identical(a$subgroup_goal, "within")) {
      out <- paste0(out, " To detect the same difference within each subgroup (", paste(sg$Subgroup, collapse = ", "), ")",
                    if (res$factor > 1) paste0(", the sample is increased by a factor of ", round(res$factor, 2)) else "", ".")
    } else {
      out <- paste0(out, " Results by subgroup (", paste(sg$Subgroup, collapse = ", "),
                    ") will detect only larger differences; the minimum detectable difference of each is given in the subgroup table.")
    }
  }
  if (res$mode == "sample_size") {
    out <- paste0(out, " This gives ", format(res$n_per_group, big.mark = ","), " ", if (adj$panel) "households" else "units", " in ",
                  tolower(res$groups$Group[1]),
                  if (adj$clustered) paste0(" (", res$groups$Clusters[1], " clusters)") else "",
                  if (nrow(res$groups) > 1) paste0(" and ", format(res$total_sample, big.mark = ","), " in total") else "",
                  if (adj$panel) paste0(", or ", format(res$total_interviews, big.mark = ","), " interviews over both rounds") else "", ".")
  } else {
    out <- paste0(out, " The total sample is ", format(res$total_sample, big.mark = ","), " units",
                  if (adj$panel) paste0(" (", format(res$total_interviews, big.mark = ","), " interviews over both rounds)") else "", ".")
  }
  out
}
