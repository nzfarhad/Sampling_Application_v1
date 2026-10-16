# Sampling Generation Functions
# This file contains functions for generating samples using various sampling methods
# Author: Generated for Sampling Application
# Date: 2025
#
# Every sampling function returns the selected rows of `data` with design columns:
#   - sample_type:    "Sample" for the main sample, "Buffer" for reserve units
#   - reserve_order:  NA for main units; 1, 2, 3 ... for reserves (use in this order;
#                     for cluster designs the order is within each cluster)
#   - stratum:        stratum value (stratified designs only)
#   - cluster_id:     cluster value (cluster designs only)
#   - selection_hits: times the cluster was selected (PPS with replacement can exceed 1)
#   - p_stage1:       stage-1 (cluster) inclusion probability; for PPS with replacement
#                     this is the expected number of selections and can exceed 1;
#                     1 for designs without a cluster stage
#   - p_stage2:       probability a unit is selected within its cluster or stratum,
#                     per selection of the cluster
#   - p_incl:         p_stage1 * p_stage2
#   - base_weight:    1 / p_incl. Buffer rows carry the weight a replacement takes
#                     when it replaces a main unit in the same cluster or stratum.
# Problems that do not stop the draw (short clusters, excluded rows) are raised
# with warning(); notes about how inputs were interpreted are raised with message().

DESIGN_COLUMNS <- c("sample_type", "reserve_order", "stratum", "cluster_id", "selection_hits",
                    "p_stage1", "p_stage2", "p_incl", "base_weight")

# =============================================================================
# HELPERS
# =============================================================================

#' Validate Sampling Parameters
#'
#' @param data A data frame containing the population data
#' @param sample_size Number of units to sample (including any buffer)
#' @param cluster_var Character string specifying the cluster variable (optional)
#' @param cluster_size Number of units per cluster (optional)
#'
#' @return TRUE if validation passes, stops with error message otherwise
validate_sampling_params <- function(data, sample_size, cluster_var = NULL, cluster_size = NULL) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Data is empty or NULL")
  }

  if (is.null(sample_size) || is.na(sample_size) || sample_size <= 0) {
    stop("Sample size must be a positive number")
  }

  if (sample_size > nrow(data)) {
    stop("Sample size (including buffer) cannot be larger than population size")
  }

  if (!is.null(cluster_var)) {
    if (is.na(cluster_var) || !cluster_var %in% names(data)) {
      stop("Cluster variable not found in data")
    }
    if (!is.null(cluster_size) && (is.na(cluster_size) || cluster_size <= 0)) {
      stop("Units per cluster must be a positive number")
    }
  }

  return(TRUE)
}

#' Label Main Sample and Buffer Rows
#'
#' @param data A data frame
#' @param main_idx Row indices of the main sample
#' @param buffer_idx Row indices of the reserve units, in the order they should be used
#'
#' @return The selected rows with sample_type and reserve_order columns
label_sample <- function(data, main_idx, buffer_idx = integer(0)) {
  out <- data[c(main_idx, buffer_idx), , drop = FALSE]
  out$sample_type <- c(rep("Sample", length(main_idx)), rep("Buffer", length(buffer_idx)))
  out$reserve_order <- c(rep(NA_integer_, length(main_idx)), seq_along(buffer_idx))
  rownames(out) <- NULL
  out
}

#' Set Selection Probabilities and Base Weight
#'
#' @param out Sampled rows
#' @param p_stage1 Stage-1 probability (scalar or one per row)
#' @param p_stage2 Stage-2 probability (scalar or one per row)
#'
#' @return `out` with p_stage1, p_stage2, p_incl and base_weight columns
set_probabilities <- function(out, p_stage1, p_stage2) {
  out$p_stage1 <- rep_len(p_stage1, nrow(out))
  out$p_stage2 <- rep_len(p_stage2, nrow(out))
  out$p_incl <- out$p_stage1 * out$p_stage2
  out$base_weight <- 1 / out$p_incl
  out
}

#' Is a Value Missing or Blank
is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

#' Cluster Measure of Size
#'
#' Builds one row per cluster with its measure of size (MOS).
#' Without `pop_var`, the MOS is the number of frame rows in the cluster.
#' With `pop_var`, the population value is used once per cluster when it is
#' constant within every cluster but differs between clusters (a cluster-level
#' value repeated on each row); otherwise it is summed over the cluster's rows.
#'
#' @param data A data frame containing the population data
#' @param cluster_var Character string specifying the cluster variable
#' @param pop_var Character string specifying the population variable (optional)
#'
#' @return A list with `table` (cluster, population, frame_units) and `rule` (text)
cluster_size_measure <- function(data, cluster_var, pop_var = NULL) {
  ids <- as.character(data[[cluster_var]])
  keep <- !is_blank(ids)
  ids <- ids[keep]

  frame_units <- table(ids)
  clusters <- names(frame_units)

  if (is.null(pop_var) || pop_var == "") {
    population <- as.numeric(frame_units)
    rule <- "Number of frame rows in each cluster"
  } else {
    pop <- suppressWarnings(as.numeric(data[[pop_var]][keep]))
    by_cluster <- split(pop, ids)[clusters]
    constant <- vapply(by_cluster, function(v) length(unique(v[!is.na(v)])) <= 1, logical(1))
    # A value that is the same on every row of the frame (e.g. 1 household per row)
    # is a per-row count, not a cluster-level population
    varies_between <- length(unique(pop[!is.na(pop)])) > 1
    if (all(constant) && varies_between) {
      population <- vapply(by_cluster, function(v) {
        v <- v[!is.na(v)]
        if (length(v) == 0) NA_real_ else v[1]
      }, numeric(1))
      rule <- paste0("'", pop_var, "' is constant within each cluster, so its value was used once per cluster")
    } else {
      population <- vapply(by_cluster, function(v) {
        if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
      }, numeric(1))
      rule <- paste0("'", pop_var, "' varies within clusters, so it was summed over each cluster's rows")
    }
  }

  list(
    table = data.frame(
      cluster = clusters,
      population = unname(population),
      frame_units = as.integer(frame_units),
      stringsAsFactors = FALSE
    ),
    rule = rule
  )
}

#' Sample Units Within Selected Clusters
#'
#' For a cluster selected k times, draws k * n_units main units once, without
#' replacement, plus ceiling(k * n_units * buffer_pct / 100) reserves from the
#' units left in the same cluster.
#'
#' @param data A data frame containing the population data
#' @param cluster_var Character string specifying the cluster variable
#' @param hits Named integer vector: times each cluster was selected (names = cluster IDs)
#' @param n_units Number of units to sample per cluster selection
#' @param buffer_pct Reserve units per cluster, as a percentage of its main sample
#'
#' @return The selected rows with sample_type, reserve_order, cluster_id,
#'   selection_hits, and p_stage2 (units taken per selection / cluster frame size)
draw_within_clusters <- function(data, cluster_var, hits, n_units, buffer_pct = 0) {
  cluster_ids <- as.character(data[[cluster_var]])
  pieces <- vector("list", length(hits))
  short_main <- character(0)
  short_reserve <- character(0)

  for (i in seq_along(hits)) {
    cl <- names(hits)[i]
    rows <- which(cluster_ids == cl)
    need <- hits[[i]] * n_units
    n_main <- min(need, length(rows))
    n_reserve_wanted <- ceiling(need * buffer_pct / 100)
    n_reserve <- min(n_reserve_wanted, length(rows) - n_main)

    if (n_main < need) {
      short_main <- c(short_main, sprintf("%s (needs %d, has %d)", cl, need, length(rows)))
    }
    if (n_reserve < n_reserve_wanted) {
      short_reserve <- c(short_reserve, cl)
    }

    drawn <- rows[sample.int(length(rows), n_main + n_reserve)]
    piece <- label_sample(data, drawn[seq_len(n_main)], drawn[n_main + seq_len(n_reserve)])
    piece$cluster_id <- rep(cl, nrow(piece))
    piece$selection_hits <- rep(hits[[i]], nrow(piece))
    piece$p_stage2 <- rep(n_main / (hits[[i]] * length(rows)), nrow(piece))
    pieces[[i]] <- piece
  }

  if (length(short_main) > 0) {
    warning(
      length(short_main), " selected cluster(s) had fewer units than required; all their units were taken: ",
      paste(short_main, collapse = "; "),
      call. = FALSE
    )
  }
  if (length(short_reserve) > 0) {
    warning(
      length(short_reserve), " cluster(s) did not have enough remaining units for the full set of reserves: ",
      paste(short_reserve, collapse = ", "),
      call. = FALSE
    )
  }

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

# =============================================================================
# SIMPLE RANDOM SAMPLING FUNCTIONS
# =============================================================================

#' Simple Random Sampling
#'
#' Draws sample_size + buffer_size units without replacement. The draw is in
#' random order, so the first sample_size units form the main sample and the
#' rest are reserves in the order drawn.
#'
#' @param data A data frame containing the population data
#' @param sample_size Number of units in the main sample
#' @param seed Random seed for reproducibility
#' @param buffer_size Number of reserve units
#'
#' @return The selected rows with design columns
simple_random_sampling <- function(data, sample_size, seed, buffer_size = 0) {
  data <- as.data.frame(data)
  validate_sampling_params(data, sample_size + buffer_size)
  set.seed(seed)

  idx <- sample.int(nrow(data), sample_size + buffer_size)
  out <- label_sample(data, idx[seq_len(sample_size)], idx[sample_size + seq_len(buffer_size)])
  set_probabilities(out, 1, sample_size / nrow(data))
}

# =============================================================================
# SYSTEMATIC SAMPLING FUNCTIONS
# =============================================================================

#' Systematic Sampling
#'
#' Uses a fractional interval k = N / n and a random start r in (0, k], selecting
#' positions ceiling(r + (i - 1) * k). Every unit has inclusion probability n / N.
#' Reserves are drawn at random from the units not selected.
#'
#' @param data A data frame containing the population data
#' @param sample_size Number of units in the main sample
#' @param seed Random seed for reproducibility
#' @param buffer_size Number of reserve units
#' @param sort_by Optional variable to order the frame by first (implicit stratification)
#'
#' @return The selected rows with design columns
systematic_sampling <- function(data, sample_size, seed, buffer_size = 0, sort_by = NULL) {
  data <- as.data.frame(data)
  validate_sampling_params(data, sample_size + buffer_size)
  set.seed(seed)

  N <- nrow(data)
  frame_order <- if (!is.null(sort_by) && sort_by != "") {
    if (!sort_by %in% names(data)) stop("Sort variable not found in data")
    order(data[[sort_by]], na.last = TRUE)
  } else {
    seq_len(N)
  }

  k <- N / sample_size
  start <- runif(1, 0, k)
  positions <- pmax(1, ceiling(start + (seq_len(sample_size) - 1) * k))
  main_idx <- frame_order[positions]

  remaining <- setdiff(seq_len(N), main_idx)
  buffer_idx <- remaining[sample.int(length(remaining), buffer_size)]

  out <- label_sample(data, main_idx, buffer_idx)
  set_probabilities(out, 1, sample_size / N)
}

# =============================================================================
# CLUSTER SAMPLING FUNCTIONS
# =============================================================================

#' Two-Stage Cluster Sampling
#'
#' Stage 1 selects a = ceiling(sample_size / n_units) clusters, either with
#' probability proportional to size (PPS, with replacement) or with equal
#' probability (without replacement). Stage 2 samples n_units per selection
#' within each selected cluster, plus reserves (see draw_within_clusters).
#'
#' Probabilities: PPS p_stage1 = a * M_i / M (expected selections); equal
#' probability p_stage1 = a / C. p_stage2 = units taken / (selections * N_i).
#' The base weight then matches the Hansen-Hurwitz estimator for PPS with
#' replacement, and is constant when the size measure equals the frame count.
#'
#' @param data A data frame containing the population data
#' @param cluster_var Character string specifying the cluster variable
#' @param sample_size Target number of units in the main sample
#' @param n_units Number of units to sample per selected cluster (interviews per cluster)
#' @param seed Random seed for reproducibility
#' @param use_pps Logical: select clusters with probability proportional to size
#' @param pop_var Optional population variable used as the measure of size; if
#'   NULL, the number of frame rows per cluster is used
#' @param buffer_pct Reserve units per cluster, as a percentage of its main sample
#'
#' @return The selected rows with design columns
two_stage_cluster_sampling <- function(data, cluster_var, sample_size, n_units, seed,
                                       use_pps = TRUE, pop_var = NULL, buffer_pct = 0) {
  data <- as.data.frame(data)
  validate_sampling_params(data, sample_size, cluster_var, n_units)
  if (!is.null(pop_var) && pop_var != "" && !pop_var %in% names(data)) {
    stop("Cluster population variable not found in data")
  }
  set.seed(seed)

  missing_ids <- sum(is_blank(data[[cluster_var]]))
  if (missing_ids > 0) {
    warning(missing_ids, " row(s) have no value in '", cluster_var,
            "' and were excluded from the frame.", call. = FALSE)
  }

  mos <- cluster_size_measure(data, cluster_var, pop_var)
  clusters <- mos$table
  if (!is.null(pop_var) && pop_var != "") {
    message("Cluster size measure: ", mos$rule, ".")
  }

  n_clusters <- ceiling(sample_size / n_units)

  if (use_pps) {
    invalid <- is.na(clusters$population) | clusters$population <= 0
    if (any(invalid)) {
      warning(sum(invalid), " cluster(s) have a missing or zero size measure and cannot be selected: ",
              paste(head(clusters$cluster[invalid], 20), collapse = ", "),
              if (sum(invalid) > 20) ", ..." else "", call. = FALSE)
      clusters <- clusters[!invalid, , drop = FALSE]
    }
    if (nrow(clusters) == 0) stop("No clusters with a valid size measure")

    draws <- sample(clusters$cluster, n_clusters, replace = TRUE,
                    prob = clusters$population / sum(clusters$population))
    p_stage1 <- setNames(n_clusters * clusters$population / sum(clusters$population), clusters$cluster)
  } else {
    if (n_clusters > nrow(clusters)) {
      stop("Not enough clusters for the requested sample size (need ", n_clusters,
           ", frame has ", nrow(clusters), ")")
    }
    draws <- clusters$cluster[sample.int(nrow(clusters), n_clusters)]
    p_stage1 <- setNames(rep(n_clusters / nrow(clusters), nrow(clusters)), clusters$cluster)
  }

  # Keep clusters in the order they were first drawn
  hits <- table(factor(draws, levels = unique(draws)))
  hits <- setNames(as.integer(hits), names(hits))

  out <- draw_within_clusters(data, cluster_var, hits, n_units, buffer_pct)
  set_probabilities(out, unname(p_stage1[out$cluster_id]), out$p_stage2)
}

# =============================================================================
# STRATIFIED SAMPLING FUNCTIONS
# =============================================================================

#' Allocate a Sample Across Strata
#'
#' Equal, proportional or Neyman allocation with a minimum per stratum and no
#' stratum above its frame size; rounded by largest remainder so the total is n.
#' Manual allocation returns the entered sizes unchanged.
#'
#' @param population Frame units per stratum
#' @param n Total sample size
#' @param method "equal", "proportional", "neyman" or "manual"
#' @param min_n Minimum sample size per stratum (capped at the stratum's frame size)
#' @param values Neyman: standard deviation per stratum; manual: sample size per stratum
#'
#' @return Integer sample size per stratum
allocate_sample <- function(population, n, method = "proportional", min_n = 0, values = NULL) {
  H <- length(population)

  if (method == "manual") {
    if (is.null(values) || length(values) != H || any(is.na(values) | values < 0)) {
      stop("Enter a sample size of 0 or more for every stratum")
    }
    return(as.integer(round(values)))
  }

  if (n > sum(population)) stop("Sample size (", n, ") exceeds the frame size (", sum(population), ")")

  target <- switch(method,
    equal = rep(1, H),
    proportional = population,
    neyman = {
      if (is.null(values) || length(values) != H || any(is.na(values) | values < 0)) {
        stop("Neyman allocation needs a standard deviation or proportion for every stratum")
      }
      population * values
    },
    stop("Unknown allocation method: ", method)
  )
  if (sum(target) <= 0) stop("Allocation weights are all zero")

  min_n <- pmin(rep_len(min_n, H), population)
  if (sum(min_n) > n) {
    stop("The minimum per stratum needs ", sum(min_n), " units, more than the total sample size (", n, ")")
  }

  # Fix strata that fall below the minimum or above their frame size, and
  # reallocate the rest; each pass fixes at least one more stratum
  fixed <- rep(NA_real_, H)
  repeat {
    free <- is.na(fixed)
    alloc <- fixed
    if (any(free)) {
      remaining <- n - sum(fixed[!free])
      alloc[free] <- if (sum(target[free]) > 0) remaining * target[free] / sum(target[free]) else 0
    }
    high <- free & alloc > population
    low <- free & !high & alloc < min_n
    if (!any(high | low)) break
    fixed[high] <- population[high]
    fixed[low] <- min_n[low]
  }

  out <- floor(alloc + 1e-9)
  short <- n - sum(out)
  if (short > 0) {
    frac <- alloc - out
    frac[out >= population] <- -Inf
    idx <- order(-frac)[seq_len(short)]
    out[idx] <- out[idx] + 1
  }
  as.integer(out)
}

#' Compute the Stratum Allocation Table
#'
#' @param data A data frame containing the population data
#' @param spec Design specification (see generate_sample_design)
#'
#' @return Data frame with one row per stratum: frame units, clusters (cluster
#'   designs), allocation input value, allocated n, buffer or clusters needed,
#'   and the stratum's random seed (derived from the master seed)
compute_allocation <- function(data, spec) {
  strata <- as.character(data[[spec$strata_var]])
  keep <- !is_blank(strata)
  frame_units <- table(strata[keep])
  stratum_names <- names(frame_units)
  population <- as.numeric(frame_units)

  values <- NULL
  if (spec$allocation %in% c("neyman", "manual")) {
    raw <- spec$allocation_values
    if (is.null(raw)) raw <- numeric(0)
    values <- unname(raw[stratum_names])
    if (spec$allocation == "neyman" && identical(spec$neyman_input, "proportion")) {
      if (any(!is.na(values) & (values < 0 | values > 1))) stop("Expected proportions must be between 0 and 1")
      values <- sqrt(values * (1 - values))
    }
  }

  n_alloc <- allocate_sample(population, spec$sample_size, spec$allocation,
                             if (is.null(spec$min_per_stratum)) 0 else spec$min_per_stratum, values)

  set.seed(spec$seed)
  stratum_seeds <- sample.int(.Machine$integer.max, length(stratum_names))

  alloc <- data.frame(
    stratum = stratum_names,
    frame_units = as.integer(population),
    stringsAsFactors = FALSE
  )
  if (spec$allocation %in% c("neyman", "manual")) {
    input <- spec$allocation_values
    alloc$allocation_input <- if (is.null(input)) NA_real_ else unname(input[stratum_names])
  }
  alloc$n_allocated <- n_alloc
  alloc$sampling_fraction_pct <- round(n_alloc / population * 100, 2)

  if (spec$method == "stratified_cluster") {
    cluster_ids <- as.character(data[[spec$cluster_var]])[keep]
    alloc$clusters_in_frame <- as.integer(tapply(cluster_ids, strata[keep], function(v) length(unique(v[!is_blank(v)])))[stratum_names])
    alloc$clusters_to_select <- as.integer(ceiling(n_alloc / spec$n_units))
  } else {
    alloc$buffer <- as.integer(pmin(round(n_alloc * spec$buffer_pct / 100), population - n_alloc))
  }
  alloc$seed <- stratum_seeds
  alloc
}

#' Stratified Sampling
#'
#' Draws each stratum independently with its own seed. Within strata: simple
#' random, systematic, or two-stage cluster sampling (when cluster_var is given).
#' Warnings and errors are prefixed with the stratum name.
#'
#' @param data A data frame containing the population data
#' @param strata_var Character string specifying the strata variable
#' @param allocation Allocation table from compute_allocation()
#' @param within "simple_random" or "systematic" (ignored for cluster designs)
#' @param sort_by Optional sort variable for systematic selection
#' @param cluster_var,n_units,use_pps,pop_var,buffer_pct Cluster settings (see two_stage_cluster_sampling)
#'
#' @return The selected rows with design columns, including stratum
stratified_sampling <- function(data, strata_var, allocation, within = "simple_random", sort_by = NULL,
                                cluster_var = NULL, n_units = NULL, use_pps = TRUE, pop_var = NULL,
                                buffer_pct = 0) {
  data <- as.data.frame(data)
  strata <- as.character(data[[strata_var]])
  pieces <- list()

  for (i in seq_len(nrow(allocation))) {
    h <- allocation$stratum[i]
    n_h <- allocation$n_allocated[i]
    if (n_h <= 0) next
    sub <- data[!is.na(strata) & strata == h, , drop = FALSE]
    prefix <- paste0("Stratum ", h, ": ")

    piece <- withCallingHandlers(
      tryCatch({
        if (!is.null(cluster_var) && cluster_var != "") {
          two_stage_cluster_sampling(sub, cluster_var, n_h, n_units, allocation$seed[i],
                                     use_pps, pop_var, buffer_pct)
        } else if (within == "systematic") {
          systematic_sampling(sub, n_h, allocation$seed[i], allocation$buffer[i], sort_by)
        } else {
          simple_random_sampling(sub, n_h, allocation$seed[i], allocation$buffer[i])
        }
      }, error = function(e) stop(prefix, conditionMessage(e), call. = FALSE)),
      warning = function(w) {
        warning(prefix, conditionMessage(w), call. = FALSE)
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        # The size-measure rule is the same in every stratum; report it once
        if (i == 1 || !grepl("^Cluster size measure", conditionMessage(m))) {
          message(prefix, trimws(conditionMessage(m)))
        }
        invokeRestart("muffleMessage")
      }
    )
    piece$stratum <- rep(h, nrow(piece))
    pieces[[length(pieces) + 1]] <- piece
  }

  if (length(pieces) == 0) stop("No stratum has a sample allocated")
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

# =============================================================================
# FRAME CHECKS AND DESIGN DRIVER
# =============================================================================

#' Check the Frame Before Drawing
#'
#' @param data A data frame containing the population data
#' @param spec Design specification (see generate_sample_design)
#'
#' @return A list with `issues` (level, check, detail), `allocation` (stratified
#'   designs), `exclusions` (reason, rows) and `can_proceed`
check_frame <- function(data, spec) {
  data <- as.data.frame(data)
  issues <- data.frame(level = character(0), check = character(0), detail = character(0), stringsAsFactors = FALSE)
  add <- function(level, check, detail) {
    issues[nrow(issues) + 1, ] <<- list(level, check, detail)
  }
  exclusions <- data.frame(reason = character(0), rows = integer(0), stringsAsFactors = FALSE)
  exclude <- function(reason, rows) {
    if (rows > 0) exclusions[nrow(exclusions) + 1, ] <<- list(reason, as.integer(rows))
  }

  is_strat <- spec$method %in% c("stratified", "stratified_cluster")
  is_cluster <- spec$method %in% c("cluster", "stratified_cluster")
  add("info", "Frame size", paste(format(nrow(data), big.mark = ","), "rows"))

  # Unit IDs
  if (is.null(spec$id_var) || spec$id_var == "") {
    add("info", "Unit ID", "No ID variable selected. Selecting one lets the app check for duplicates and fills the replacement log.")
  } else {
    ids <- data[[spec$id_var]]
    n_missing <- sum(is_blank(ids))
    n_dup <- sum(duplicated(ids[!is_blank(ids)]))
    if (n_missing > 0) add("warning", "Unit ID", paste(n_missing, "row(s) have no value in", spec$id_var))
    if (n_dup > 0) add("warning", "Unit ID", paste(n_dup, "duplicate value(s) in", spec$id_var, "- the same unit may be listed twice"))
    if (n_missing == 0 && n_dup == 0) add("ok", "Unit ID", paste("All values of", spec$id_var, "are present and unique"))
  }

  # Strata
  if (is_strat) {
    n_missing <- sum(is_blank(data[[spec$strata_var]]))
    if (n_missing > 0) {
      add("warning", "Strata", paste(n_missing, "row(s) have no stratum and are excluded"))
      exclude(paste0("No value in strata variable '", spec$strata_var, "'"), n_missing)
    } else {
      add("ok", "Strata", paste(length(unique(data[[spec$strata_var]])), "strata, all rows have a stratum"))
    }
  }

  # Clusters
  if (is_cluster) {
    cl <- data[[spec$cluster_var]]
    n_missing <- sum(is_blank(cl))
    if (n_missing > 0) {
      add("warning", "Clusters", paste(n_missing, "row(s) have no cluster ID and are excluded"))
      exclude(paste0("No value in cluster variable '", spec$cluster_var, "'"), n_missing)
    }
    mos <- cluster_size_measure(data, spec$cluster_var, spec$pop_var)
    if (!is.null(spec$pop_var) && spec$pop_var != "") add("info", "Cluster size measure", mos$rule)
    if (isTRUE(spec$use_pps)) {
      bad <- is.na(mos$table$population) | mos$table$population <= 0
      if (any(bad)) {
        add("warning", "Cluster size measure", paste(sum(bad), "cluster(s) have a missing or zero size measure and cannot be selected"))
        exclude("Cluster has a missing or zero size measure", sum(mos$table$frame_units[bad]))
      }
    }
    small <- mos$table$frame_units < spec$n_units
    if (any(small)) {
      advice <- if (mean(small) > 0.2) {
        paste0(". Consider fewer interviews per cluster (median cluster has ",
               stats::median(mos$table$frame_units), " units) or merging small clusters with neighbours")
      } else ""
      add("warning", "Cluster size", paste0(sum(small), " of ", nrow(mos$table), " clusters have fewer than ",
                                            spec$n_units, " units; if selected, all their units are taken and the sample falls short",
                                            advice))
    } else {
      add("ok", "Cluster size", paste("All", nrow(mos$table), "clusters have at least", spec$n_units, "units"))
    }
    if (is_strat) {
      keep <- !is_blank(cl) & !is_blank(data[[spec$strata_var]])
      strata_per_cluster <- tapply(as.character(data[[spec$strata_var]])[keep], as.character(cl)[keep], function(v) length(unique(v)))
      if (any(strata_per_cluster > 1)) {
        add("warning", "Clusters", paste(sum(strata_per_cluster > 1), "cluster(s) appear in more than one stratum; each part is treated as a separate cluster"))
      }
    }
  }

  # Sample size and allocation
  allocation <- NULL
  if (is_strat) {
    allocation <- tryCatch(compute_allocation(data, spec), error = function(e) {
      add("error", "Allocation", conditionMessage(e))
      NULL
    })
    if (!is.null(allocation)) {
      over <- allocation$n_allocated > allocation$frame_units
      if (any(over)) add("error", "Allocation", paste("Allocated sample exceeds the frame in:", paste(allocation$stratum[over], collapse = ", ")))
      if (spec$method == "stratified_cluster" && !isTRUE(spec$use_pps)) {
        few <- allocation$clusters_to_select > allocation$clusters_in_frame
        if (any(few)) add("error", "Allocation", paste("Not enough clusters for equal-probability selection in:", paste(allocation$stratum[few], collapse = ", ")))
      }
      if (spec$method == "stratified") {
        capped <- round(allocation$n_allocated * spec$buffer_pct / 100) > allocation$buffer
        if (any(capped)) add("info", "Buffer", paste("Buffer reduced to the units left in:", paste(allocation$stratum[capped], collapse = ", ")))
      }
      add("info", "Allocation", paste0(spec$allocation, " allocation, total ", sum(allocation$n_allocated), " units"))
    }
  } else if (spec$method %in% c("simple_random", "systematic")) {
    total <- spec$sample_size + round(spec$sample_size * spec$buffer_pct / 100)
    if (total > nrow(data)) add("error", "Sample size", paste("Sample plus buffer (", total, ") exceeds the frame (", nrow(data), ")"))
  } else if (spec$method == "cluster") {
    needed <- ceiling(spec$sample_size / spec$n_units)
    n_cl <- length(unique(as.character(data[[spec$cluster_var]][!is_blank(data[[spec$cluster_var]])])))
    add("info", "Clusters", paste(needed, "cluster selections of", spec$n_units, "units from", n_cl, "clusters"))
    if (!isTRUE(spec$use_pps) && needed > n_cl) add("error", "Clusters", paste("Need", needed, "clusters but the frame has", n_cl))
  }

  list(issues = issues, allocation = allocation, exclusions = exclusions, can_proceed = !any(issues$level == "error"))
}

#' Summarise Clusters After a Cluster Draw
#'
#' One row per frame cluster (per stratum for stratified cluster designs): size
#' measure, selection probability per draw, times selected, and main/reserve
#' units drawn. Selected clusters are listed first.
#'
#' @param data The frame used for the draw
#' @param sample The sample returned by generate_sample_design()
#' @param spec The design specification
#'
#' @return A data frame
summarise_clusters <- function(data, sample, spec) {
  data <- as.data.frame(data)
  strat <- spec$method == "stratified_cluster"
  groups <- if (strat) {
    strata <- as.character(data[[spec$strata_var]])
    split(seq_len(nrow(data)), strata)
  } else {
    list(All = seq_len(nrow(data)))
  }

  zero_if_na <- function(x) { x[is.na(x)] <- 0L; as.integer(x) }

  pieces <- lapply(names(groups), function(h) {
    mos <- cluster_size_measure(data[groups[[h]], , drop = FALSE], spec$cluster_var, spec$pop_var)$table
    s <- if (strat) sample[!is.na(sample$stratum) & sample$stratum == h, , drop = FALSE] else sample
    valid <- !is.na(mos$population) & mos$population > 0
    prob <- if (isTRUE(spec$use_pps)) ifelse(valid, mos$population / sum(mos$population[valid]), 0) else rep(1 / nrow(mos), nrow(mos))
    is_main <- s$sample_type == "Sample"
    hits <- tapply(s$selection_hits, s$cluster_id, function(v) v[1])

    out <- data.frame(
      stratum = rep(h, nrow(mos)),
      cluster = mos$cluster,
      size_measure = mos$population,
      frame_units = mos$frame_units,
      probability_per_draw = round(prob, 6),
      times_selected = zero_if_na(hits[mos$cluster]),
      sample_count = zero_if_na(table(s$cluster_id[is_main])[mos$cluster]),
      buffer_count = zero_if_na(table(s$cluster_id[!is_main])[mos$cluster]),
      stringsAsFactors = FALSE
    )
    out$required_sample <- out$times_selected * spec$n_units
    out$shortfall <- pmax(0L, out$required_sample - out$sample_count)
    out
  })

  out <- do.call(rbind, pieces)
  out <- out[order(out$times_selected == 0, out$stratum, -out$times_selected, out$cluster), ]
  if (!strat) out$stratum <- NULL
  rownames(out) <- NULL
  out
}

#' Rename Frame Columns That Clash With Design Columns
#'
#' @return `data` with clashing columns renamed to <name>_frame
protect_design_columns <- function(data) {
  clash <- intersect(names(data), DESIGN_COLUMNS)
  if (length(clash) > 0) {
    names(data)[match(clash, names(data))] <- paste0(clash, "_frame")
    message("Frame column(s) renamed to avoid clashing with design columns: ",
            paste0(clash, " -> ", clash, "_frame", collapse = ", "))
  }
  data
}

#' Generate a Sample From a Design Specification
#'
#' @param data A data frame containing the population data
#' @param spec A list with:
#'   method ("simple_random", "systematic", "stratified", "cluster", "stratified_cluster"),
#'   sample_size, buffer_pct, seed, and as needed: sort_var, within ("simple_random"
#'   or "systematic"), strata_var, allocation ("equal", "proportional", "neyman",
#'   "manual"), allocation_values (named by stratum), neyman_input ("sd" or
#'   "proportion"), min_per_stratum, cluster_var, pop_var, n_units, use_pps
#'
#' @return A list with `sample` (design columns first) and `allocation` (stratified designs)
generate_sample_design <- function(data, spec) {
  data <- protect_design_columns(as.data.frame(data))
  buffer_pct <- if (is.null(spec$buffer_pct) || is.na(spec$buffer_pct)) 0 else spec$buffer_pct
  allocation <- NULL

  s <- switch(spec$method,
    simple_random = simple_random_sampling(data, spec$sample_size, spec$seed,
                                           round(spec$sample_size * buffer_pct / 100)),
    systematic = systematic_sampling(data, spec$sample_size, spec$seed,
                                     round(spec$sample_size * buffer_pct / 100), spec$sort_var),
    cluster = two_stage_cluster_sampling(data, spec$cluster_var, spec$sample_size, spec$n_units, spec$seed,
                                         isTRUE(spec$use_pps), spec$pop_var, buffer_pct),
    stratified = ,
    stratified_cluster = {
      if (is.null(spec$strata_var) || !spec$strata_var %in% names(data)) stop("Strata variable not found in data")
      allocation <- compute_allocation(data, spec)
      is_cluster <- spec$method == "stratified_cluster"
      stratified_sampling(
        data, spec$strata_var, allocation,
        within = if (is.null(spec$within)) "simple_random" else spec$within,
        sort_by = spec$sort_var,
        cluster_var = if (is_cluster) spec$cluster_var else NULL,
        n_units = spec$n_units, use_pps = isTRUE(spec$use_pps), pop_var = spec$pop_var,
        buffer_pct = buffer_pct
      )
    },
    stop("Unknown sampling method: ", spec$method)
  )

  for (col in setdiff(DESIGN_COLUMNS, names(s))) s[[col]] <- NA
  s <- s[, c(DESIGN_COLUMNS, setdiff(names(s), DESIGN_COLUMNS))]
  list(sample = s, allocation = allocation)
}
