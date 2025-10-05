# Sampling Generation Functions
# This file contains functions for generating samples using various sampling methods
# Author: Generated for Sampling Application
# Date: 2025

# Load required libraries
library(dplyr)

# =============================================================================
# CLUSTER SAMPLING FUNCTIONS
# =============================================================================

#' Probability Proportional to Size (PPS) Cluster Sampling with Replacement
#' 
#' This function performs PPS cluster sampling where clusters are selected with 
#' probability proportional to their size (MOS), and units are sampled within 
#' selected clusters.
#' 
#' @param data A data frame containing the population data
#' @param cluster_id Character string specifying the cluster identifier variable
#' @param mos Character string specifying the measure of size variable
#' @param sample_size Total number of units to sample
#' @param n_units Number of units to sample per cluster
#' @param seed Random seed for reproducibility
#' 
#' @return A list containing:
#'   - selected_clusters: Data frame of selected clusters with their MOS and probabilities
#'   - sampled_data: Data frame of sampled units
#' 
#' @examples
#' # Create sample data
#' test_data <- data.frame(
#'   cluster_id = rep(1:3, each=5),
#'   value = rnorm(15),
#'   mos = rep(c(10, 20, 30), each=5)
#' )
#' 
#' # Perform PPS cluster sampling
#' result <- cluster_sampling_pps_wr_total_size(
#'   data = test_data,
#'   cluster_id = "cluster_id",
#'   mos = "mos",
#'   sample_size = 6,
#'   n_units = 2,
#'   seed = 123
#' )
cluster_sampling_pps_wr_total_size <- function(data, cluster_id, mos, sample_size, n_units, seed) {
  # Load necessary library
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(dplyr)
  
  # Convert cluster_id and mos to symbols for tidy evaluation
  cluster_id_sym <- sym(cluster_id)
  mos_sym <- sym(mos)
  
  # Step 1: Calculate total MOS for each cluster
  cluster_mos <- data %>%
    distinct(!!cluster_id_sym, .keep_all = TRUE) %>%
    select(!!cluster_id_sym, !!mos_sym) %>%
    filter(!is.na(!!mos_sym))  # Remove rows with NA MOS values
  
  # Check if we have any clusters after filtering
  if (nrow(cluster_mos) == 0) {
    stop("No clusters found with valid MOS values")
  }
  
  # Ensure MOS is positive
  if(any(cluster_mos[[mos]] <= 0, na.rm = TRUE)) {
    stop("All Measures of Size (MOS) must be positive.")
  }
  
  # Step 2: Calculate selection probabilities
  cluster_mos <- cluster_mos %>%
    mutate(
      prob = (!!mos_sym) / sum(!!mos_sym)
    )
  
  # Step 3: Calculate number of clusters to sample
  n_clusters <- ceiling(sample_size / n_units)
  
  # Step 4: Select clusters using PPS with Replacement
  set.seed(seed)  # For reproducibility
  selected_clusters <- cluster_mos %>%
    slice_sample(n = n_clusters, weight_by = prob, replace = TRUE)
  
  # Count the number of times each cluster is selected
  cluster_counts <- selected_clusters %>%
    group_by(!!cluster_id_sym) %>%
    summarise(selection_count = n(), .groups = 'drop')
  
  # Merge the counts back to the data
  data_with_counts <- data %>%
    left_join(cluster_counts, by = cluster_id)
  
  # Replace NA counts with 0 (clusters not selected)
  data_with_counts$selection_count[is.na(data_with_counts$selection_count)] <- 0
  
  # Filter clusters with selection_count > 0
  data_selected <- data_with_counts %>%
    filter(selection_count > 0)
  
  # Step 5: Sample units within each selected cluster proportionally
  sampled_data <- data_selected %>%
    group_by(!!cluster_id_sym) %>%
    group_modify(~ {
      n_to_sample <- unique(.x$selection_count) * n_units
      cluster_size <- nrow(.x)
      if (n_to_sample > cluster_size) {
        slice_sample(.x, n = n_to_sample, replace = FALSE)
      } else {
        slice_sample(.x, n = n_to_sample, replace = FALSE)
      }
    }) %>%
    ungroup() %>%
    select(-selection_count)
  
  # Output the results
  return(list(
    selected_clusters = selected_clusters,
    sampled_data = sampled_data
  ))
}

# =============================================================================
# SIMPLE RANDOM SAMPLING FUNCTIONS
# =============================================================================

#' Simple Random Sampling
#' 
#' Performs simple random sampling without replacement from the population.
#' 
#' @param data A data frame containing the population data
#' @param sample_size Number of units to sample
#' @param seed Random seed for reproducibility
#' 
#' @return A data frame containing the sampled units
simple_random_sampling <- function(data, sample_size, seed) {
  set.seed(seed)
  
  # Validate inputs
  if (sample_size > nrow(data)) {
    stop("Sample size cannot be larger than population size")
  }
  
  # Perform simple random sampling
  sample_indices <- sample(1:nrow(data), size = sample_size, replace = FALSE)
  sampled_data <- data[sample_indices, ]
  
  return(sampled_data)
}

# =============================================================================
# SYSTEMATIC SAMPLING FUNCTIONS
# =============================================================================

#' Systematic Sampling
#' 
#' Performs systematic sampling with a random start point.
#' 
#' @param data A data frame containing the population data
#' @param sample_size Number of units to sample
#' @param seed Random seed for reproducibility
#' 
#' @return A data frame containing the sampled units
systematic_sampling <- function(data, sample_size, seed) {
  set.seed(seed)
  
  N <- nrow(data)
  k <- floor(N / sample_size)  # Sampling interval
  
  if (k < 1) {
    stop("Sample size too large for systematic sampling")
  }
  
  # Random start point
  start <- sample(1:k, 1)
  sample_indices <- seq(start, N, by = k)[1:sample_size]
  sampled_data <- data[sample_indices, ]
  
  return(sampled_data)
}

#' Equal Probability Cluster Sampling
#' 
#' Performs equal probability cluster sampling without replacement.
#' 
#' @param data A data frame containing the population data
#' @param cluster_id Character string specifying the cluster identifier variable
#' @param sample_size Total number of units to sample
#' @param n_units Number of units to sample per cluster
#' @param seed Random seed for reproducibility
#' 
#' @return A data frame containing the sampled units
equal_probability_cluster_sampling <- function(data, cluster_id, sample_size, n_units, seed) {
  set.seed(seed)
  
  # Validate inputs
  validate_sampling_params(data, sample_size, cluster_id, n_units)
  
  # Get unique clusters
  clusters <- unique(data[[cluster_id]])
  n_clusters <- length(clusters)
  
  # Calculate number of clusters to sample
  n_clusters_to_sample <- ceiling(sample_size / n_units)
  
  if (n_clusters_to_sample > n_clusters) {
    stop("Not enough clusters for the requested sample size")
  }
  
  # Stage 1: Sample clusters with equal probability (without replacement)
  sampled_clusters <- sample(clusters, n_clusters_to_sample, replace = FALSE)
  
  # Stage 2: Sample units within each selected cluster
  sample_data <- data.frame()
  
  # Count how many times each cluster was selected
  cluster_counts <- table(sampled_clusters)
  
  for (cluster_name in names(cluster_counts)) {
    cluster <- cluster_name
    times_selected <- cluster_counts[cluster_name]
    
    cluster_data <- data[data[[cluster_id]] == cluster, ]
    
    # Skip if cluster has no data
    if (nrow(cluster_data) == 0) next
    
    # Sample units for each time this cluster was selected
    for (selection in 1:times_selected) {
      # Check if cluster has enough units for the specified cluster size
      if (nrow(cluster_data) >= n_units) {
        # Cluster has enough units - sample exactly n_units units
        cluster_sample_indices <- sample(1:nrow(cluster_data), n_units, replace = FALSE)
        cluster_sample <- cluster_data[cluster_sample_indices, ]
      } else {
        # Cluster has fewer units - sample all available units
        cluster_sample_indices <- sample(1:nrow(cluster_data), nrow(cluster_data), replace = FALSE)
        cluster_sample <- cluster_data[cluster_sample_indices, ]
      }
      
      # Only add if cluster_sample has data
      if (nrow(cluster_sample) > 0) {
        sample_data <- rbind(sample_data, cluster_sample)
      }
    }
  }
  
  return(sample_data)
}


#' Cluster List Sampling
#' 
#' Performs cluster sampling using a predefined cluster list with population data.
#' 
#' @param data A data frame containing the population data
#' @param cluster_list_var Character string specifying the cluster list variable
#' @param cluster_pop_var Character string specifying the cluster population variable
#' @param sample_size Total number of units to sample
#' @param n_units Number of units to sample per cluster
#' @param use_pps Logical indicating whether to use PPS sampling
#' @param seed Random seed for reproducibility
#' 
#' @return A data frame containing the sampled units
cluster_list_sampling <- function(data, cluster_list_var, cluster_pop_var, sample_size, n_units, use_pps, seed) {
  set.seed(seed)
  
  # Validate inputs
  validate_sampling_params(data, sample_size, cluster_list_var, n_units)
  
  if (!cluster_pop_var %in% names(data)) {
    stop("Cluster population variable not found in data")
  }
  
  # Create cluster list with populations
  cluster_list <- data.frame(
    cluster = unique(data[[cluster_list_var]]),
    stringsAsFactors = FALSE
  )
  
  # Get population for each cluster
  cluster_populations <- aggregate(data[[cluster_pop_var]], 
                                 by = list(cluster = data[[cluster_list_var]]), 
                                 FUN = sum, na.rm = TRUE)
  names(cluster_populations) <- c("cluster", "population")
  
  # Merge cluster list with populations
  cluster_list <- merge(cluster_list, cluster_populations, by = "cluster", all.x = TRUE)
  
  # Calculate probabilities for PPS sampling
  if (use_pps) {
    total_population <- sum(cluster_list$population, na.rm = TRUE)
    cluster_list$probability <- cluster_list$population / total_population
  } else {
    # Equal probability sampling
    cluster_list$probability <- 1 / nrow(cluster_list)
  }
  
  n_clusters <- nrow(cluster_list)
  
  # Calculate number of clusters to sample
  n_clusters_to_sample <- ceiling(sample_size / n_units)
  
  if (n_clusters_to_sample > n_clusters) {
    stop("Not enough clusters for the requested sample size")
  }
  
  # Stage 1: Sample clusters using PPS or equal probability
  if (use_pps) {
    # PPS sampling with replacement - clusters can be selected multiple times
    sampled_clusters <- sample(cluster_list$cluster, 
                             n_clusters_to_sample, 
                             replace = TRUE, 
                             prob = cluster_list$probability)
  } else {
    # Equal probability sampling without replacement
    sampled_clusters <- sample(cluster_list$cluster, 
                             n_clusters_to_sample, 
                             replace = FALSE)
  }
  
  # Stage 2: Sample units within each cluster
  sample_data <- data.frame()
  
  # Count how many times each cluster was selected
  cluster_counts <- table(sampled_clusters)
  
  for (cluster_name in names(cluster_counts)) {
    cluster <- cluster_name
    times_selected <- cluster_counts[cluster_name]
    
    cluster_data <- data[data[[cluster_list_var]] == cluster, ]
    
    # Skip if cluster has no data
    if (nrow(cluster_data) == 0) next
    
    # Sample units for each time this cluster was selected
    for (selection in 1:times_selected) {
      # Check if cluster has enough units for the specified cluster size
      if (nrow(cluster_data) >= n_units) {
        # Cluster has enough units - sample exactly n_units units
        cluster_sample_indices <- sample(1:nrow(cluster_data), n_units, replace = FALSE)
        cluster_sample <- cluster_data[cluster_sample_indices, ]
      } else {
        # Cluster has fewer units - sample all available units
        cluster_sample_indices <- sample(1:nrow(cluster_data), nrow(cluster_data), replace = FALSE)
        cluster_sample <- cluster_data[cluster_sample_indices, ]
      }
      
      # Only add if cluster_sample has data
      if (nrow(cluster_sample) > 0) {
        sample_data <- rbind(sample_data, cluster_sample)
      }
    }
  }
  
  return(sample_data)
}


#' 
#' Creates a measure of size variable based on cluster sizes.
#' 
#' @param data A data frame containing the population data
#' @param cluster_var Character string specifying the cluster variable
#' 
#' @return A data frame with an additional 'mos' column
create_mos_from_cluster <- function(data, cluster_var) {
  # Calculate cluster sizes
  cluster_mos <- data %>% 
    group_by(!!sym(cluster_var)) %>%
    summarise(
      mos = n()  # Counts the number of units in each cluster
    ) %>%
    ungroup()
  
  # Merge MOS data with original data
  data_with_mos <- data %>%
    left_join(cluster_mos, by = cluster_var)
  
  return(data_with_mos)
}

#' Validate Sampling Parameters
#' 
#' Validates input parameters for sampling functions.
#' 
#' @param data A data frame containing the population data
#' @param sample_size Number of units to sample
#' @param cluster_var Character string specifying the cluster variable (optional)
#' @param cluster_size Number of units per cluster (optional)
#' 
#' @return TRUE if validation passes, stops with error message otherwise
validate_sampling_params <- function(data, sample_size, cluster_var = NULL, cluster_size = NULL) {
  # Validate data
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Data is empty or NULL")
  }
  
  # Validate sample size
  if (is.null(sample_size) || is.na(sample_size) || sample_size <= 0) {
    stop("Sample size must be a positive number")
  }
  
  if (sample_size > nrow(data)) {
    stop("Sample size cannot be larger than population size")
  }
  
  # Validate cluster parameters if provided
  if (!is.null(cluster_var)) {
    if (!cluster_var %in% names(data)) {
      stop("Cluster variable not found in data")
    }
    
    if (!is.null(cluster_size)) {
      if (is.null(cluster_size) || is.na(cluster_size) || cluster_size <= 0) {
        stop("Cluster size must be a positive number")
      }
    }
  }
  
  return(TRUE)
}
