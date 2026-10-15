# Quick check of the ATR Sampling Tool
# Run from the project folder: Rscript test_app.R
# 1. checks the required packages, 2. loads the functions and every module,
# 3. runs the automated tests in tests/testthat (including an app launch test).

required_packages <- c("shiny", "bslib", "shinyjs", "DT", "plotly", "dplyr", "readr", "readxl",
                       "writexl", "openxlsx", "rmarkdown", "knitr", "htmltools", "stringr", "markdown")
test_packages <- c("testthat", "withr", "callr", "curl")

missing_packages <- c(required_packages, test_packages)[
  !vapply(c(required_packages, test_packages), requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       "\nInstall them with: install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))")
}
cat("All required packages are available.\n")

# Load the functions and modules the same way app.R does
suppressPackageStartupMessages(invisible(lapply(required_packages, library, character.only = TRUE)))
source("sampling_generation_functions.R")
source("sample_size_functions.R")
module_files <- c("modules/decision_tree_module.R", "modules/sample_calculator_module.R",
                  "modules/evaluation_calculator_module.R", "modules/sampling_generator_module.R",
                  "modules/wiki_module.R", "modules/user_guide_module.R")
for (module_file in module_files) {
  source(module_file)
  cat("Loaded", module_file, "\n")
}

# A few known values
cat("\nSRS, N = 10,000, p = 0.5, 95%, e = 5%, 5% non-response:",
    calculate_simple_random_sample_size(10000, 0.5, 0.95, 0.05, 0.05), "\n")
cat("Cluster, same, m = 10, ICC = 0.06:",
    calculate_cluster_sample_size(10000, 0.5, 0.95, 0.05, cluster_size = 10, icc = 0.06), "\n")
ev <- evaluation_sample_size(list(design = "independent", p1 = 0.15, effect = 0.10, alpha = 0.05, power = 0.80,
                                  response_rate = 0.80, clustered = TRUE, icc = 0.10, m = 6))
cat("Evaluation regression case (expect 468 per group, 78 clusters):", ev$n_per_group, "per group,",
    ev$groups$Clusters[1], "clusters\n")

# Full automated test suite
cat("\nRunning tests/testthat ...\n")
results <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
failed <- sum(as.data.frame(results)$failed) + sum(as.data.frame(results)$error)
if (failed > 0) stop(failed, " test(s) failed")
cat("\nAll tests passed. Run the app with: shiny::runApp()\n")
