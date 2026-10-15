# Manual check: draw samples from the Wheat PDM frame with each design.
# Run from the project root: Rscript tests/manual_check_wheat.R
# The data file is not committed (see .gitignore).

suppressMessages(source("app.R"))  # loads packages and modules; shinyApp() object is not run

frame_file <- "All Wheat Package Wheat PDM Dataset_subset_HARMONIZED.xlsx"
d <- as.data.frame(readxl::read_excel(frame_file))
cat("Frame:", nrow(d), "rows x", ncol(d), "cols:", paste(names(d), collapse = " | "), "\n")
cat("Rows with any NA:", sum(!complete.cases(d)), "\n\n")

province <- "0.1 Province Name-Coded"
village <- "VillageName_Harmonized"

run <- function(label, spec) {
  warns <- character(0)
  res <- withCallingHandlers(
    generate_sample_design(d, spec),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) invokeRestart("muffleMessage")
  )
  s <- res$sample
  main <- s$sample_type == "Sample"
  frame_cols <- setdiff(names(s), DESIGN_COLUMNS)
  cat(sprintf("%-34s main=%4d buffer=%3d dup_units=%d sum_w=%8.1f warnings=%d\n", label,
              sum(main), sum(!main), sum(duplicated(s[, frame_cols])), sum(s$base_weight[main]), length(warns)))
  invisible(res)
}

base <- list(sample_size = 400, buffer_pct = 10, seed = 123456)
run("SRS", c(base, method = "simple_random"))
run("Systematic sorted by province", c(base, method = "systematic", sort_var = province))
run("PPS cluster (village, m=8)", c(base, method = "cluster", cluster_var = village, n_units = 8, use_pps = TRUE))
r1 <- run("Stratified proportional (province)", c(base, method = "stratified", strata_var = province,
                                                  allocation = "proportional", min_per_stratum = 5))
r2 <- run("Stratified cluster (province/village)", c(base, method = "stratified_cluster", strata_var = province,
                                                     allocation = "proportional", min_per_stratum = 8,
                                                     cluster_var = village, n_units = 8, use_pps = TRUE))
chk <- check_frame(d, c(base, method = "stratified_cluster", strata_var = province, allocation = "proportional",
                        min_per_stratum = 8, cluster_var = village, n_units = 8, use_pps = TRUE))
cat("\nFrame checks (stratified cluster):\n")
print(chk$issues, row.names = FALSE)
cat("\nWeights per stratum vs frame (stratified proportional):\n")
m <- r1$sample[r1$sample$sample_type == "Sample", ]
print(head(merge(r1$allocation[, c("stratum", "frame_units", "n_allocated")],
                 data.frame(stratum = names(tapply(m$base_weight, m$stratum, sum)),
                            sum_weights = as.vector(tapply(m$base_weight, m$stratum, sum)))), 8), row.names = FALSE)
