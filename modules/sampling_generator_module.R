# Sampling Generator Module
# Allows users to draw samples directly from uploaded datasets

# Sampling Generator UI
sampling_generator_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .sampling-generator-container {
          max-width: 1200px;
          margin: 0 auto;
        }
        
        .file-upload-area {
          border: 2px dashed #730202;
          border-radius: 8px;
          padding: 30px;
          text-align: center;
          background-color: #f8f9fa;
          margin-bottom: 20px;
        }
        
        .file-upload-area:hover {
          background-color: #e9ecef;
          border-color: #5a0202;
        }
        
        .sampling-method-card {
          border: 1px solid #dee2e6;
          border-radius: 8px;
          margin-bottom: 15px;
          transition: all 0.3s ease;
        }
        
        .sampling-method-card:hover {
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        
        .sample-results {
          background-color: #f8f9fa;
          border-radius: 8px;
          padding: 20px;
          margin-top: 20px;
        }
        
        .sample-preview-table {
          font-size: 0.9rem;
        }
        
        .sample-preview-table table {
          table-layout: fixed;
        }
        
        .sample-preview-table td {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 200px;
          height: 40px;
          vertical-align: middle;
        }
        
        .sample-preview-table th {
          height: 40px;
          vertical-align: middle;
        }
        
        .cluster-summary-table {
          font-size: 0.9rem;
        }
        
        .cluster-summary-table table {
          table-layout: fixed;
        }
        
        .cluster-summary-table td {
          text-align: center;
          padding: 8px;
          vertical-align: middle;
        }
        
        .cluster-summary-table th {
          text-align: center;
          padding: 8px;
          font-weight: bold;
          background-color: #f8f9fa;
          vertical-align: middle;
        }
        
        .btn-generate-sample {
          background-color: #730202;
          border-color: #730202;
          color: white;
          font-weight: 600;
          padding: 12px 30px;
          border-radius: 6px;
        }
        
        .btn-generate-sample:hover {
          background-color: #5a0202;
          border-color: #5a0202;
        }
        
        .conditional-inputs {
          background-color: #f8f9fa;
          border-radius: 6px;
          padding: 15px;
          margin-top: 15px;
        }
        
        .draw-messages ul {
          padding-left: 1.2rem;
        }

        .draw-messages details summary {
          cursor: pointer;
          font-size: 0.85rem;
        }

        .draw-messages details ul {
          max-height: 220px;
          overflow-y: auto;
          font-size: 0.85rem;
          margin-top: 4px;
        }

        .variable-names-modal .modal-dialog {
          max-width: 600px;
        }
        
        .variable-names-list {
          max-height: 400px;
          overflow-y: auto;
        }
        
        .variable-item {
          padding: 8px 12px;
          border-bottom: 1px solid #eee;
          font-family: monospace;
          font-size: 0.9rem;
        }
        
        .variable-item:hover {
          background-color: #f8f9fa;
        }
      "))
    ),
    
    # Header
    div(
      class = "sampling-generator-container",
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2("Sample Generator", class = "text-center mb-3"),
          p("Upload your dataset and generate samples using various sampling methods.",
            class = "text-center text-muted")
        )
      ),
      uiOutput(ns("handover_note")),
      
      # File Upload Section
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            class = "card",
            div(
              class = "card-header",
              h5("📁 Upload Dataset", class = "mb-0")
            ),
            div(
              class = "card-body",
              div(
                class = "file-upload-area",
                fileInput(
                  ns("dataset_file"),
                  "Choose CSV or Excel file",
                  accept = c(".csv", ".xlsx", ".xls"),
                  placeholder = "No file selected",
                  width = "100%"
                ),
                p("Supported formats: CSV, Excel (.xlsx, .xls)",
                  class = "text-muted small mt-2")
              ),
              div(
                class = "alert alert-warning small mt-2 mb-0",
                tags$strong("Data protection: "),
                "This app runs on a public cloud host. Do not upload names, phone numbers, GPS points or other identifiers. ",
                "Use anonymised IDs and link back to the full list offline."
              ),
              
              # File info display
              div(
                id = ns("file_info_display"),
                style = "display: none;",
                div(
                  class = "alert alert-success",
                  div(
                    class = "row align-items-center",
                    div(
                      class = "col-md-8",
                      p(id = ns("file_name_display"), class = "mb-1"),
                      p(id = ns("file_dimensions_display"), class = "mb-0 small")
                    ),
                    div(
                      class = "col-md-4 text-end",
                      actionButton(ns("show_variable_names"), "📋 View Variables", 
                                  class = "btn btn-sm btn-outline-primary")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Sampling Parameters Section
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Sampling Parameters", class = "mb-0")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                
                # Sampling Method Selection
                div(
                  class = "col-md-3",
                  selectInput(
                    ns("sampling_method"),
                    "Sampling Method",
                    choices = list(
                      "Simple Random Sampling" = "simple_random",
                      "Systematic Sampling" = "systematic",
                      "Stratified Random Sampling" = "stratified",
                      "Cluster Sampling" = "cluster",
                      "Stratified Cluster Sampling" = "stratified_cluster"
                    ),
                    selected = "simple_random",
                    width = "100%"
                  ),
                  p("Choose the sampling method to use", class = "text-muted small")
                ),
                
                # Sample Size Input
                div(
                  class = "col-md-3",
                  numericInput(
                    ns("sample_size"),
                    "Sample Size",
                    value = 100,
                    min = 1,
                    step = 1,
                    width = "100%"
                  ),
                  p("Main sample size (the total across strata for stratified designs)", class = "text-muted small")
                ),
                
                # Buffer Input
                div(
                  class = "col-md-3",
                  numericInput(
                    ns("buffer_percentage"),
                    "Buffer (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1,
                    width = "100%"
                  ),
                  p("Reserve units for replacements, drawn separately from the main sample", class = "text-muted small")
                ),
                
                # Random Seed Input
                div(
                  class = "col-md-3",
                  numericInput(
                    ns("random_seed"),
                    "Random Seed",
                    value = 123456,
                    min = 1,
                    step = 1,
                    width = "100%"
                  ),
                  p("Seed for reproducible results", class = "text-muted small")
                )
              ),

              # Study name and unit ID (recorded in the export)
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  textInput(
                    ns("study_name"),
                    "Study Name",
                    placeholder = "e.g. Wheat PDM Round 2",
                    width = "100%"
                  ),
                  p("Recorded in the Methodology sheet of the export", class = "text-muted small")
                ),
                div(
                  class = "col-md-6",
                  selectInput(
                    ns("id_variable"),
                    "Unit ID Variable (recommended)",
                    choices = c("None" = ""),
                    width = "100%"
                  ),
                  p("Used to check for duplicate units and to fill the replacement log", class = "text-muted small")
                )
              ),

              # Conditional Inputs
              div(
                id = ns("conditional_inputs"),
                class = "conditional-inputs",
                style = "display: none;",
                
                # Strata Variable Selector (for stratified methods)
                div(
                  id = ns("strata_inputs"),
                  style = "display: none;",
                  h6("Stratification Parameters", class = "mb-3"),
                  div(
                    class = "row",
                    div(
                      class = "col-md-4",
                      selectInput(
                        ns("strata_variable"),
                        "Strata Variable",
                        choices = c("Select a variable..." = ""),
                        width = "100%"
                      ),
                      p("Variable that defines strata, e.g. province or sex", class = "text-muted small")
                    ),
                    div(
                      class = "col-md-4",
                      selectInput(
                        ns("allocation_method"),
                        "Allocation",
                        choices = c(
                          "Proportional to stratum size" = "proportional",
                          "Equal per stratum" = "equal",
                          "Neyman (optimal)" = "neyman",
                          "Manual (enter per stratum)" = "manual"
                        ),
                        width = "100%"
                      ),
                      p("How the total sample is split across strata", class = "text-muted small")
                    ),
                    div(
                      class = "col-md-4",
                      numericInput(
                        ns("min_per_stratum"),
                        "Minimum per Stratum",
                        value = 0,
                        min = 0,
                        step = 1,
                        width = "100%"
                      ),
                      p("No stratum gets fewer than this (not used for manual)", class = "text-muted small")
                    )
                  ),
                  div(
                    class = "row",
                    div(
                      id = ns("within_method_inputs"),
                      class = "col-md-4",
                      radioButtons(
                        ns("within_method"),
                        "Selection Within Strata",
                        choices = c("Simple random" = "simple_random", "Systematic" = "systematic"),
                        inline = TRUE
                      )
                    ),
                    div(
                      id = ns("neyman_inputs"),
                      class = "col-md-8",
                      style = "display: none;",
                      radioButtons(
                        ns("neyman_input"),
                        "Neyman Input per Stratum",
                        choices = c("Expected proportion (0-1)" = "proportion", "Standard deviation" = "sd"),
                        inline = TRUE
                      )
                    )
                  ),
                  p(id = ns("allocation_help"), class = "text-muted small mb-2",
                    "Allocation preview. For Neyman or manual allocation, double-click a cell in the Input column, type the value, then click outside the cell or press Tab to save it."),
                  DT::dataTableOutput(ns("allocation_table"))
                ),
                
                # Systematic Parameters
                div(
                  id = ns("systematic_inputs"),
                  style = "display: none;",
                  h6("Systematic Sampling Parameters", class = "mb-3"),
                  div(
                    class = "row",
                    div(
                      class = "col-md-6",
                      selectInput(
                        ns("systematic_sort_variable"),
                        "Sort Frame By (optional)",
                        choices = c("Keep file order" = ""),
                        width = "100%"
                      )
                    ),
                    div(
                      class = "col-md-6",
                      p("Sorting by a variable such as district spreads the sample across its values (implicit stratification)", class = "text-muted small")
                    )
                  )
                ),

                # Cluster Parameters (for cluster methods)
                div(
                  id = ns("cluster_inputs"),
                  style = "display: none;",
                  h6("Cluster Parameters", class = "mb-3"),
                  
                  # Cluster Sampling Method Selection
                  div(
                    class = "row mb-3",
                    div(
                      class = "col-12",
                      radioButtons(
                        ns("cluster_sampling_method"),
                        "Cluster Sampling Method",
                        choices = list(
                          "Method 1: Sample from Population Units" = "population_units",
                          "Method 2: Sample from Cluster List" = "cluster_list"
                        ),
                        selected = "population_units",
                        inline = TRUE
                      ),
                      p("Choose the approach for 2-stage cluster sampling", class = "text-muted small")
                    )
                  ),
                  div(
                    class = "row mb-2",
                    div(
                      class = "col-md-6",
                      numericInput(
                        ns("reserve_clusters"),
                        "Reserve Clusters",
                        value = 0,
                        min = 0,
                        step = 1,
                        width = "100%"
                      ),
                      p("Extra clusters, used in order when a selected cluster cannot be reached (e.g. insecure). Per stratum for stratified designs.", class = "text-muted small")
                    ),
                    div(
                      class = "col-md-6",
                      p(id = ns("reserve_clusters_note"), class = "text-muted small mt-md-4",
                        "Reserve clusters are drawn from the clusters not selected, by the same method (PPS or equal probability). ",
                        "With Method 1 each gets the same interviews per cluster and buffer as a selected cluster.")
                    )
                  ),
                  
                  # Method 1: Population Units Approach
                  div(
                    id = ns("population_units_method"),
                  div(
                    class = "row",
                    div(
                      class = "col-md-6",
                      selectInput(
                        ns("cluster_variable"),
                        "Cluster Variable",
                        choices = c("Select a variable..." = ""),
                        width = "100%"
                      ),
                      p("Select the variable that defines clusters", class = "text-muted small")
                    ),
                    div(
                      class = "col-md-6",
                      numericInput(
                        ns("cluster_size"),
                        "Interviews per Cluster (m)",
                        value = 10,
                        min = 1,
                        step = 1,
                        width = "100%"
                      ),
                        p("Number of units to sample from each selected cluster", class = "text-muted small")
                    )
                    ),
                    div(
                      class = "row mt-2",
                      div(
                        class = "col-md-12",
                        checkboxInput(
                          ns("use_pps"),
                          "Use PPS Sampling",
                          value = TRUE,
                          width = "100%"
                        ),
                        p("Probability Proportional to Size sampling - cluster population is calculated as the number of units in each cluster", class = "text-muted small")
                      )
                    )
                  ),
                  
                  # Method 2: Cluster List Approach
                  div(
                    id = ns("cluster_list_method"),
                    style = "display: none;",
                    div(
                      class = "row",
                      div(
                        class = "col-md-6",
                        selectInput(
                          ns("cluster_list_variable"),
                          "Cluster List Variable",
                          choices = c("Select a variable..." = ""),
                          width = "100%"
                        ),
                        p("Each row of the file is one cluster (e.g. a village). A name that repeats on another row gets its row number added.", class = "text-muted small")
                      ),
                      div(
                        class = "col-md-6",
                        selectInput(
                          ns("cluster_population_variable"),
                          "Cluster Population Variable",
                          choices = c("None (equal probability only)" = ""),
                          width = "100%"
                        ),
                        p("Size of each cluster in the units you interview (e.g. households). Needed for PPS and for the weights.", class = "text-muted small")
                      )
                    ),
                    div(
                      class = "row mt-2",
                      div(
                        class = "col-md-6",
                        numericInput(
                          ns("cluster_list_size"),
                          "Interviews per Cluster (m)",
                          value = 10,
                          min = 1,
                          step = 1,
                          width = "100%"
                        ),
                        p("Interviews in each selected cluster. The sample lists the clusters; units are chosen in the field, e.g. from a household listing.", class = "text-muted small")
                      ),
                      div(
                        class = "col-md-6",
                        checkboxInput(
                          ns("use_pps_cluster_list"),
                          "Use PPS Sampling",
                          value = TRUE,
                          width = "100%"
                        ),
                        p("Probability Proportional to Size sampling", class = "text-muted small")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Generate Sample Button
      div(
        class = "row mb-4",
        div(
          class = "col-12 text-center",
          actionButton(
            ns("generate_sample"),
            "Generate Sample",
            class = "btn btn-generate-sample btn-lg"
          )
        )
      ),
      
      # Results Section
      div(
        id = ns("results_section"),
        style = "display: none;",
        div(
          class = "row",
          div(
            class = "col-12",
            div(
              class = "card",
              div(
                class = "card-header",
                h5(id = ns("results_header"), "Sample Results", class = "mb-0")
              ),
              div(
                class = "card-body",
                
                # Sample Summary
                div(
                  class = "row mb-3",
                  div(
                    class = "col-md-2",
                    div(
                      class = "card bg-primary text-white",
                      div(
                        class = "card-body text-center",
                        h4(id = ns("sample_size_result"), class = "mb-0"),
                        p("Sample Size", class = "mb-0")
                      )
                    )
                  ),
                  div(
                    class = "col-md-2",
                    div(
                      class = "card bg-success text-white",
                      div(
                        class = "card-body text-center",
                        h4(id = ns("population_size_result"), class = "mb-0"),
                        p("Population Size", class = "mb-0")
                      )
                    )
                  ),
                  div(
                    class = "col-md-2",
                    div(
                      class = "card bg-info text-white",
                      div(
                        class = "card-body text-center",
                        h4(id = ns("sampling_fraction_result"), class = "mb-0"),
                        p("Sampling Fraction", class = "mb-0")
                      )
                    )
                  ),
                  div(
                    class = "col-md-3",
                    div(
                      class = "card bg-secondary text-white",
                      div(
                        class = "card-body text-center",
                        h4(id = ns("seed_result"), class = "mb-0"),
                        p("Random Seed", class = "mb-0")
                      )
                    )
                  ),
                  div(
                    class = "col-md-3",
                    div(
                      class = "card bg-dark text-white",
                      div(
                        class = "card-body text-center",
                        h4(id = ns("buffer_result"), class = "mb-0"),
                        p("Buffer", class = "mb-0")
                      )
                    )
                  )
                ),
                
                # Warnings and notes from the draw
                uiOutput(ns("draw_messages")),

                # Download Options
                div(
                  class = "row mb-4",
                  div(
                    class = "col-12 text-center",
                    h6("Export Sample", class = "mb-3"),
                    div(
                      class = "btn-group",
                      role = "group",
                      downloadButton(ns("download_sample_csv"), "📄 Download CSV", class = "btn btn-success"),
                      downloadButton(ns("download_sample_excel"), "📊 Download Excel", class = "btn btn-primary")
                    )
                  )
                ),
                
                # Sample Preview
                div(
                  class = "sample-results",
                  h6("Sample Preview", class = "mb-3"),
                  DT::dataTableOutput(ns("sample_preview"))
                ),
                
                # Cluster Summary Table (for cluster sampling methods)
                div(
                  id = ns("cluster_summary_section"),
                  class = "sample-results mt-4",
                  h6("Cluster Sampling Summary", class = "mb-3"),
                  
                  # Cluster Summary Download Buttons
                  div(
                    class = "mb-3",
                    downloadButton(ns("download_cluster_summary_csv"), "📄 Download CSV", class = "btn btn-outline-primary")
                  ),
                  
                  DT::dataTableOutput(ns("cluster_summary"), height = "400px")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Human-readable labels for the export
SAMPLING_METHOD_LABELS <- c(
  simple_random = "Simple random sampling",
  systematic = "Systematic sampling",
  stratified = "Stratified sampling",
  cluster = "Two-stage cluster sampling",
  stratified_cluster = "Stratified two-stage cluster sampling"
)

DESIGN_COLUMN_NOTES <- data.frame(
  Column = DESIGN_COLUMNS,
  Meaning = c(
    "Sample = main sample; Buffer = reserve unit; Reserve cluster = unit to interview in a reserve cluster; Reserve cluster buffer = reserve unit in a reserve cluster",
    "Order in which reserves are used (within the cluster for cluster designs, within the stratum for stratified designs)",
    "Reserve clusters (cluster designs): 1, 2, 3 ... in the order to use them when a selected cluster cannot be reached (per stratum for stratified designs); blank for selected clusters",
    "Stratum (stratified designs)",
    "Cluster (cluster designs)",
    "Times the cluster was selected (PPS with replacement can select a cluster more than once)",
    "Stage-1 (cluster) inclusion probability; for PPS with replacement, the expected number of selections (can exceed 1); 1 when there is no cluster stage",
    "Probability the unit is selected within its cluster or stratum, per selection of the cluster",
    "p_stage1 x p_stage2",
    "1 / p_incl. Buffer rows show the weight a replacement takes. Adjust for non-response before analysis."
  ),
  stringsAsFactors = FALSE
)

# From a cluster list, each sample row is a cluster
CLUSTER_LIST_COLUMN_NOTES <- local({
  notes <- DESIGN_COLUMN_NOTES
  set <- function(column, meaning) notes$Meaning[notes$Column == column] <<- meaning
  set("sample_type", "Sample = selected cluster; Reserve cluster = reserve cluster")
  set("reserve_order", "Not used: units are chosen in the field")
  set("p_stage2", "interviews / (selection_hits x cluster size): the chance a unit is interviewed, per selection of its cluster")
  set("base_weight", "1 / p_incl, the weight of each interview in the cluster. Adjust for non-response before analysis.")
  rbind(notes, data.frame(
    Column = "interviews",
    Meaning = "Interviews to do in the cluster: m per selection, or every unit when the cluster is smaller. Select them in the field, e.g. from a household listing."
  ))
})

#' Group Draw Warnings or Notes for Display
#'
#' The same message repeated in several strata ("Stratum X: 3 cluster(s) ...")
#' becomes one line with the counts added up, and long lists of clusters move
#' to the details, one line per stratum or per item.
#'
#' @param msgs Character vector of messages
#'
#' @return A data frame with `summary` and a list column `details`
group_draw_messages <- function(msgs) {
  msgs <- unique(trimws(msgs[!is.na(msgs) & nzchar(trimws(msgs))]))
  out <- data.frame(summary = character(0), stringsAsFactors = FALSE)
  out$details <- list()
  if (length(msgs) == 0) return(out)

  has_stratum <- grepl("^Stratum [^:]+: ", msgs)
  stratum <- ifelse(has_stratum, sub("^Stratum ([^:]+): .*$", "\\1", msgs), NA_character_)
  body <- sub("^Stratum [^:]+: ", "", msgs)
  # A list after the first colon (not a sentence) is set apart from the message,
  # so the same message with different clusters groups together
  pos <- regexpr(": ", body, fixed = TRUE)
  tail <- ifelse(pos > 0, substring(body, pos + 2), "")
  is_list <- nzchar(tail) & !grepl("[[:alpha:])][.]$", tail)
  head <- ifelse(is_list, substr(body, 1, pos - 1), body)
  tail[!is_list] <- ""
  key <- gsub("[0-9]+", "#", head)

  split_items <- function(x) {
    if (grepl("; ", x, fixed = TRUE)) strsplit(x, "; ", fixed = TRUE)[[1]]
    else if (!grepl("(", x, fixed = TRUE)) strsplit(x, ", ", fixed = TRUE)[[1]]
    else x
  }

  for (k in unique(key)) {
    i <- which(key == k)
    if (length(i) == 1) {
      # A short list stays in the message; a long one goes to the details
      short <- nchar(tail[i]) <= 60
      summary <- paste0(if (has_stratum[i]) paste0("Stratum ", stratum[i], ": "), head[i],
                        if (nzchar(tail[i]) && short) paste0(": ", tail[i]))
      details <- if (nzchar(tail[i]) && !short) split_items(tail[i]) else character(0)
    } else {
      summary <- head[i[1]]
      if (all(grepl("^[0-9]+ ", head[i]))) {
        summary <- sub("^[0-9]+", sum(as.numeric(sub("^([0-9]+) .*$", "\\1", head[i]))), summary)
      }
      strata <- sum(has_stratum[i])
      if (strata > 0) summary <- paste0(summary, " (in ", strata, " strata)")
      details <- paste0(ifelse(has_stratum[i], paste0("Stratum ", stratum[i], ": "), ""), ifelse(nzchar(tail[i]), tail[i], head[i]))
    }
    out[nrow(out) + 1, "summary"] <- summary
    out$details[[nrow(out)]] <- details
  }
  out
}

REPLACEMENT_REASONS <-c("Insecure / inaccessible", "Refused", "Not found / moved", "Other")

# App version from the VERSION file, plus the git commit when available and
# commit = TRUE (used in exports; the footer shows the version only)
app_version <- function(commit = TRUE) {
  version <- if (file.exists("VERSION")) trimws(readLines("VERSION", warn = FALSE)[1]) else "unknown"
  if (!commit) return(version)
  hash <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (length(hash) == 1 && nzchar(hash) && is.null(attr(hash, "status"))) {
    paste0(version, " (commit ", hash, ")")
  } else {
    version
  }
}

# Write the sample workbook: data sheets plus a replacement log with a reason dropdown
write_sample_workbook <- function(path, sheets, replacement_log) {
  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#F2F2F2", border = "bottom")

  for (name in names(sheets)) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, sheets[[name]], headerStyle = header)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(ncol(sheets[[name]])), widths = "auto")
  }

  openxlsx::addWorksheet(wb, "Replacement Log")
  openxlsx::writeData(wb, "Replacement Log", replacement_log, headerStyle = header)
  openxlsx::freezePane(wb, "Replacement Log", firstRow = TRUE)
  openxlsx::setColWidths(wb, "Replacement Log", cols = seq_len(ncol(replacement_log)), widths = 22)

  openxlsx::addWorksheet(wb, "Lists", visible = FALSE)
  openxlsx::writeData(wb, "Lists", data.frame(reason = REPLACEMENT_REASONS))
  reason_col <- match("reason", names(replacement_log))
  openxlsx::dataValidation(
    wb, "Replacement Log", cols = reason_col, rows = 2:1000, type = "list",
    value = paste0("'Lists'!$A$2:$A$", length(REPLACEMENT_REASONS) + 1)
  )

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

# Empty replacement log, one column per item to record when a reserve replaces a unit
empty_replacement_log <- function() {
  data.frame(
    original_unit_id = character(0),
    stratum = character(0),
    cluster_id = character(0),
    replacement_unit_id = character(0),
    reserve_order_used = integer(0),
    reason = character(0),
    reason_details = character(0),
    date_replaced = character(0),
    enumerator = character(0),
    supervisor_approval = character(0),
    stringsAsFactors = FALSE
  )
}

# Sampling Generator Server
sampling_generator_server <- function(id, study_name = NULL) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    uploaded_data <- reactiveVal(NULL)
    generated_sample <- reactiveVal(NULL)
    cluster_summary <- reactiveVal(NULL)
    allocation_result <- reactiveVal(NULL)
    methodology <- reactiveVal(NULL)
    frame_check_result <- reactiveVal(NULL)
    pending_spec <- reactiveVal(NULL)
    allocation_inputs <- reactiveVal(NULL)
    draw_messages <- reactiveVal(NULL)

    # Warnings and notes from the last draw, grouped, in one box
    output$draw_messages <- renderUI({
      m <- draw_messages()
      req(m)
      block <- function(msgs, class, title) {
        g <- group_draw_messages(msgs)
        if (nrow(g) == 0) return(NULL)
        div(
          class = paste("alert draw-messages", class),
          tags$strong(title(nrow(g))),
          tags$ul(class = "mb-0 mt-1", lapply(seq_len(nrow(g)), function(i) {
            details <- g$details[[i]]
            tags$li(
              g$summary[i],
              if (length(details)) tags$details(
                tags$summary(paste0("Show ", length(details), if (length(details) == 1) " detail" else " details")),
                tags$ul(lapply(details, tags$li))
              )
            )
          }))
        )
      }
      tagList(
        block(m$warnings, "alert-warning", function(n) paste0(n, " warning", if (n != 1) "s", " from the draw")),
        block(m$notes, "alert-secondary", function(n) if (n == 1) "Note" else "Notes")
      )
    })

    # Study Name synchronization with the other tabs
    if (!is.null(study_name)) {
      observeEvent(study_name(), {
        if (!identical(study_name(), input$study_name)) {
          updateTextInput(session, "study_name", value = study_name())
        }
      })
      observeEvent(input$study_name, {
        if (!identical(input$study_name, study_name())) {
          study_name(input$study_name)
        }
      })
    }

    # Variable names modal
    observeEvent(input$show_variable_names, {
      req(uploaded_data())

      data <- uploaded_data()
      variable_names <- names(data)

      # Create organized variable names list
      organized_names <- lapply(seq_along(variable_names), function(i) {
        var_name <- variable_names[i]
        var_type <- class(data[[var_name]])

        # Get sample values for preview
        sample_values <- if (is.numeric(data[[var_name]])) {
          paste("Range:", round(min(data[[var_name]], na.rm = TRUE), 2), "to",
                round(max(data[[var_name]], na.rm = TRUE), 2))
        } else {
          unique_vals <- unique(data[[var_name]])
          if (length(unique_vals) <= 5) {
            paste("Values:", paste(unique_vals, collapse = ", "))
          } else {
            paste("Values:", paste(unique_vals[1:3], collapse = ", "), "... (+",
                  length(unique_vals) - 3, " more)")
          }
        }

        div(
          class = "variable-item",
          div(
            class = "d-flex justify-content-between align-items-start",
            div(
              div(
                tags$strong(var_name),
                tags$span(paste("(", var_type, ")"), class = "text-muted ml-2")
              ),
              div(
                tags$small(sample_values, class = "text-info mt-1")
              )
            ),
            div(
              tags$small(paste("Column", i), class = "text-muted")
            )
          )
        )
      })

      showModal(modalDialog(
        title = "📋 Dataset Variables",
        div(
          class = "variable-names-list",
          organized_names
        ),
        size = "m",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Handle file upload
    observeEvent(input$dataset_file, {
      req(input$dataset_file)

      tryCatch({
        file_path <- input$dataset_file$datapath
        file_ext <- tolower(tools::file_ext(input$dataset_file$name))

        if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else if (file_ext == "csv") {
          data <- readr::read_csv(file_path, show_col_types = FALSE)
        } else {
          stop("Unsupported file format")
        }
        data <- as.data.frame(data)

        uploaded_data(data)
        generated_sample(NULL)
        cluster_summary(NULL)
        draw_messages(NULL)
        shinyjs::hide("results_section")

        shinyjs::runjs(paste0('
          document.getElementById("', session$ns("file_name_display"), '").textContent = "File: ', gsub('"', "'", input$dataset_file$name), '";
          document.getElementById("', session$ns("file_dimensions_display"), '").textContent = "Dimensions: ', nrow(data), ' rows × ', ncol(data), ' columns";
          document.getElementById("', session$ns("file_info_display"), '").style.display = "block";
        '))

        vars <- names(data)
        updateSelectInput(session, "id_variable", choices = c("None" = "", vars))
        updateSelectInput(session, "strata_variable", choices = c("Select a variable..." = "", vars))
        updateSelectInput(session, "cluster_variable", choices = c("Select a variable..." = "", vars))
        updateSelectInput(session, "cluster_list_variable", choices = c("Select a variable..." = "", vars))
        updateSelectInput(session, "cluster_population_variable", choices = c("None (equal probability only)" = "", vars))
        updateSelectInput(session, "systematic_sort_variable", choices = c("Keep file order" = "", vars))

        showNotification("Dataset uploaded successfully!", type = "message")

      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        uploaded_data(NULL)
      })
    })

    # Show/hide conditional inputs based on sampling method and options
    observe({
      method <- input$sampling_method
      req(method)
      # Simple random sampling has no extra settings, so the panel would be empty
      shinyjs::toggle("conditional_inputs", condition = method != "simple_random")
      shinyjs::toggle("strata_inputs", condition = method %in% c("stratified", "stratified_cluster"))
      shinyjs::toggle("cluster_inputs", condition = method %in% c("cluster", "stratified_cluster"))
      shinyjs::toggle("within_method_inputs", condition = method == "stratified")
      shinyjs::toggle("systematic_inputs", condition = method == "systematic" ||
                        (method == "stratified" && identical(input$within_method, "systematic")))
      shinyjs::toggle("neyman_inputs", condition = identical(input$allocation_method, "neyman"))
    })

    # Show/hide cluster method sections based on selected method
    observeEvent(input$cluster_sampling_method, {
      shinyjs::toggle("population_units_method", condition = input$cluster_sampling_method == "population_units")
      shinyjs::toggle("cluster_list_method", condition = input$cluster_sampling_method == "cluster_list")
    })

    # From a cluster list there is no unit buffer: reserves are whole clusters
    observe({
      list_mode <- input$sampling_method %in% c("cluster", "stratified_cluster") &&
        identical(input$cluster_sampling_method, "cluster_list")
      shinyjs::toggleState("buffer_percentage", condition = !list_mode)
    })

    # Per-stratum allocation inputs (Neyman values or manual sizes), reset when the strata change
    observeEvent(list(uploaded_data(), input$strata_variable), {
      data <- uploaded_data()
      if (is.null(data) || is.null(input$strata_variable) || input$strata_variable == "") {
        allocation_inputs(NULL)
        return()
      }
      strata <- as.character(data[[input$strata_variable]])
      counts <- table(strata[!is_blank(strata)])
      allocation_inputs(data.frame(stratum = names(counts), input = NA_real_, stringsAsFactors = FALSE))
    })

    observeEvent(input$allocation_table_cell_edit, {
      edit <- input$allocation_table_cell_edit
      current <- allocation_inputs()
      req(current, input$allocation_method %in% c("neyman", "manual"))
      # Column index is 0-based without row names
      if (edit$col == allocation_preview()$input_col) {
        current$input[edit$row] <- suppressWarnings(as.numeric(edit$value))
        allocation_inputs(current)
      }
    })

    # Build the design specification from the inputs
    build_spec <- function() {
      method <- input$sampling_method
      seed <- input$random_seed
      if (is.null(seed) || is.na(seed)) {
        seed <- sample.int(999999, 1)
        updateNumericInput(session, "random_seed", value = seed)
        showNotification(paste("No seed entered; using random seed", seed), type = "message")
      }

      spec <- list(
        method = method,
        sample_size = input$sample_size,
        buffer_pct = if (is.na(input$buffer_percentage)) 0 else input$buffer_percentage,
        seed = seed,
        id_var = input$id_variable,
        sort_var = input$systematic_sort_variable
      )

      if (method %in% c("stratified", "stratified_cluster")) {
        inputs <- allocation_inputs()
        spec$strata_var <- input$strata_variable
        spec$allocation <- input$allocation_method
        spec$min_per_stratum <- if (is.na(input$min_per_stratum)) 0 else input$min_per_stratum
        spec$neyman_input <- input$neyman_input
        spec$within <- if (method == "stratified") input$within_method else "simple_random"
        if (!is.null(inputs)) spec$allocation_values <- setNames(inputs$input, inputs$stratum)
        if (method == "stratified" && spec$within != "systematic") spec$sort_var <- NULL
      } else if (method != "systematic") {
        spec$sort_var <- NULL
      }

      if (method %in% c("cluster", "stratified_cluster")) {
        if (input$cluster_sampling_method == "population_units") {
          spec$cluster_var <- input$cluster_variable
          spec$pop_var <- NULL
          spec$n_units <- input$cluster_size
          spec$use_pps <- isTRUE(input$use_pps)
        } else {
          spec$cluster_var <- input$cluster_list_variable
          spec$pop_var <- if (isTRUE(nzchar(input$cluster_population_variable))) input$cluster_population_variable else NULL
          spec$n_units <- input$cluster_list_size
          spec$use_pps <- isTRUE(input$use_pps_cluster_list)
        }
        spec$cluster_method <- input$cluster_sampling_method
        spec$reserve_clusters <- if (is.null(input$reserve_clusters) || is.na(input$reserve_clusters)) 0 else input$reserve_clusters
        # From a cluster list, units are chosen in the field, so there is no unit buffer
        if (spec$cluster_method == "cluster_list") spec$buffer_pct <- 0
      }
      spec
    }

    # Problems with the inputs themselves (before looking at the frame)
    validate_spec <- function(spec) {
      if (is.null(spec$sample_size) || is.na(spec$sample_size) || spec$sample_size <= 0) {
        return("Enter a sample size above zero.")
      }
      if (spec$method %in% c("stratified", "stratified_cluster") && (is.null(spec$strata_var) || spec$strata_var == "")) {
        return("Please select a strata variable!")
      }
      if (spec$method %in% c("cluster", "stratified_cluster")) {
        if (is.null(spec$cluster_var) || spec$cluster_var == "") return("Please select a cluster variable!")
        if (spec$cluster_method == "cluster_list" && isTRUE(spec$use_pps) && is.null(spec$pop_var)) {
          return("PPS needs the cluster population variable. Select it, or untick PPS to select clusters with equal probability.")
        }
        if (is.null(spec$n_units) || is.na(spec$n_units) || spec$n_units <= 0) {
          return("Interviews per cluster must be a positive number!")
        }
        if (spec$reserve_clusters < 0 || spec$reserve_clusters != round(spec$reserve_clusters)) {
          return("Reserve clusters must be a whole number, 0 or more.")
        }
      }
      NULL
    }

    # Live allocation preview for stratified designs
    allocation_preview <- reactive({
      data <- uploaded_data()
      req(data, input$sampling_method %in% c("stratified", "stratified_cluster"), input$strata_variable != "")
      spec <- list(
        method = input$sampling_method,
        sample_size = input$sample_size,
        buffer_pct = if (is.na(input$buffer_percentage)) 0 else input$buffer_percentage,
        seed = if (is.na(input$random_seed)) 1 else input$random_seed,
        strata_var = input$strata_variable,
        allocation = input$allocation_method,
        min_per_stratum = if (is.na(input$min_per_stratum)) 0 else input$min_per_stratum,
        neyman_input = input$neyman_input,
        n_units = if (input$cluster_sampling_method == "population_units") input$cluster_size else input$cluster_list_size,
        cluster_var = if (input$cluster_sampling_method == "population_units") input$cluster_variable else input$cluster_list_variable,
        cluster_method = input$cluster_sampling_method,
        pop_var = if (input$cluster_sampling_method == "cluster_list" && isTRUE(nzchar(input$cluster_population_variable))) input$cluster_population_variable
      )
      inputs <- allocation_inputs()
      if (!is.null(inputs)) spec$allocation_values <- setNames(inputs$input, inputs$stratum)
      if (spec$method == "stratified_cluster" && (is.null(spec$cluster_var) || spec$cluster_var == "")) {
        spec$method <- "stratified"
      }

      # From a cluster list, strata are counted in clusters and their total size
      list_mode <- is_cluster_list(spec)
      strata <- as.character(data[[spec$strata_var]])
      counts <- table(strata[!is_blank(strata)])
      preview <- data.frame(stratum = names(counts), frame_units = as.integer(counts), stringsAsFactors = FALSE)
      labels <- c("Stratum", if (list_mode) "Clusters" else "Frame units")
      if (list_mode && !is.null(spec$pop_var)) {
        size <- suppressWarnings(as.numeric(data[[spec$pop_var]]))
        size[is.na(size) | size < 0] <- 0
        preview$frame_population <- as.numeric(tapply(size[!is_blank(strata)], strata[!is_blank(strata)], sum)[preview$stratum])
        labels <- c(labels, "Total size")
      }
      input_col <- ncol(preview)
      preview$input <- if (!is.null(inputs)) inputs$input[match(preview$stratum, inputs$stratum)] else NA_real_
      labels <- c(labels, "Input", if (list_mode) "Allocated interviews" else "Allocated n", "Sampling fraction (%)",
                  if (list_mode) "Clusters to select")

      result <- tryCatch({
        alloc <- compute_allocation(data, spec)
        at <- match(preview$stratum, alloc$stratum)
        preview$n_allocated <- alloc$n_allocated[at]
        preview$sampling_fraction_pct <- if (is.null(alloc$sampling_fraction_pct)) NA_real_ else alloc$sampling_fraction_pct[at]
        if (list_mode) preview$clusters_to_select <- alloc$clusters_to_select[at]
        NULL
      }, error = function(e) {
        preview$n_allocated <<- NA_integer_
        preview$sampling_fraction_pct <<- NA_real_
        if (list_mode) preview$clusters_to_select <<- NA_integer_
        conditionMessage(e)
      })
      list(table = preview, labels = labels, input_col = input_col, message = result)
    })

    output$allocation_table <- DT::renderDataTable({
      preview <- allocation_preview()
      editable_input <- input$allocation_method %in% c("neyman", "manual")
      input_label <- switch(input$allocation_method,
        neyman = if (identical(input$neyman_input, "sd")) "Input: SD" else "Input: expected proportion",
        manual = "Input: sample size",
        "Input (not used)"
      )
      labels <- preview$labels
      labels[labels == "Input"] <- input_label
      DT::datatable(
        preview$table,
        colnames = labels,
        rownames = FALSE,
        selection = "none",
        editable = if (editable_input) list(target = "cell", disable = list(columns = setdiff(seq_along(labels) - 1, preview$input_col))) else FALSE,
        caption = if (!is.null(preview$message)) htmltools::tags$caption(style = "color:#b00020;", preview$message),
        options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
      )
    })

    # Generate sample: check the frame first, then ask for confirmation
    observeEvent(input$generate_sample, {
      req(uploaded_data(), input$sample_size)
      spec <- build_spec()
      problem <- validate_spec(spec)
      if (!is.null(problem)) {
        showNotification(problem, type = "error")
        return()
      }

      check <- tryCatch(check_frame(uploaded_data(), spec), error = function(e) {
        showNotification(paste("Frame check failed:", conditionMessage(e)), type = "error")
        NULL
      })
      req(check)
      frame_check_result(check)
      pending_spec(spec)

      level_icon <- c(ok = "✅", info = "ℹ️", warning = "⚠️", error = "❌")
      issue_rows <- lapply(seq_len(nrow(check$issues)), function(i) {
        tags$tr(
          tags$td(level_icon[[check$issues$level[i]]]),
          tags$td(tags$strong(check$issues$check[i])),
          tags$td(check$issues$detail[i])
        )
      })

      showModal(modalDialog(
        title = "Frame Checks",
        tags$table(class = "table table-sm", tags$tbody(issue_rows)),
        if (nrow(check$exclusions) > 0) tagList(
          h6("Rows excluded from the frame"),
          tags$ul(lapply(seq_len(nrow(check$exclusions)), function(i) {
            tags$li(paste0(check$exclusions$reason[i], ": ", check$exclusions$rows[i], " row(s)"))
          }))
        ),
        if (!is.null(check$allocation)) {
          alloc <- check$allocation[, setdiff(names(check$allocation), "seed")]
          tagList(
            h6("Allocation by stratum"),
            div(
              style = "max-height: 300px; overflow-y: auto;",
              tags$table(
                class = "table table-sm table-striped",
                tags$thead(tags$tr(lapply(names(alloc), tags$th))),
                tags$tbody(lapply(seq_len(nrow(alloc)), function(i) {
                  tags$tr(lapply(alloc[i, ], function(v) tags$td(as.character(v))))
                }))
              )
            )
          )
        },
        if (!check$can_proceed) div(class = "alert alert-danger", "Fix the errors above before drawing the sample."),
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          if (check$can_proceed) actionButton(session$ns("confirm_draw"), "Confirm and Draw Sample", class = "btn btn-generate-sample")
        )
      ))
    })

    # Draw the sample after the user confirms the frame checks
    observeEvent(input$confirm_draw, {
      removeModal()
      spec <- pending_spec()
      data <- uploaded_data()
      req(spec, data)

      sampling_warnings <- character(0)
      sampling_notes <- character(0)

      tryCatch({
        result <- withCallingHandlers(
          generate_sample_design(data, spec),
          warning = function(w) {
            sampling_warnings <<- c(sampling_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          },
          message = function(m) {
            sampling_notes <<- c(sampling_notes, trimws(conditionMessage(m)))
            invokeRestart("muffleMessage")
          }
        )
        sample_data <- result$sample

        if (nrow(sample_data) == 0) {
          showNotification("No units were sampled. Please check your data and parameters.", type = "error")
          return()
        }

        generated_sample(sample_data)
        allocation_result(result$allocation)

        is_main <- sample_data$sample_type == "Sample"
        main_count <- sum(is_main)
        buffer_count <- sum(sample_data$sample_type == "Buffer")
        reserve_cluster_count <- length(unique(paste(sample_data$stratum, sample_data$cluster_id)[!is.na(sample_data$cluster_reserve_order)]))
        reserve_text <- if (reserve_cluster_count > 0) paste0(reserve_cluster_count, " reserve cluster", if (reserve_cluster_count != 1) "s") else ""
        is_cluster <- spec$method %in% c("cluster", "stratified_cluster")
        # From a cluster list each row is a cluster: count interviews, and the
        # population is the clusters' total size
        cluster_list <- is_cluster_list(spec)
        population_text <- format(nrow(data), big.mark = ",")
        if (cluster_list) {
          main_count <- sum(sample_data$interviews[is_main])
          named <- !is.na(cluster_list_ids(data, spec$cluster_var))
          size <- if (!is.null(spec$pop_var)) suppressWarnings(as.numeric(data[[spec$pop_var]])) else rep(NA_real_, nrow(data))
          total <- sum(size[named & size > 0], na.rm = TRUE)
          population_text <- if (total > 0) format(round(total), big.mark = ",") else paste(format(sum(named), big.mark = ","), "clusters")
          fraction_text <- if (total > 0) paste0(round(main_count / total * 100, 2), "%") else "-"
          buffer_text <- if (nzchar(reserve_text)) reserve_text else "0 reserve clusters"
          sample_text <- paste0(main_count, " (", sum(is_main), " clusters)")
        } else {
          fraction_text <- paste0(round(main_count / nrow(data) * 100, 2), "%")
          buffer_text <- paste0(buffer_count, " (", round(buffer_count / main_count * 100, 1), "%)",
                                if (nzchar(reserve_text)) paste0(" + ", reserve_text))
          sample_text <- as.character(main_count)
        }

        if (is_cluster) {
          cluster_summary(summarise_clusters(data, sample_data, spec))
          shinyjs::show("cluster_summary_section")
        } else {
          cluster_summary(NULL)
          shinyjs::hide("cluster_summary_section")
        }

        expected <- if (!is.null(result$allocation)) sum(result$allocation$n_allocated) else spec$sample_size
        if (main_count != expected) {
          sampling_warnings <- c(
            paste0("Main sample has ", main_count, if (cluster_list) " interviews" else " units", " (requested ", expected, ")",
                   if (is_cluster) ". Cluster samples are whole multiples of the interviews per cluster, and small clusters can fall short." else "."),
            sampling_warnings
          )
        }
        draw_messages(list(notes = sampling_notes, warnings = sampling_warnings))

        methodology(build_methodology(spec, data, sample_data, frame_check_result(), sampling_notes, sampling_warnings))

        shinyjs::show("results_section")

        shinyjs::runjs(paste0('
          document.getElementById("', session$ns("results_header"), '").textContent = "Sample Results - ', SAMPLING_METHOD_LABELS[[spec$method]], '";
          document.getElementById("', session$ns("sample_size_result"), '").textContent = "', sample_text, '";
          document.getElementById("', session$ns("population_size_result"), '").textContent = "', population_text, '";
          document.getElementById("', session$ns("sampling_fraction_result"), '").textContent = "', fraction_text, '";
          document.getElementById("', session$ns("seed_result"), '").textContent = "', spec$seed, '";
          document.getElementById("', session$ns("buffer_result"), '").textContent = "', buffer_text, '";
        '))

        # One short notice; the details are in the box under the result cards
        n_warn <- nrow(group_draw_messages(sampling_warnings))
        if (n_warn > 0) {
          showNotification(paste0("Sample generated with ", n_warn, " warning", if (n_warn != 1) "s",
                                  ". See the box under the results."), type = "warning", duration = 8)
        } else {
          showNotification("Sample generated successfully!", type = "message")
        }

      }, error = function(e) {
        showNotification(paste("Error generating sample:", e$message), type = "error", duration = NULL)
      })
    })

    # Methodology sheet: how the sample was drawn, for the client and for re-running the draw
    build_methodology <- function(spec, data, sample_data, check, notes, warnings) {
      or_blank <- function(x) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) "" else paste(as.character(x), collapse = ", ")
      main <- sample_data$sample_type == "Sample"
      is_strat <- spec$method %in% c("stratified", "stratified_cluster")
      is_cluster <- spec$method %in% c("cluster", "stratified_cluster")
      cluster_list <- is_cluster_list(spec)
      # From a cluster list each row is a cluster with its number of interviews
      units <- if (cluster_list) sample_data$interviews else rep(1, nrow(sample_data))

      rows <- list(
        c("Study name", or_blank(input$study_name)),
        c("Sampling method", SAMPLING_METHOD_LABELS[[spec$method]]),
        c("Frame", if (cluster_list) "Cluster list (one row per cluster); units are selected within each cluster in the field"
                   else if (is_cluster) "Unit list (one row per unit)" else ""),
        c("Selection within strata", if (spec$method == "stratified") or_blank(spec$within) else ""),
        c("Strata variable", if (is_strat) or_blank(spec$strata_var) else ""),
        c("Allocation", if (is_strat) or_blank(spec$allocation) else ""),
        c("Neyman input", if (is_strat && identical(spec$allocation, "neyman")) or_blank(spec$neyman_input) else ""),
        c("Minimum per stratum", if (is_strat) or_blank(spec$min_per_stratum) else ""),
        c("Cluster variable", if (is_cluster) or_blank(spec$cluster_var) else ""),
        c("Cluster population variable", if (is_cluster) or_blank(spec$pop_var) else ""),
        c("Cluster selection", if (is_cluster) (if (isTRUE(spec$use_pps)) "PPS with replacement" else "Equal probability without replacement") else ""),
        c("Interviews per cluster (m)", if (is_cluster) or_blank(spec$n_units) else ""),
        c("Reserve clusters requested", if (is_cluster) paste0(or_blank(spec$reserve_clusters), if (is_strat) " per stratum") else ""),
        c("Frame sorted by", or_blank(spec$sort_var)),
        c("Unit ID variable", or_blank(spec$id_var)),
        c("Requested sample size", or_blank(spec$sample_size)),
        c("Buffer (%)", or_blank(spec$buffer_pct)),
        c("Master random seed", or_blank(spec$seed)),
        c("Frame file", or_blank(input$dataset_file$name)),
        c("Frame rows", or_blank(nrow(data))),
        c("Frame rows excluded", or_blank(sum(check$exclusions$rows)))
      )
      for (i in seq_len(nrow(check$exclusions))) {
        rows[[length(rows) + 1]] <- c(paste0("  Excluded: ", check$exclusions$reason[i]), or_blank(check$exclusions$rows[i]))
      }
      weight_sum <- sum(sample_data$base_weight[main] * units[main])
      in_reserve <- !is.na(sample_data$cluster_reserve_order)
      rows <- c(rows, list(
        c("Clusters selected", if (cluster_list) or_blank(sum(main)) else ""),
        c("Main sample drawn", or_blank(sum(units[main]))),
        c("Buffer units drawn", if (cluster_list) "" else or_blank(sum(sample_data$sample_type == "Buffer"))),
        c("Reserve clusters drawn", if (is_cluster) or_blank(length(unique(paste(sample_data$stratum, sample_data$cluster_id)[in_reserve]))) else ""),
        c("Units drawn in reserve clusters", if (is_cluster && !cluster_list) or_blank(sum(in_reserve)) else ""),
        c("Sum of base weights (main sample)", if (is.na(weight_sum)) "Not available until the selected clusters are listed" else or_blank(round(weight_sum, 1))),
        c("Generated at", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        c("App version", app_version()),
        c("R version", R.version.string)
      ))
      for (n in notes) rows[[length(rows) + 1]] <- c("Note", n)
      for (w in warnings) rows[[length(rows) + 1]] <- c("Warning", w)

      out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(out) <- c("Item", "Value")
      out[out$Value != "" | out$Item %in% c("Study name"), ]
    }

    # Sample preview table
    output$sample_preview <- DT::renderDataTable({
      req(generated_sample())

      DT::datatable(
        generated_sample(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'rtip',
          lengthMenu = c(5, 10, 25, 50, 100),
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          paging = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = 'sample-preview-table'
      ) %>%
        DT::formatStyle(1, fontWeight = 'bold') %>%
        DT::formatSignif(c("p_stage1", "p_stage2", "p_incl"), 4) %>%
        DT::formatRound("base_weight", 2)
    })

    # Cluster summary table
    output$cluster_summary <- DT::renderDataTable({
      req(cluster_summary())

      DT::datatable(
        cluster_summary(),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'rtip',
          lengthMenu = c(10, 15, 25, 50),
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          paging = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = "cluster-summary-table"
      )
    })

    # Download handlers
    output$download_sample_csv <- downloadHandler(
      filename = function() {
        paste0("sample_", input$sampling_method, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(generated_sample(), file, row.names = FALSE, na = "")
      }
    )

    output$download_sample_excel <- downloadHandler(
      filename = function() {
        paste0("sample_", input$sampling_method, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sheets <- list("Sample" = generated_sample())
        if (!is.null(methodology())) sheets[["Methodology"]] <- methodology()
        if (!is.null(allocation_result())) sheets[["Allocation"]] <- allocation_result()
        if (!is.null(cluster_summary()) && nrow(cluster_summary()) > 0) sheets[["Cluster Summary"]] <- cluster_summary()
        if (!is.null(frame_check_result())) sheets[["Frame Checks"]] <- frame_check_result()$issues
        sheets[["Column Guide"]] <- if ("interviews" %in% names(generated_sample())) CLUSTER_LIST_COLUMN_NOTES else DESIGN_COLUMN_NOTES

        write_sample_workbook(file, sheets, empty_replacement_log())
      }
    )

    # Cluster summary CSV download handler (keep separate CSV option)
    output$download_cluster_summary_csv <- downloadHandler(
      filename = function() {
        paste0("cluster_summary_", input$sampling_method, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(cluster_summary(), file, row.names = FALSE)
      }
    )

    # Settings sent from the Decision Tree
    handover <- reactiveVal(NULL)
    output$handover_note <- renderUI(handover_note_ui(handover(), session$ns("dismiss_handover")))
    observeEvent(input$dismiss_handover, handover(NULL))

    list(apply_handover = function(settings, design_name) {
      clustered <- settings$method %in% c("cluster", "stratified_cluster")
      updateSelectInput(session, "sampling_method", selected = settings$method)
      if (clustered) {
        updateRadioButtons(session, "cluster_sampling_method",
                           selected = if (settings$cluster_list) "cluster_list" else "population_units")
        updateCheckboxInput(session, if (settings$cluster_list) "use_pps_cluster_list" else "use_pps", value = settings$pps)
      }
      handover(list(design = design_name, action = "then upload the frame and generate the sample", details = c(
        paste("method:", SAMPLING_METHOD_LABELS[[settings$method]]),
        if (clustered) paste0(if (settings$cluster_list) "sample from a list of villages" else "sample villages from the unit list",
                              if (settings$pps) ", with PPS" else ", with equal probability"),
        if (length(settings$strata)) paste("after uploading, choose the strata column for:", paste(settings$strata, collapse = " and "))
      )))
    })
  })
}
