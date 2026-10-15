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
                    "Allocation preview. For Neyman or manual allocation, double-click a cell in the Input column to edit it."),
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
                        p("Variable containing cluster identifiers", class = "text-muted small")
                      ),
                      div(
                        class = "col-md-6",
                        selectInput(
                          ns("cluster_population_variable"),
                          "Cluster Population Variable",
                          choices = c("Select a variable..." = ""),
                          width = "100%"
                        ),
                        p("Population size of each cluster. If the value repeats on every row of a cluster it is used once; if it varies, it is summed.", class = "text-muted small")
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
                        p("Number of units to sample from each selected cluster", class = "text-muted small")
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
                  DT::dataTableOutput(ns("sample_preview"), height = "400px")
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
    "Sample = main sample; Buffer = reserve unit",
    "Order in which reserves are used (within the cluster for cluster designs, within the stratum for stratified designs)",
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

REPLACEMENT_REASONS <- c("Insecure / inaccessible", "Refused", "Not found / moved", "Other")

# App version from the VERSION file, plus the git commit when available
app_version <- function() {
  version <- if (file.exists("VERSION")) trimws(readLines("VERSION", warn = FALSE)[1]) else "unknown"
  commit <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (length(commit) == 1 && nzchar(commit) && is.null(attr(commit, "status"))) {
    paste0(version, " (commit ", commit, ")")
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
        updateSelectInput(session, "cluster_population_variable", choices = c("Select a variable..." = "", vars))
        updateSelectInput(session, "systematic_sort_variable", choices = c("Keep file order" = "", vars))

        updateNumericInput(session, "sample_size", max = nrow(data))

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
      shinyjs::show("conditional_inputs")
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
      # Column index is 0-based without row names; the Input column is the third
      if (edit$col == 2) {
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
          spec$pop_var <- input$cluster_population_variable
          spec$n_units <- input$cluster_list_size
          spec$use_pps <- isTRUE(input$use_pps_cluster_list)
        }
        spec$cluster_method <- input$cluster_sampling_method
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
        if (spec$cluster_method == "cluster_list" && (is.null(spec$pop_var) || spec$pop_var == "")) {
          return("Please select both cluster list and population variables!")
        }
        if (is.null(spec$n_units) || is.na(spec$n_units) || spec$n_units <= 0) {
          return("Interviews per cluster must be a positive number!")
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
        cluster_var = if (input$cluster_sampling_method == "population_units") input$cluster_variable else input$cluster_list_variable
      )
      inputs <- allocation_inputs()
      if (!is.null(inputs)) spec$allocation_values <- setNames(inputs$input, inputs$stratum)

      strata <- as.character(data[[spec$strata_var]])
      counts <- table(strata[!is_blank(strata)])
      preview <- data.frame(stratum = names(counts), frame_units = as.integer(counts), stringsAsFactors = FALSE)
      preview$input <- if (!is.null(inputs)) inputs$input[match(preview$stratum, inputs$stratum)] else NA_real_

      result <- tryCatch({
        if (spec$method == "stratified_cluster" && (is.null(spec$cluster_var) || spec$cluster_var == "")) {
          spec$method <- "stratified"
        }
        alloc <- compute_allocation(data, spec)
        preview$n_allocated <- alloc$n_allocated[match(preview$stratum, alloc$stratum)]
        preview$sampling_fraction_pct <- alloc$sampling_fraction_pct[match(preview$stratum, alloc$stratum)]
        list(table = preview, message = NULL)
      }, error = function(e) {
        preview$n_allocated <- NA_integer_
        list(table = preview, message = conditionMessage(e))
      })
      result
    })

    output$allocation_table <- DT::renderDataTable({
      preview <- allocation_preview()
      editable_input <- input$allocation_method %in% c("neyman", "manual")
      input_label <- switch(input$allocation_method,
        neyman = if (identical(input$neyman_input, "sd")) "Input: SD" else "Input: expected proportion",
        manual = "Input: sample size",
        "Input (not used)"
      )
      DT::datatable(
        preview$table,
        colnames = c("Stratum", "Frame units", input_label, "Allocated n", "Sampling fraction (%)")[seq_len(ncol(preview$table))],
        rownames = FALSE,
        selection = "none",
        editable = if (editable_input) list(target = "cell", disable = list(columns = c(0, 1, 3, 4))) else FALSE,
        caption = if (!is.null(preview$message)) htmltools::tags$caption(style = "color:#b00020;", preview$message),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
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

        main_count <- sum(sample_data$sample_type == "Sample")
        buffer_count <- sum(sample_data$sample_type == "Buffer")
        is_cluster <- spec$method %in% c("cluster", "stratified_cluster")

        if (is_cluster) {
          cluster_summary(summarise_clusters(data, sample_data, spec))
          shinyjs::show("cluster_summary_section")
        } else {
          cluster_summary(NULL)
          shinyjs::hide("cluster_summary_section")
        }

        methodology(build_methodology(spec, data, sample_data, frame_check_result(), sampling_notes, sampling_warnings))

        shinyjs::show("results_section")

        shinyjs::runjs(paste0('
          document.getElementById("', session$ns("results_header"), '").textContent = "Sample Results - ', SAMPLING_METHOD_LABELS[[spec$method]], '";
          document.getElementById("', session$ns("sample_size_result"), '").textContent = "', main_count, '";
          document.getElementById("', session$ns("population_size_result"), '").textContent = "', nrow(data), '";
          document.getElementById("', session$ns("sampling_fraction_result"), '").textContent = "', round(main_count / nrow(data) * 100, 2), '%";
          document.getElementById("', session$ns("seed_result"), '").textContent = "', spec$seed, '";
          document.getElementById("', session$ns("buffer_result"), '").textContent = "', buffer_count, ' (', round(buffer_count / main_count * 100, 1), '%)";
        '))

        for (note in sampling_notes) {
          showNotification(note, type = "message", duration = 10)
        }
        for (w in sampling_warnings) {
          showNotification(w, type = "warning", duration = NULL)
        }

        expected <- if (!is.null(result$allocation)) sum(result$allocation$n_allocated) else spec$sample_size
        if (main_count != expected) {
          showNotification(
            paste0("Main sample has ", main_count, " units (requested ", expected, ")",
                   if (is_cluster) ". Cluster samples are whole multiples of the interviews per cluster, and small clusters can fall short." else "."),
            type = "warning", duration = NULL
          )
        }

        showNotification("Sample generated successfully!", type = "message")

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

      rows <- list(
        c("Study name", or_blank(input$study_name)),
        c("Sampling method", SAMPLING_METHOD_LABELS[[spec$method]]),
        c("Selection within strata", if (spec$method == "stratified") or_blank(spec$within) else ""),
        c("Strata variable", if (is_strat) or_blank(spec$strata_var) else ""),
        c("Allocation", if (is_strat) or_blank(spec$allocation) else ""),
        c("Neyman input", if (is_strat && identical(spec$allocation, "neyman")) or_blank(spec$neyman_input) else ""),
        c("Minimum per stratum", if (is_strat) or_blank(spec$min_per_stratum) else ""),
        c("Cluster variable", if (is_cluster) or_blank(spec$cluster_var) else ""),
        c("Cluster population variable", if (is_cluster) or_blank(spec$pop_var) else ""),
        c("Cluster selection", if (is_cluster) (if (isTRUE(spec$use_pps)) "PPS with replacement" else "Equal probability without replacement") else ""),
        c("Interviews per cluster (m)", if (is_cluster) or_blank(spec$n_units) else ""),
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
      rows <- c(rows, list(
        c("Main sample drawn", or_blank(sum(main))),
        c("Buffer units drawn", or_blank(sum(!main))),
        c("Sum of base weights (main sample)", or_blank(round(sum(sample_data$base_weight[main]), 1))),
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
        sheets[["Column Guide"]] <- DESIGN_COLUMN_NOTES

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
  })
}
