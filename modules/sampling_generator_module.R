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
          h2("Sample Generator (Under Development)", class = "text-center mb-3"),
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
                  p("Number of observations to sample", class = "text-muted small")
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
                  p("Non-response rate adjustment", class = "text-muted small")
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
                      class = "col-md-6",
                      selectInput(
                        ns("strata_variable"),
                        "Strata Variable",
                        choices = c("Select a variable..." = ""),
                        width = "100%"
                      )
                    ),
                    div(
                      class = "col-md-6",
                      p("Select the variable that defines strata", class = "text-muted small")
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
                        "Cluster Size",
                        value = 10,
                        min = 2,
                        step = 1,
                        width = "100%"
                      ),
                        p("Average number of observations per cluster", class = "text-muted small")
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
                        p("Variable containing population size for each cluster", class = "text-muted small")
                      )
                    ),
                    div(
                      class = "row mt-2",
                      div(
                        class = "col-md-6",
                        numericInput(
                          ns("cluster_list_size"),
                          "Cluster Size",
                          value = 10,
                          min = 2,
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

# Helper function to create cluster summary
createClusterSummary <- function(data, sample_data, method, cluster_method, cluster_var, cluster_size, use_pps, cluster_list_var = NULL, cluster_pop_var = NULL) {
  
  if (cluster_method == "population_units") {
    # Method 1: Sample from Population Units
    cluster_summary <- aggregate(rep(1, nrow(data)), by = list(cluster = data[[cluster_var]]), FUN = sum)
    names(cluster_summary) <- c("cluster", "population")
    
    # Calculate probabilities for PPS
    total_population <- sum(cluster_summary$population)
    cluster_summary$probability <- cluster_summary$population / total_population
    
    # Count samples per cluster
    sample_counts <- table(sample_data[[cluster_var]])
    cluster_summary$sample_count <- sample_counts[match(cluster_summary$cluster, names(sample_counts))]
    cluster_summary$sample_count[is.na(cluster_summary$sample_count)] <- 0
    
    # Add sampling method info
    cluster_summary$sampling_method <- ifelse(use_pps, "PPS", "Equal Probability")
    cluster_summary$cluster_size <- cluster_size
    
  } else if (cluster_method == "cluster_list") {
    # Method 2: Sample from Cluster List
    cluster_list <- unique(data[[cluster_list_var]])
    cluster_populations <- aggregate(data[[cluster_pop_var]], by = list(cluster = data[[cluster_list_var]]), FUN = sum)
    names(cluster_populations) <- c("cluster", "population")
    
    # Calculate probabilities for PPS
    total_population <- sum(cluster_populations$population)
    cluster_populations$probability <- cluster_populations$population / total_population
    
    # Count samples per cluster
    sample_counts <- table(sample_data[[cluster_list_var]])
    cluster_populations$sample_count <- sample_counts[match(cluster_populations$cluster, names(sample_counts))]
    cluster_populations$sample_count[is.na(cluster_populations$sample_count)] <- 0
    
    # Add sampling method info
    cluster_populations$sampling_method <- ifelse(use_pps, "PPS", "Equal Probability")
    cluster_populations$cluster_size <- cluster_size
    
    cluster_summary <- cluster_populations
  }
  
  # Add additional information
  cluster_summary$sampling_fraction <- round(cluster_summary$sample_count / cluster_summary$population * 100, 2)
  cluster_summary$expected_sample <- pmin(cluster_summary$population, cluster_size)
  cluster_summary$sample_efficiency <- round(cluster_summary$sample_count / cluster_summary$expected_sample * 100, 2)
  
  return(cluster_summary)
}

# Sampling Generator Server
sampling_generator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    uploaded_data <- reactiveVal(NULL)
    generated_sample <- reactiveVal(NULL)
  cluster_summary <- reactiveVal(NULL)
    
    # Variable names modal
    observeEvent(input$show_variable_names, {
      req(uploaded_data())
      
      data <- uploaded_data()
      variable_names <- names(data)
      
      # Create organized variable names list
      organized_names <- lapply(1:length(variable_names), function(i) {
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
      
      # Create and show modal
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
        # Read the uploaded file
        file_path <- input$dataset_file$datapath
        file_ext <- tools::file_ext(input$dataset_file$name)
        
        if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else if (file_ext == "csv") {
          data <- readr::read_csv(file_path, show_col_types = FALSE)
        } else {
          stop("Unsupported file format")
        }
        
        # Store the data
        uploaded_data(data)
        
         # Show file info
         shinyjs::runjs(paste0('
           document.getElementById("', session$ns("file_name_display"), '").textContent = "File: ', input$dataset_file$name, '";
           document.getElementById("', session$ns("file_dimensions_display"), '").textContent = "Dimensions: ', nrow(data), ' rows × ', ncol(data), ' columns";
           document.getElementById("', session$ns("file_info_display"), '").style.display = "block";
         '))
        
        # Update variable choices for conditional inputs
        updateSelectInput(session, "strata_variable", 
                         choices = c("Select a variable..." = "", names(data)))
        updateSelectInput(session, "cluster_variable", 
                         choices = c("Select a variable..." = "", names(data)))
        updateSelectInput(session, "cluster_list_variable", 
                         choices = c("Select a variable..." = "", names(data)))
        updateSelectInput(session, "cluster_population_variable", 
                         choices = c("Select a variable..." = "", names(data)))
        
        # Update sample size maximum
        updateNumericInput(session, "sample_size", max = nrow(data))
        
        showNotification("Dataset uploaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        uploaded_data(NULL)
      })
    })
    
    # Show/hide conditional inputs based on sampling method
    observeEvent(input$sampling_method, {
      # Hide all conditional inputs first
      shinyjs::hide("conditional_inputs")
      shinyjs::hide("strata_inputs")
      shinyjs::hide("cluster_inputs")
      
      # Show conditional inputs section
      shinyjs::show("conditional_inputs")
      
      # Show relevant inputs based on method
      if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
        shinyjs::show("strata_inputs")
      }
      
      if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
        shinyjs::show("cluster_inputs")
      }
    })
    
    # Show/hide cluster method sections based on selected method
    observeEvent(input$cluster_sampling_method, {
      if (input$cluster_sampling_method == "population_units") {
        shinyjs::show("population_units_method")
        shinyjs::hide("cluster_list_method")
      } else if (input$cluster_sampling_method == "cluster_list") {
        shinyjs::hide("population_units_method")
        shinyjs::show("cluster_list_method")
      }
    })
    
     # Generate sample
     observeEvent(input$generate_sample, {
       req(uploaded_data(), input$sample_size)
       
       data <- uploaded_data()
       sample_size <- input$sample_size
       buffer_percentage <- input$buffer_percentage
       method <- input$sampling_method
       seed_value <- input$random_seed
       
       # Calculate buffer and final sample size
       buffer_amount <- round(sample_size * buffer_percentage / 100)
       final_sample_size <- sample_size + buffer_amount
       
       # Validate final sample size
       if (final_sample_size > nrow(data)) {
         showNotification("Final sample size (including buffer) cannot be larger than population size!", type = "error")
         return()
       }
       
       tryCatch({
         # Set seed for reproducibility
         set.seed(seed_value)
         
         if (method == "simple_random") {
           # Simple Random Sampling using centralized function
           sample_data <- simple_random_sampling(data, final_sample_size, seed_value)
           
         } else if (method == "systematic") {
           # Systematic Sampling using centralized function
           sample_data <- systematic_sampling(data, final_sample_size, seed_value)
           
         } else if (method == "cluster") {
           # 2-Stage Cluster Sampling with two methods
           cluster_method <- input$cluster_sampling_method
           
           if (cluster_method == "population_units") {
             # Method 1: Sample from Population Units
             cluster_var <- input$cluster_variable
             cluster_size <- input$cluster_size
             use_pps <- input$use_pps
           
             if (cluster_var == "") {
               showNotification("Please select a cluster variable!", type = "error")
               return()
             }
             
             # Use the new PPS cluster sampling function for Method 1
             if (use_pps) {
               # Validate parameters before calling PPS function
               if (is.null(cluster_var) || is.na(cluster_var) || cluster_var == "") {
                 showNotification("Cluster variable is not properly selected!", type = "error")
                 return()
               }
               if (is.null(cluster_size) || is.na(cluster_size) || cluster_size <= 0) {
                 showNotification("Cluster size must be a positive number!", type = "error")
                 return()
               }
               if (is.null(final_sample_size) || is.na(final_sample_size) || final_sample_size <= 0) {
                 showNotification("Sample size is invalid!", type = "error")
                 return()
               }
               if (is.null(seed_value) || is.na(seed_value)) {
                 seed_value <- 123456  # Default seed if not provided
               }
               
               # Create MOS (Measure of Size) from cluster variable
               data_with_mos <- create_mos_from_cluster(data, cluster_var)
               
               # Use the new PPS cluster sampling function
               cluster_result <- cluster_sampling_pps_wr_total_size(
                 data = data_with_mos,
                 cluster_id = cluster_var,
                 mos = "mos",
                 sample_size = final_sample_size,
                 n_units = cluster_size,
                 seed = seed_value
               )
               
               sample_data <- cluster_result$sampled_data
             } else {
               # Equal probability sampling without replacement using centralized function
               sample_data <- equal_probability_cluster_sampling(
                 data = data,
                 cluster_id = cluster_var,
                 sample_size = final_sample_size,
                 n_units = cluster_size,
                 seed = seed_value
               )
             }
             
           } else if (cluster_method == "cluster_list") {
             # Method 2: Sample from Cluster List using centralized function
             cluster_list_var <- input$cluster_list_variable
             cluster_pop_var <- input$cluster_population_variable
             cluster_size <- input$cluster_list_size
             use_pps <- input$use_pps_cluster_list
             
             if (cluster_list_var == "" || cluster_pop_var == "") {
               showNotification("Please select both cluster list and population variables!", type = "error")
               return()
             }
             
             # Use the centralized cluster list sampling function
             sample_data <- cluster_list_sampling(
               data = data,
               cluster_list_var = cluster_list_var,
               cluster_pop_var = cluster_pop_var,
               sample_size = final_sample_size,
               n_units = cluster_size,
               use_pps = use_pps,
               seed = seed_value
             )
           }
           
           # Remove any empty rows that might have been created
           sample_data <- sample_data[complete.cases(sample_data), ]
           
         } else if (method %in% c("stratified", "stratified_cluster")) {
           # Placeholder for future implementation
           showNotification("This sampling method will be implemented in a future update.", type = "warning")
           return()
         }
         
         # Add sample type column to distinguish between main sample and buffer
         actual_sample_size <- nrow(sample_data)
         
         # Remove any empty rows before adding sample_type
         sample_data <- sample_data[complete.cases(sample_data), ]
         actual_sample_size <- nrow(sample_data)
         
         # Check if we have any data
         if (actual_sample_size == 0) {
           showNotification("No valid data found in the sample. Please check your data and parameters.", type = "error")
           return()
         }
         
         # For cluster sampling, we don't strictly control sample size due to PPS
         # Assign sample types based on actual sample size
         if (actual_sample_size <= sample_size) {
           # All are sample if we didn't reach the target
           sample_data$sample_type <- rep("Sample", actual_sample_size)
         } else {
           # First 'sample_size' rows are sample, rest are buffer
           sample_data$sample_type <- c(rep("Sample", sample_size), rep("Buffer", actual_sample_size - sample_size))
         }
         
         # Reorder columns to put sample_type first
         sample_data <- sample_data[, c("sample_type", setdiff(names(sample_data), "sample_type"))]
         
         # Store the generated sample
         generated_sample(sample_data)
        
         # Create cluster summary for cluster sampling methods
         if (method %in% c("cluster_sampling", "stratified_cluster_sampling")) {
           cluster_summary_data <- createClusterSummary(data, sample_data, method, cluster_method, 
                                                       cluster_var, cluster_size, use_pps, 
                                                       cluster_list_var, cluster_pop_var)
           # Store cluster summary as a reactive value
           cluster_summary(cluster_summary_data)
         }
        
        # Show results section
        shinyjs::show("results_section")
        
        # Show/hide cluster summary based on sampling method
        if (method %in% c("cluster_sampling", "stratified_cluster_sampling")) {
          shinyjs::show("cluster_summary_section")
        } else {
          shinyjs::hide("cluster_summary_section")
        }
        
        # Update summary cards
        actual_buffer_amount <- actual_sample_size - sample_size
        if (actual_buffer_amount < 0) actual_buffer_amount <- 0
        
        shinyjs::runjs(paste0('
          document.getElementById("', session$ns("results_header"), '").textContent = "Sample Results - ', tools::toTitleCase(gsub("_", " ", method)), '";
          document.getElementById("', session$ns("sample_size_result"), '").textContent = "', sample_size, '";
          document.getElementById("', session$ns("population_size_result"), '").textContent = "', nrow(data), '";
          document.getElementById("', session$ns("sampling_fraction_result"), '").textContent = "', round(sample_size / nrow(data) * 100, 2), '%";
          document.getElementById("', session$ns("seed_result"), '").textContent = "', seed_value, '";
          document.getElementById("', session$ns("buffer_result"), '").textContent = "', actual_buffer_amount, ' (', round(actual_buffer_amount / sample_size * 100, 1), '%)";
        '))
        
        showNotification("Sample generated successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating sample:", e$message), type = "error")
      })
    })
    
    # Sample preview table
    output$sample_preview <- DT::renderDataTable({
      req(generated_sample())
      
      DT::datatable(
        generated_sample(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'rtip',
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50, 100),
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          paging = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(
              targets = "_all",
              className = "dt-center",
              render = JS("function(data, type, row) {
                if (type === 'display') {
                  // Truncate long text and add ellipsis
                  if (typeof data === 'string' && data.length > 30) {
                    return '<span title=\"' + data + '\">' + data.substring(0, 30) + '...</span>';
                  }
                  return data;
                }
                return data;
              }")
            )
          )
        ),
        rownames = FALSE,
        escape = FALSE,
        class = 'sample-preview-table'
      ) %>%
        DT::formatStyle(1, fontWeight = 'bold')
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
        write.csv(generated_sample(), file, row.names = FALSE)
      }
    )
    
    output$download_sample_excel <- downloadHandler(
      filename = function() {
        paste0("sample_", input$sampling_method, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Create list of sheets
        sheets_list <- list("Sample Data" = generated_sample())
        
        # Add cluster summary as second sheet if available
        if (!is.null(cluster_summary()) && nrow(cluster_summary()) > 0) {
          sheets_list[["Cluster Summary"]] <- cluster_summary()
        }
        
        writexl::write_xlsx(sheets_list, file)
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
