# Sample Size Calculator Module
# Calculators for different sampling methods

# Sample size calculation functions
calculate_simple_random_sample_size <- function(N, p = 0.5, confidence_level = 0.95, margin_error = 0.05, non_response_rate = 0.05) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  n <- (z^2 * p * (1 - p)) / (margin_error^2)
  n_adjusted <- n / (1 + (n - 1) / N)
  # Apply non-response rate adjustment
  n_final <- n_adjusted / (1 - non_response_rate)
  return(ceiling(n_final))
}

calculate_stratified_sample_size <- function(strata_data, p = 0.5, confidence_level = 0.95, margin_error = 0.05, non_response_rate = 0.05) {
  # Calculate sample size for each stratum based on its population size
  total_sample_size <- 0
  
  for (i in 1:nrow(strata_data)) {
    N_stratum <- strata_data$Population[i]
    n_stratum <- calculate_simple_random_sample_size(N_stratum, p, confidence_level, margin_error, non_response_rate)
    total_sample_size <- total_sample_size + n_stratum
  }
  
  return(total_sample_size)
}

calculate_cluster_sample_size <- function(N, p = 0.5, confidence_level = 0.95, margin_error = 0.05, 
                                        cluster_size = 10, icc = 0.06, non_response_rate = 0.05) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  n <- (z^2 * p * (1 - p)) / (margin_error^2)
  dess <- 1 + (cluster_size - 1) * icc
  n_cluster <- n * dess
  n_adjusted <- n_cluster / (1 + (n_cluster - 1) / N)
  # Apply non-response rate adjustment
  n_final <- n_adjusted / (1 - non_response_rate)
  return(ceiling(n_final))
}

calculate_stratified_cluster_sample_size <- function(strata_data, p = 0.5, confidence_level = 0.95, margin_error = 0.05, 
                                                   cluster_size = 10, icc = 0.06, non_response_rate = 0.05) {
  # Calculate sample size for each stratum based on its population size
  total_sample_size <- 0
  
  for (i in 1:nrow(strata_data)) {
    N_stratum <- strata_data$Population[i]
    n_stratum <- calculate_cluster_sample_size(N_stratum, p, confidence_level, margin_error, cluster_size, icc, non_response_rate)
    total_sample_size <- total_sample_size + n_stratum
  }
  
  return(total_sample_size)
}

# Function to create stratified sampling breakdown table
create_stratified_breakdown <- function(strata_data, p, confidence_level, margin_error, method_type = "stratified", cluster_size = 10, icc = 0.06, non_response_rate = 0.05) {
  # Calculate sample size for each stratum
  sample_sizes <- numeric(nrow(strata_data))
  
  for (i in 1:nrow(strata_data)) {
    N_stratum <- strata_data$Population[i]
    
    if (method_type == "stratified") {
      sample_sizes[i] <- calculate_simple_random_sample_size(N_stratum, p, confidence_level, margin_error, non_response_rate)
    } else if (method_type == "stratified_cluster") {
      sample_sizes[i] <- calculate_cluster_sample_size(N_stratum, p, confidence_level, margin_error, cluster_size, icc, non_response_rate)
    }
  }
  
  # Create breakdown table
  breakdown_data <- data.frame(
    Stratum = strata_data$Stratum,
    Population_Size = strata_data$Population,
    Sample_Size = sample_sizes,
    Sampling_Fraction = round(sample_sizes / strata_data$Population * 100, 2),
    Confidence_Level = rep(confidence_level * 100, nrow(strata_data)),
    Margin_of_Error = rep(margin_error * 100, nrow(strata_data)),
    Expected_Proportion = rep(p, nrow(strata_data)),
    Non_Response_Rate = rep(non_response_rate * 100, nrow(strata_data)),
    Method = rep(method_type, nrow(strata_data))
  )
  
  # Add design effect information for stratified cluster sampling
  if (method_type == "stratified_cluster") {
    dess <- 1 + (cluster_size - 1) * icc
    breakdown_data$Cluster_Size <- rep(cluster_size, nrow(strata_data))
    breakdown_data$ICC <- rep(round(icc, 3), nrow(strata_data))
    breakdown_data$Design_Effect <- rep(round(dess, 3), nrow(strata_data))
    
    # Reorder columns to put Method at the end
    breakdown_data <- breakdown_data[, c("Stratum", "Population_Size", "Sample_Size", "Sampling_Fraction", 
                                        "Confidence_Level", "Margin_of_Error", "Expected_Proportion", 
                                        "Non_Response_Rate", "Cluster_Size", "ICC", "Design_Effect", "Method")]
  }
  
  return(breakdown_data)
}

# Default theme for ggplot2
theme_sampling <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
    )
}

sample_calculator_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Custom CSS for Excel-like table styling
    tags$head(
      tags$style(HTML("
        .excel-table {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
          font-size: 14px;
        }
        
        .excel-table th {
          background-color: #4472C4 !important;
          color: white !important;
          font-weight: bold !important;
          text-align: center !important;
          border: 1px solid #8EA9DB !important;
          padding: 8px !important;
          vertical-align: middle !important;
          height: 40px !important;
          line-height: 1.2 !important;
        }
        
        .excel-table td {
          border: 1px solid #D9D9D9 !important;
          padding: 6px 8px !important;
          background-color: white !important;
        }
        
        .excel-table tr:hover td {
          background-color: #F2F2F2 !important;
        }
        
        .excel-table td:focus {
          background-color: #E6F3FF !important;
          border: 2px solid #4472C4 !important;
        }
        
        .strata-inputs-container {
          border: 2px solid #D9D9D9;
          border-radius: 4px;
          padding: 15px;
          background-color: #FAFAFA;
        }
        
        .stratum-input-group {
          background-color: white;
          border: 1px solid #E0E0E0;
          border-radius: 6px;
          padding: 12px;
          margin-bottom: 10px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .stratum-input-group:hover {
          border-color: #4472C4;
          box-shadow: 0 2px 6px rgba(68, 114, 196, 0.15);
        }
        
        .table-edit-hint {
          font-size: 12px;
          color: #666;
          font-style: italic;
          margin-top: 5px;
        }

        /* Compact layout for calculator */
        .card-body { padding: 0.75rem; }
        .card-header { padding: 0.5rem 0.75rem; }
        .form-group { margin-bottom: 0.5rem; }
        label { margin-bottom: 0.25rem; font-size: 0.9rem; }
        .form-control { padding: 0.25rem 0.5rem; height: auto; }
        .mb-4 { margin-bottom: 0.9rem !important; }
        h2 { margin-bottom: 0.5rem; }
        .card { margin-bottom: 0.9rem; }
        .row.g-2 > [class^='col-'], .row.g-2 > [class*=' col-'] { padding-left: 6px; padding-right: 6px; }
      "))
    ),
    
    # Header
    div(
      class = "row mb-4",
      div(
        class = "col-12",
        h2("Sample Size Calculator", class = "text-center mb-3"),
        p("Calculate the required sample size for your study using different sampling methods.", 
          class = "text-center text-muted")
      )
    ),
    
    # Method selection
    div(
      class = "row mb-4",
      div(
        class = "col-12",
        div(
          class = "card",
          div(
            class = "card-header",
            h5("Select Sampling Method", class = "mb-0")
          ),
          div(
            class = "card-body",
            div(
              class = "row",
              div(
                class = "col-md-6",
                selectInput(
                  ns("sampling_method"),
                  "Sampling Method",
                  choices = list(
                    "Simple Random Sampling" = "simple_random",
                    "Stratified Random Sampling" = "stratified",
                    "Cluster Sampling" = "cluster",
                    "Stratified Cluster Sampling" = "stratified_cluster",
                    "Systematic Sampling" = "systematic"
                  ),
                  selected = "simple_random"
                )
              ),
              div(
                class = "col-md-6",
                p("Choose the sampling method you plan to use for your study.", 
                  class = "text-muted small")
              )
            )
          )
        )
      )
    ),
    
    # Input parameters
    div(
      class = "row mb-4",
      div(
        class = "col-12",
        div(
          class = "card",
          div(
            class = "card-header",
            h5("Study Parameters", class = "mb-0")
          ),
          div(
            class = "card-body",
            # Study Name
            div(
              class = "mb-3",
              textInput(
                ns("study_name"),
                "Study Name",
                placeholder = "Enter study name (optional)",
                width = "100%"
              )
            ),
            div(
              class = "row",
              
              # Basic parameters
              div(
                class = "col-md-6",
                h6("Basic Parameters"),
                div(
                  class = "row g-2",
                  div(
                    class = "col-md-6",
                    numericInput(
                      ns("population_size"),
                      "Population Size (N)",
                      value = 10000,
                      min = 1,
                      step = 1
                    )
                  ),
                  div(
                    class = "col-md-6",
                    numericInput(
                      ns("confidence_level"),
                      "Confidence Level",
                      value = 0.95,
                      min = 0.80,
                      max = 0.99,
                      step = 0.01
                    )
                  ),
                  div(
                    class = "col-md-6",
                    numericInput(
                      ns("margin_error"),
                      "Margin of Error",
                      value = 0.05,
                      min = 0.01,
                      max = 0.20,
                      step = 0.01
                    )
                  ),
                  div(
                    class = "col-md-6",
                    numericInput(
                      ns("expected_proportion"),
                      "Expected Proportion (p)",
                      value = 0.5,
                      min = 0.01,
                      max = 0.99,
                      step = 0.01
                    )
                  ),
                  div(
                    class = "col-md-6",
                    numericInput(
                      ns("non_response_rate"),
                      "Non-Response Rate (%)",
                      value = 5,
                      min = 0,
                      max = 50,
                      step = 1
                    )
                  )
                )
              ),
              
              # Data input method selection (only for stratified methods)
              div(
                id = ns("data_input_method_section"),
                class = "col-12",
                style = "display: none;",
                div(
                  class = "card mb-3",
                  div(
                    class = "card-header",
                    h6("Data Input Method", class = "mb-0")
                  ),
                  div(
                    class = "card-body",
                    radioButtons(
                      ns("data_input_method"),
                      "Choose how to provide population data:",
                      choices = list(
                        "Manual Entry" = "manual",
                        "Upload Data File" = "upload"
                      ),
                      selected = "manual",
                      inline = TRUE
                    ),
                    
                    # Data upload section (conditional)
                    div(
                      id = ns("data_upload_section"),
                      style = "display: none;",
                      fileInput(
                        ns("population_data_file"),
                        "Upload Excel/CSV file with population data",
                        accept = c(".xlsx", ".xls", ".csv"),
                        placeholder = "No file selected"
                      ),
                      div(
                        id = ns("file_info"),
                        style = "display: none;",
                        div(
                          class = "alert alert-info",
                          p(id = ns("file_name"), class = "mb-1"),
                          p(id = ns("file_dimensions"), class = "mb-0 small")
                        )
                      )
                    )
                  )
                )
              ),
              
              # Method-specific parameters
              div(
                class = "col-12",
                h6("Method-Specific Parameters"),
                
                # Stratified sampling parameters
                div(
                  id = ns("stratified_params"),
                  style = "display: none;",
                  div(
                    class = "row",
                    # Left column - Manual entry
                    div(
                      class = "col-md-6",
                      div(
                        id = ns("manual_strata_section"),
                        h6("Manual Strata Entry", class = "mb-3"),
                        numericInput(
                          ns("num_strata"),
                          "Number of Strata",
                          value = 3,
                          min = 2,
                          step = 1
                        ),
                        div(
                          id = ns("strata_inputs_container"),
                          class = "strata-inputs-container",
                          h6("Strata Information", class = "mb-3"),
                          uiOutput(ns("strata_inputs")),
                          div(
                            class = "mt-3",
                            actionButton(ns("add_stratum"), "➕ Add Stratum", class = "btn btn-sm btn-outline-primary me-2"),
                            actionButton(ns("remove_stratum"), "➖ Remove Stratum", class = "btn btn-sm btn-outline-danger")
                          )
                        ),
                       
                      )
                    ),
                    # Right column - Data upload selection
                    div(
                      class = "col-md-6",
                      div(
                        id = ns("uploaded_data_section"),
                        style = "display: none;",
                        h6("Select Data Columns", class = "mb-3"),
                        selectInput(
                          ns("stratified_strata_column"),
                          "Strata Variable",
                          choices = c("Select a column..." = ""),
                          width = "100%"
                        ),
                        selectInput(
                          ns("stratified_population_column"),
                          "Population Variable",
                          choices = c("Select a column..." = ""),
                          width = "100%"
                        )
                      )
                    )
                  )
                ),
                
                # Cluster sampling parameters
                div(
                  id = ns("cluster_params"),
                  style = "display: none;",
                  numericInput(
                    ns("cluster_size"),
                    "Cluster Size",
                    value = 10,
                    min = 2,
                    step = 1
                  ),
                  numericInput(
                    ns("intraclass_correlation"),
                    "Intraclass Correlation (ICC)",
                    value = 0.06,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  
                  # Data column selection for cluster sampling
                  div(
                    id = ns("cluster_data_selection"),
                    style = "display: none;",
                    h6("Select Data Columns", class = "mb-3"),
                    selectInput(
                      ns("cluster_population_column"),
                      "Population Variable",
                      choices = c("Select a column..." = ""),
                      width = "100%"
                    ),
                    selectInput(
                      ns("cluster_name_column"),
                      "Cluster Name Variable",
                      choices = c("Select a column..." = ""),
                      width = "100%"
                    )
                  )
                ),
                
                # Stratified cluster sampling parameters
                div(
                  id = ns("stratified_cluster_params"),
                  style = "display: none;",
                  div(
                    class = "row",
                    # Left column - Manual entry
                    div(
                      class = "col-md-6",
                      div(
                        id = ns("manual_strata_cluster_section"),
                        h6("Manual Strata Entry", class = "mb-3"),
                        numericInput(
                          ns("num_strata_cluster"),
                          "Number of Strata",
                          value = 3,
                          min = 2,
                          step = 1
                        ),
                        div(
                          id = ns("strata_inputs_cluster_container"),
                          class = "strata-inputs-container",
                          h6("Strata Information", class = "mb-3"),
                          uiOutput(ns("strata_inputs_cluster")),
                          div(
                            class = "mt-3",
                            actionButton(ns("add_stratum_cluster"), "➕ Add Stratum", class = "btn btn-sm btn-outline-primary me-2"),
                            actionButton(ns("remove_stratum_cluster"), "➖ Remove Stratum", class = "btn btn-sm btn-outline-danger")
                          )
                        ),
                        numericInput(
                          ns("cluster_size_stratified"),
                          "Cluster Size",
                          value = 10,
                          min = 2,
                          step = 1
                        ),
                        numericInput(
                          ns("intraclass_correlation_stratified"),
                          "Intraclass Correlation (ICC)",
                          value = 0.06,
                          min = 0,
                          max = 1,
                          step = 0.01
                        )
                      )
                    ),
                    # Right column - Data upload selection
                    div(
                      class = "col-md-6",
                      div(
                        id = ns("uploaded_data_cluster_section"),
                        style = "display: none;",
                        h6("Select Data Columns", class = "mb-3"),
                        selectInput(
                          ns("stratified_cluster_strata_column"),
                          "Strata Variable",
                          choices = c("Select a column..." = ""),
                          width = "100%"
                        ),
                        selectInput(
                          ns("stratified_cluster_population_column"),
                          "Population Variable",
                          choices = c("Select a column..." = ""),
                          width = "100%"
                        ),
                        numericInput(
                          ns("cluster_size_stratified_upload"),
                          "Cluster Size",
                          value = 10,
                          min = 2,
                          step = 1
                        ),
                        numericInput(
                          ns("intraclass_correlation_stratified_upload"),
                          "Intraclass Correlation (ICC)",
                          value = 0.06,
                          min = 0,
                          max = 1,
                          step = 0.01
                        )
                      )
                    )
                  )
                ),
                
                # Systematic sampling parameters
                div(
                  id = ns("systematic_params"),
                  style = "display: none;",
                  numericInput(
                    ns("sampling_interval"),
                    "Sampling Interval (k)",
                    value = 10,
                    min = 2,
                    step = 1
                  ),
                  p("k = N/n, where N is population size and n is desired sample size.", 
                    class = "text-muted small")
                )
              )
            )
          )
        )
      )
    ),
    
    # Calculate button
    div(
      class = "row mb-3",
      div(
        class = "col-12 text-end",
        actionButton(
          ns("calculate"),
          "Calculate Sample Size",
          class = "btn btn-primary"
        )
      )
    ),
    
    # Results
    div(
      class = "row",
      div(
        class = "col-12",
        div(
          id = ns("results_container"),
          style = "display: none;",
          div(
            class = "row",
            # First row: sample size + plot and calculations side by side
            div(
              class = "col-md-6",
              div(
                class = "card border-success",
                div(
                  class = "card-header bg-success text-white",
                  h5("Required Sample Size", class = "mb-0")
                ),
                div(
                  class = "card-body text-center",
                  h2(
                    id = ns("sample_size_result"),
                    class = "text-success mb-2"
                  ),
                  p(
                    id = ns("sample_size_explanation"),
                    class = "text-muted"
                  )
                )
              ),
              div(
                class = "card",
                div(
                  class = "card-header",
                  h5("Sample Size Visualization", class = "mb-0")
                ),
                div(
                  class = "card-body",
                  plotlyOutput(ns("sample_size_plot"), height = "320px")
                )
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-header",
                  h5("Detailed Calculations", class = "mb-0")
                ),
                div(
                  class = "card-body",
                  DT::dataTableOutput(ns("calculation_table"))
                )
              ),
              div(
                class = "card",
                div(
                  class = "card-header",
                  h5("Download Results", class = "mb-0")
                ),
                div(
                  class = "card-body",
                  div(
                    class = "btn-group",
                    role = "group",
                    downloadButton(ns("download_excel"), "Excel", class = "btn btn-outline-success btn-sm"),
                    downloadButton(ns("download_pdf"), "PDF Report", class = "btn btn-outline-danger btn-sm")
                  )
                )
              )
            ),
            # Second row: stratified breakdown (full width)
            div(
              class = "col-12",
              div(
                id = ns("stratified_breakdown_section"),
                class = "card",
                style = "display: none;",
                div(
                  class = "card-header",
                  h5("Stratified Sampling Breakdown", class = "mb-0")
                ),
                div(
                  class = "card-body",
                  DT::dataTableOutput(ns("stratified_breakdown_table")),
                  div(
                    class = "mt-2",
                    downloadButton(ns("download_stratified_breakdown"), "Download Stratified Breakdown", class = "btn btn-outline-primary btn-sm")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

sample_calculator_server <- function(id, study_name = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Study Name synchronization
    if (!is.null(study_name)) {
      # Update local input when shared study_name changes
      observeEvent(study_name(), {
        if (study_name() != input$study_name) {
          updateTextInput(session, "study_name", value = study_name())
        }
      })
      
      # Update shared study_name when local input changes
      observeEvent(input$study_name, {
        if (input$study_name != study_name()) {
          study_name(input$study_name)
        }
      })
    }
    
    # Function to create dynamic strata input fields using Shiny's renderUI
    update_strata_inputs <- function(container_id, data) {
      ns <- session$ns
      
      # Create a list of input elements
      input_elements <- list()
      
      for (i in 1:nrow(data)) {
        stratum_name_id <- paste0("stratum_name_", i)
        stratum_pop_id <- paste0("stratum_pop_", i)
        
        input_elements[[i]] <- div(
          class = "stratum-input-group",
          div(
            class = "row",
            div(
              class = "col-md-6",
              textInput(ns(stratum_name_id), "Stratum Name", value = data$Stratum[i])
            ),
            div(
              class = "col-md-6",
              numericInput(ns(stratum_pop_id), "Population", value = data$Population[i], min = 1)
            )
          )
        )
      }
      
      # Update the container using renderUI
      output[[container_id]] <- renderUI({
        input_elements
      })
    }
    
    # Show/hide method-specific parameters
    observeEvent(input$sampling_method, {
      # Hide all parameter panels
      lapply(c("stratified_params", "cluster_params", "stratified_cluster_params", "systematic_params"), 
             function(id) hideElement(id))
      
      # Hide data input method section by default
      hideElement("data_input_method_section")
      
      # Show relevant parameter panel
      if (input$sampling_method == "stratified") {
        showElement("stratified_params")
        showElement("data_input_method_section")
        # Initialize strata inputs when stratified sampling is selected
        update_strata_inputs("strata_inputs", strata_data())
      } else if (input$sampling_method == "stratified_cluster") {
        showElement("stratified_cluster_params")
        showElement("data_input_method_section")
        # Initialize strata inputs when stratified cluster sampling is selected
        update_strata_inputs("strata_inputs_cluster", strata_data_cluster())
      } else if (input$sampling_method == "cluster") {
        showElement("cluster_params")
      } else if (input$sampling_method == "systematic") {
        showElement("systematic_params")
      }
    })
    
    # Handle data input method selection
    observeEvent(input$data_input_method, {
      if (input$data_input_method == "manual") {
        showElement("manual_strata_section")
        hideElement("uploaded_data_section")
        hideElement("data_upload_section")
        if (input$sampling_method == "stratified_cluster") {
          showElement("manual_strata_cluster_section")
          hideElement("uploaded_data_cluster_section")
        }
      } else if (input$data_input_method == "upload") {
        hideElement("manual_strata_section")
        showElement("uploaded_data_section")
        showElement("data_upload_section")
        if (input$sampling_method == "stratified_cluster") {
          hideElement("manual_strata_cluster_section")
          showElement("uploaded_data_cluster_section")
        }
      }
    })
    
    # Reactive values for strata tables
    strata_data <- reactiveVal(data.frame(
      Stratum = c("Stratum 1", "Stratum 2", "Stratum 3"),
      Population = c(3333, 3333, 3334),
      stringsAsFactors = FALSE
    ))
    
    # Reactive values for uploaded data
    uploaded_data <- reactiveVal(NULL)
    column_names <- reactiveVal(character(0))
    
    # Handle file upload
    observeEvent(input$population_data_file, {
      req(input$population_data_file)
      
      tryCatch({
        # Read the uploaded file
        file_path <- input$population_data_file$datapath
        file_ext <- tools::file_ext(input$population_data_file$name)
        
        if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else if (file_ext == "csv") {
          data <- readr::read_csv(file_path, show_col_types = FALSE)
        } else {
          stop("Unsupported file format")
        }
        
        # Store the data and column names
        uploaded_data(data)
        column_names(names(data))
        
        # Show file info
        shinyjs::runjs(paste0('
          document.getElementById("', session$ns("file_name"), '").textContent = "File: ', input$population_data_file$name, '";
          document.getElementById("', session$ns("file_dimensions"), '").textContent = "Dimensions: ', nrow(data), ' rows × ', ncol(data), ' columns";
          document.getElementById("', session$ns("file_info"), '").style.display = "block";
        '))
        
        # Show uploaded data section and update dropdown choices based on current method
        if (input$sampling_method == "stratified") {
          showElement("uploaded_data_section")
          updateSelectInput(session, "stratified_strata_column", choices = c("Select a column..." = "", column_names()))
          updateSelectInput(session, "stratified_population_column", choices = c("Select a column..." = "", column_names()))
        } else if (input$sampling_method == "stratified_cluster") {
          showElement("uploaded_data_cluster_section")
          updateSelectInput(session, "stratified_cluster_strata_column", choices = c("Select a column..." = "", column_names()))
          updateSelectInput(session, "stratified_cluster_population_column", choices = c("Select a column..." = "", column_names()))
        }
        
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        uploaded_data(NULL)
        column_names(character(0))
      })
    })
    
    # Update population size when stratified columns are selected
    observeEvent(c(input$stratified_strata_column, input$stratified_population_column), {
      if (input$sampling_method == "stratified" && 
          !is.null(uploaded_data()) && 
          input$stratified_strata_column != "" && 
          input$stratified_population_column != "") {
        
        # Calculate total population from uploaded data
        strata_col <- input$stratified_strata_column
        pop_col <- input$stratified_population_column
        
        if (is.numeric(uploaded_data()[[pop_col]])) {
          total_population <- sum(uploaded_data()[[pop_col]], na.rm = TRUE)
          updateNumericInput(session, "population_size", value = total_population)
        }
      }
    })
    
    # Update population size when stratified cluster columns are selected
    observeEvent(c(input$stratified_cluster_strata_column, input$stratified_cluster_population_column), {
      if (input$sampling_method == "stratified_cluster" && 
          !is.null(uploaded_data()) && 
          input$stratified_cluster_strata_column != "" && 
          input$stratified_cluster_population_column != "") {
        
        # Calculate total population from uploaded data
        strata_col <- input$stratified_cluster_strata_column
        pop_col <- input$stratified_cluster_population_column
        
        if (is.numeric(uploaded_data()[[pop_col]])) {
          total_population <- sum(uploaded_data()[[pop_col]], na.rm = TRUE)
          updateNumericInput(session, "population_size", value = total_population)
        }
      }
    })
    
    strata_data_cluster <- reactiveVal(data.frame(
      Stratum = c("Stratum 1", "Stratum 2", "Stratum 3"),
      Population = c(3333, 3333, 3334),
      stringsAsFactors = FALSE
    ))
    
    # Update strata data based on number of strata for stratified sampling
    observeEvent(input$num_strata, {
      if (input$sampling_method == "stratified") {
        current_data <- strata_data()
        current_rows <- nrow(current_data)
        new_rows <- input$num_strata
        
        if (new_rows > current_rows) {
          # Add new strata
          for (i in (current_rows + 1):new_rows) {
            new_row <- data.frame(
              Stratum = paste0("Stratum ", i),
              Population = 1000,  # Default population for new strata
              stringsAsFactors = FALSE
            )
            current_data <- rbind(current_data, new_row)
          }
        } else if (new_rows < current_rows) {
          # Remove excess strata
          current_data <- current_data[1:new_rows, ]
        }
        
        strata_data(current_data)
        update_strata_inputs("strata_inputs", strata_data())
      }
    })
    
    # Update strata data based on number of strata for stratified cluster sampling
    observeEvent(input$num_strata_cluster, {
      if (input$sampling_method == "stratified_cluster") {
        current_data <- strata_data_cluster()
        current_rows <- nrow(current_data)
        new_rows <- input$num_strata_cluster
        
        if (new_rows > current_rows) {
          # Add new strata
          for (i in (current_rows + 1):new_rows) {
            new_row <- data.frame(
              Stratum = paste0("Stratum ", i),
              Population = 1000,  # Default population for new strata
              stringsAsFactors = FALSE
            )
            current_data <- rbind(current_data, new_row)
          }
        } else if (new_rows < current_rows) {
          # Remove excess strata
          current_data <- current_data[1:new_rows, ]
        }
        
        strata_data_cluster(current_data)
        update_strata_inputs("strata_inputs_cluster", strata_data_cluster())
      }
    })
    
    # Add stratum for regular stratified sampling
    observeEvent(input$add_stratum, {
      if (input$sampling_method == "stratified") {
        current_data <- strata_data()
        new_row <- data.frame(
          Stratum = paste0("Stratum ", nrow(current_data) + 1),
          Population = 1000,  # Default population
          stringsAsFactors = FALSE
        )
        strata_data(rbind(current_data, new_row))
        updateNumericInput(session, "num_strata", value = nrow(current_data) + 1)
        update_strata_inputs("strata_inputs", strata_data())
        
        # Update Population Size (N) input based on total strata population
        total_population <- sum(rbind(current_data, new_row)$Population)
        updateNumericInput(session, "population_size", value = total_population)
      }
    })
    
    # Remove stratum for regular stratified sampling
    observeEvent(input$remove_stratum, {
      if (input$sampling_method == "stratified") {
        current_data <- strata_data()
        if (nrow(current_data) > 2) {  # Keep at least 2 strata
          new_data <- current_data[-nrow(current_data), ]
          strata_data(new_data)
          updateNumericInput(session, "num_strata", value = nrow(current_data) - 1)
          update_strata_inputs("strata_inputs", strata_data())
          
          # Update Population Size (N) input based on total strata population
          total_population <- sum(new_data$Population)
          updateNumericInput(session, "population_size", value = total_population)
        }
      }
    })
    
    # Add stratum for stratified cluster sampling
    observeEvent(input$add_stratum_cluster, {
      if (input$sampling_method == "stratified_cluster") {
        current_data <- strata_data_cluster()
        new_row <- data.frame(
          Stratum = paste0("Stratum ", nrow(current_data) + 1),
          Population = 1000,  # Default population
          stringsAsFactors = FALSE
        )
        strata_data_cluster(rbind(current_data, new_row))
        updateNumericInput(session, "num_strata_cluster", value = nrow(current_data) + 1)
        update_strata_inputs("strata_inputs_cluster", strata_data_cluster())
        
        # Update Population Size (N) input based on total strata population
        total_population <- sum(rbind(current_data, new_row)$Population)
        updateNumericInput(session, "population_size", value = total_population)
      }
    })
    
    # Remove stratum for stratified cluster sampling
    observeEvent(input$remove_stratum_cluster, {
      if (input$sampling_method == "stratified_cluster") {
        current_data <- strata_data_cluster()
        if (nrow(current_data) > 2) {  # Keep at least 2 strata
          new_data <- current_data[-nrow(current_data), ]
          strata_data_cluster(new_data)
          updateNumericInput(session, "num_strata_cluster", value = nrow(current_data) - 1)
          update_strata_inputs("strata_inputs_cluster", strata_data_cluster())
          
          # Update Population Size (N) input based on total strata population
          total_population <- sum(new_data$Population)
          updateNumericInput(session, "population_size", value = total_population)
        }
      }
    })
    
    # Function to update strata data from dynamic inputs
    update_strata_from_inputs <- function(container_id, strata_reactive_val) {
      ns <- session$ns
      
      # JavaScript to capture input values and send to R
      js_code <- paste0("
        var strataData = [];
        $('#", ns(container_id), " .stratum-input-group').each(function(index) {
          var nameInput = $(this).find('input[type=\"text\"]');
          var popInput = $(this).find('input[type=\"number\"]');
          
          if (nameInput.length > 0 && popInput.length > 0) {
            strataData.push({
              stratum: nameInput.val() || 'Stratum ' + (index + 1),
              population: parseInt(popInput.val()) || 1000
            });
          }
        });
        
        Shiny.setInputValue('", ns("strata_input_data"), "', strataData, {priority: 'event'});
      ")
      
      shinyjs::runjs(js_code)
    }
    
    # Observer to capture strata input data from Shiny inputs
    observe({
      if (input$sampling_method == "stratified") {
        current_data <- strata_data()
        if (nrow(current_data) > 0) {
          # Capture values from Shiny inputs
          stratum_names <- character(nrow(current_data))
          stratum_populations <- numeric(nrow(current_data))
          
          for (i in 1:nrow(current_data)) {
            name_input <- input[[paste0("stratum_name_", i)]]
            pop_input <- input[[paste0("stratum_pop_", i)]]
            
            stratum_names[i] <- if (is.null(name_input) || name_input == "") paste0("Stratum ", i) else name_input
            stratum_populations[i] <- if (is.null(pop_input) || is.na(pop_input)) 1000 else as.numeric(pop_input)
          }
          
          # Create new data frame
          new_data <- data.frame(
            Stratum = stratum_names,
            Population = stratum_populations,
            stringsAsFactors = FALSE
          )
          
          # Update if different
          if (!identical(new_data, current_data)) {
            strata_data(new_data)
            
            # Update Population Size (N) input based on total strata population
            total_population <- sum(new_data$Population)
            updateNumericInput(session, "population_size", value = total_population)
          }
        }
      }
    })
    
    # Observer for stratified cluster sampling
    observe({
      if (input$sampling_method == "stratified_cluster") {
        current_data <- strata_data_cluster()
        if (nrow(current_data) > 0) {
          # Capture values from Shiny inputs
          stratum_names <- character(nrow(current_data))
          stratum_populations <- numeric(nrow(current_data))
          
          for (i in 1:nrow(current_data)) {
            name_input <- input[[paste0("stratum_name_", i)]]
            pop_input <- input[[paste0("stratum_pop_", i)]]
            
            stratum_names[i] <- if (is.null(name_input) || name_input == "") paste0("Stratum ", i) else name_input
            stratum_populations[i] <- if (is.null(pop_input) || is.na(pop_input)) 1000 else as.numeric(pop_input)
          }
          
          # Create new data frame
          new_data <- data.frame(
            Stratum = stratum_names,
            Population = stratum_populations,
            stringsAsFactors = FALSE
          )
          
          # Update if different
          if (!identical(new_data, current_data)) {
            strata_data_cluster(new_data)
            
            # Update Population Size (N) input based on total strata population
            total_population <- sum(new_data$Population)
            updateNumericInput(session, "population_size", value = total_population)
          }
        }
      }
    })
    
    # Calculate sample size
    calculate_sample_size <- reactive({
      req(input$population_size, input$confidence_level, input$margin_error, input$expected_proportion)
      
      N <- input$population_size
      p <- input$expected_proportion
      confidence_level <- input$confidence_level
      margin_error <- input$margin_error
      
      if (input$sampling_method == "simple_random") {
        non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
        n <- calculate_simple_random_sample_size(N, p, confidence_level, margin_error, non_response_rate)
        method_name <- "Simple Random Sampling"
        
      } else if (input$sampling_method == "stratified") {
        # Use uploaded data if data input method is upload and columns are selected
        if (input$data_input_method == "upload" && 
            !is.null(uploaded_data()) && 
            input$stratified_strata_column != "" && 
            input$stratified_population_column != "") {
          
          # Extract strata data from uploaded file
          strata_col <- input$stratified_strata_column
          pop_col <- input$stratified_population_column
          
          # Check if population column is numeric
          if (!is.numeric(uploaded_data()[[pop_col]])) {
            showNotification("Population column must be numeric.", type = "error")
            return(NULL)
          }

          # Create strata summary using base R approach to avoid dplyr conflicts
          strata_summary <- aggregate(uploaded_data()[[pop_col]], 
                                    by = list(Stratum = uploaded_data()[[strata_col]]), 
                                    FUN = sum, na.rm = TRUE)
          names(strata_summary)[2] <- "Population"
          
          non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
          n <- calculate_stratified_sample_size(strata_summary, p, confidence_level, margin_error, non_response_rate)
          method_name <- "Stratified Sampling"
        } else {
          # Use manual strata data
          non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
          n <- calculate_stratified_sample_size(strata_data(), p, confidence_level, margin_error, non_response_rate)
          method_name <- "Stratified Sampling"
        }
        
      } else if (input$sampling_method == "cluster") {
        cluster_size <- input$cluster_size
        icc <- input$intraclass_correlation
        
        # Use uploaded data if available and columns are selected
        if (!is.null(uploaded_data()) && 
            input$cluster_population_column != "" && 
            input$cluster_name_column != "") {
          
          # Calculate total population from uploaded data
          pop_col <- input$cluster_population_column
          N <- sum(uploaded_data()[[pop_col]], na.rm = TRUE)
        }
        
        non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
        n <- calculate_cluster_sample_size(N, p, confidence_level, margin_error, cluster_size, icc, non_response_rate)
        method_name <- if (!is.null(uploaded_data()) && 
                          input$cluster_population_column != "" && 
                          input$cluster_name_column != "") {
          "Cluster Sampling"
        } else {
          "Cluster Sampling"
        }
        
      } else if (input$sampling_method == "stratified_cluster") {
        # Use uploaded data if data input method is upload and columns are selected
        if (input$data_input_method == "upload" && 
            !is.null(uploaded_data()) && 
            input$stratified_cluster_strata_column != "" && 
            input$stratified_cluster_population_column != "") {
          
          cluster_size <- input$cluster_size_stratified_upload
          icc <- input$intraclass_correlation_stratified_upload
          
          # Extract strata data from uploaded file
          strata_col <- input$stratified_cluster_strata_column
          pop_col <- input$stratified_cluster_population_column
          
          # Check if population column is numeric
          if (!is.numeric(uploaded_data()[[pop_col]])) {
            showNotification("Population column must be numeric.", type = "error")
            return(NULL)
          }

          # Create strata summary using base R approach to avoid dplyr conflicts
          strata_summary <- aggregate(uploaded_data()[[pop_col]], 
                                    by = list(Stratum = uploaded_data()[[strata_col]]), 
                                    FUN = sum, na.rm = TRUE)
          names(strata_summary)[2] <- "Population"
          
          non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
          n <- calculate_stratified_cluster_sample_size(strata_summary, p, confidence_level, margin_error, cluster_size, icc, non_response_rate)
          method_name <- "Stratified Cluster Sampling"
        } else {
          # Use manual strata data
          cluster_size <- input$cluster_size_stratified
          icc <- input$intraclass_correlation_stratified
          non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
          n <- calculate_stratified_cluster_sample_size(strata_data_cluster(), p, confidence_level, margin_error, cluster_size, icc, non_response_rate)
          method_name <- "Stratified Cluster Sampling"
        }
        
      } else if (input$sampling_method == "systematic") {
        non_response_rate <- input$non_response_rate / 100  # Convert percentage to decimal
        n <- calculate_simple_random_sample_size(N, p, confidence_level, margin_error, non_response_rate)
        method_name <- "Systematic Sampling"
      }
      
      return(list(
        sample_size = n,
        method_name = method_name,
        population_size = N,
        confidence_level = confidence_level,
        margin_error = margin_error,
        expected_proportion = p,
        non_response_rate = input$non_response_rate / 100
      ))
    })
    
    # Update results when calculate button is clicked
    observeEvent(input$calculate, {
      results <- calculate_sample_size()
      
      # Update sample size display
      shinyjs::runjs(paste0("
        $('#", session$ns("sample_size_result"), "').text('", results$sample_size, "');
      "))
      
      # Update explanation
      explanation <- paste0(
        "For a population of ", format(results$population_size, big.mark = ","), 
        " with ", results$confidence_level * 100, "% confidence level and ", 
        results$margin_error * 100, "% margin of error, you need a sample size of ",
        results$sample_size, " using ", results$method_name, 
        " (adjusted for ", input$non_response_rate, "% non-response rate)."
      )
      
      # Add design effect information for cluster sampling methods
      if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
        cluster_size <- if (input$sampling_method == "cluster") input$cluster_size else input$cluster_size_stratified
        icc <- if (input$sampling_method == "cluster") input$intraclass_correlation else input$intraclass_correlation_stratified
        dess <- 1 + (cluster_size - 1) * icc
        
        explanation <- paste0(
          explanation, 
          " Design Effect (DEFF): ", round(dess, 3), 
          " (Cluster Size: ", cluster_size, ", ICC: ", round(icc, 3), ")."
        )
      }
      shinyjs::runjs(paste0("
        $('#", session$ns("sample_size_explanation"), "').text('", explanation, "');
      "))
      
      # Show results container
      showElement("results_container")
      
      # Show stratified breakdown if applicable
      if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
        showElement("stratified_breakdown_section")
      } else {
        hideElement("stratified_breakdown_section")
      }
    })
    
    # Sample size plot
    output$sample_size_plot <- renderPlotly({
      results <- calculate_sample_size()
      
      if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
        # Specialized plot for stratified sampling
        p <- create_stratified_plot(results)
      } else {
        # Standard plot for other sampling methods
        p <- create_standard_plot(results)
      }
      
      ggplotly(p, tooltip = c("x", "y", "text"))
    })
    
    # Function to create stratified sampling plot
    create_stratified_plot <- function(results) {
      # Get strata data - use uploaded data if available
      if (input$sampling_method == "stratified") {
        # Use uploaded data if data input method is upload and columns are selected
        if (input$data_input_method == "upload" && 
            !is.null(uploaded_data()) && 
            input$stratified_strata_column != "" && 
            input$stratified_population_column != "") {
          
          # Extract strata data from uploaded file
          strata_col <- input$stratified_strata_column
          pop_col <- input$stratified_population_column
          
          # Create strata summary using base R approach to avoid dplyr conflicts
          current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                        by = list(Stratum = uploaded_data()[[strata_col]]), 
                                        FUN = sum, na.rm = TRUE)
          names(current_strata_data)[2] <- "Population"
        } else {
          current_strata_data <- strata_data()
        }
      } else {
        # Use uploaded data if data input method is upload and columns are selected
        if (input$data_input_method == "upload" && 
            !is.null(uploaded_data()) && 
            input$stratified_cluster_strata_column != "" && 
            input$stratified_cluster_population_column != "") {
          
          # Extract strata data from uploaded file
          strata_col <- input$stratified_cluster_strata_column
          pop_col <- input$stratified_cluster_population_column
          
          # Create strata summary using base R approach to avoid dplyr conflicts
          current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                        by = list(Stratum = uploaded_data()[[strata_col]]), 
                                        FUN = sum, na.rm = TRUE)
          names(current_strata_data)[2] <- "Population"
        } else {
          current_strata_data <- strata_data_cluster()
        }
      }
      
      
      # Create data for visualization
      sample_sizes <- seq(100, min(results$population_size, 5000), by = 100)
      
      # Calculate margins for each stratum
      plot_data <- data.frame()
      colors <- c("#2E86AB", "#C73E1D", "#F18F01", "#C73E1D", "#7209B7", "#F72585", "#4CC9F0", "#06FFA5")
      
      for (i in 1:nrow(current_strata_data)) {
        stratum_name <- current_strata_data$Stratum[i]
        stratum_pop <- current_strata_data$Population[i]
        
        # Calculate sample size for this stratum
        stratum_sample_size <- calculate_simple_random_sample_size(
          stratum_pop, 
          results$expected_proportion, 
          results$confidence_level, 
          results$margin_error,
          results$non_response_rate
        )
        
        # Calculate margins for this stratum
        margins <- sapply(sample_sizes, function(n) {
          z <- qnorm(1 - (1 - results$confidence_level) / 2)
          sqrt((z^2 * results$expected_proportion * (1 - results$expected_proportion)) / n)
        })
        
        stratum_data <- data.frame(
          SampleSize = sample_sizes,
          MarginOfError = margins,
          Stratum = stratum_name,
          StratumSampleSize = stratum_sample_size,
          StratumPopulation = stratum_pop
        )
        
        plot_data <- rbind(plot_data, stratum_data)
      }
      
      # Calculate total sample size and margin
      total_sample_size <- results$sample_size
      z <- qnorm(1 - (1 - results$confidence_level) / 2)
      total_margin <- sqrt((z^2 * results$expected_proportion * (1 - results$expected_proportion)) / total_sample_size)
      
      # Create data for dots (actual sample sizes for each stratum)
      dots_data <- data.frame()
      for (i in 1:nrow(current_strata_data)) {
        stratum_name <- current_strata_data$Stratum[i]
        stratum_pop <- current_strata_data$Population[i]
        
        # Use appropriate calculation method based on sampling type
        if (input$sampling_method == "stratified") {
          stratum_sample_size <- calculate_simple_random_sample_size(
            stratum_pop, 
            results$expected_proportion, 
            results$confidence_level, 
            results$margin_error,
            results$non_response_rate
          )
        } else if (input$sampling_method == "stratified_cluster") {
          # Use uploaded data parameters if data input method is upload
          if (input$data_input_method == "upload" && 
              !is.null(uploaded_data()) && 
              input$stratified_cluster_strata_column != "" && 
              input$stratified_cluster_population_column != "") {
            cluster_size <- input$cluster_size_stratified_upload
            icc <- input$intraclass_correlation_stratified_upload
          } else {
            cluster_size <- input$cluster_size_stratified
            icc <- input$intraclass_correlation_stratified
          }
          
          stratum_sample_size <- calculate_cluster_sample_size(
            stratum_pop, 
            results$expected_proportion, 
            results$confidence_level, 
            results$margin_error,
            cluster_size,
            icc,
            results$non_response_rate
          )
        }
        
        # Calculate margin for this stratum's sample size
        z <- qnorm(1 - (1 - results$confidence_level) / 2)
        stratum_margin <- sqrt((z^2 * results$expected_proportion * (1 - results$expected_proportion)) / stratum_sample_size)
        
        dots_data <- rbind(dots_data, data.frame(
          SampleSize = stratum_sample_size,
          MarginOfError = stratum_margin,
          Stratum = stratum_name,
          Population = stratum_pop
        ))
      }
      
      # Create the plot with dots instead of lines
      p <- ggplot(dots_data, aes(x = SampleSize, y = MarginOfError, color = Stratum)) +
        geom_point(size = 4, alpha = 0.8) +
        geom_hline(yintercept = results$margin_error, linetype = "dashed", color = "#333333", linewidth = 1) +
        labs(
          title = "Sample Size vs Margin of Error by Stratum",
          x = "Sample Size",
          y = "Margin of Error",
          subtitle = paste("Target margin of error:", results$margin_error * 100, "%"),
          color = "Stratum"
        ) +
        theme_sampling() +
        theme(legend.position = "bottom") +
        scale_color_manual(values = colors[1:nrow(current_strata_data)])
      
      # Add total sample size point
      p <- p + annotate("point", x = total_sample_size, y = total_margin, 
                       color = "#000000", size = 5, shape = 18) +
        annotate("text", x = total_sample_size, y = total_margin + 0.002, 
                label = "Total", color = "#000000", size = 3, hjust = 0.5, vjust = 0)
      
      return(p)
    }
    
    # Function to create standard plot for non-stratified methods
    create_standard_plot <- function(results) {
      # Create data for visualization
      sample_sizes <- seq(100, min(results$population_size, 5000), by = 100)
      margins <- sapply(sample_sizes, function(n) {
        z <- qnorm(1 - (1 - results$confidence_level) / 2)
        sqrt((z^2 * results$expected_proportion * (1 - results$expected_proportion)) / n)
      })
      
      plot_data <- data.frame(
        SampleSize = sample_sizes,
        MarginOfError = margins
      )
      
      p <- ggplot(plot_data, aes(x = SampleSize, y = MarginOfError)) +
        geom_line(color = "#2E86AB", linewidth = 1) +
        annotate("point", x = results$sample_size, y = results$margin_error, 
                 color = "#C73E1D", size = 3) +
        geom_hline(yintercept = results$margin_error, linetype = "dashed", color = "#C73E1D") +
        labs(
          title = "Sample Size vs Margin of Error",
          x = "Sample Size",
          y = "Margin of Error",
          subtitle = paste("Target margin of error:", results$margin_error * 100, "%")
        ) +
        theme_sampling()
      
      return(p)
    }
    
    # Calculation table
    output$calculation_table <- DT::renderDataTable({
      results <- calculate_sample_size()
      
      # Calculate intermediate values
      z <- qnorm(1 - (1 - results$confidence_level) / 2)
      n0 <- (z^2 * results$expected_proportion * (1 - results$expected_proportion)) / (results$margin_error^2)
      n_adjusted <- n0 / (1 + (n0 - 1) / results$population_size)
      n_final <- n_adjusted / (1 - results$non_response_rate)
      
      # Base parameters
      base_params <- c(
        "Population Size (N)",
        "Confidence Level",
        "Margin of Error",
        "Expected Proportion (p)",
        "Non-Response Rate",
        "Z-score (α/2)",
        "Initial Sample Size (n₀)",
        "Finite Population Correction",
        "Sample Size (before non-response)",
        "Final Sample Size (n)"
      )
      
      base_values <- c(
        format(results$population_size, big.mark = ","),
        paste0(results$confidence_level * 100, "%"),
        paste0(results$margin_error * 100, "%"),
        results$expected_proportion,
        paste0(results$non_response_rate * 100, "%"),
        round(z, 3),
        round(n0, 0),
        round(1 + (n0 - 1) / results$population_size, 3),
        round(n_adjusted, 0),
        results$sample_size
      )
      
      base_formulas <- c(
        "Given",
        "Given",
        "Given",
        "Given",
        "Given",
        "qnorm(1 - α/2)",
        "z²p(1-p)/e²",
        "1 + (n₀-1)/N",
        "n₀ / FPC",
        "n_adjusted / (1 - non_response_rate)"
      )
      
      # Add cluster-specific parameters if applicable
      if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
        # Get cluster size and ICC based on method and data input
        if (input$sampling_method == "cluster") {
          cluster_size <- input$cluster_size
          icc <- input$intraclass_correlation
        } else if (input$sampling_method == "stratified_cluster") {
          # Use uploaded data parameters if data input method is upload
          if (input$data_input_method == "upload" && 
              !is.null(uploaded_data()) && 
              input$stratified_cluster_strata_column != "" && 
              input$stratified_cluster_population_column != "") {
            cluster_size <- input$cluster_size_stratified_upload
            icc <- input$intraclass_correlation_stratified_upload
          } else {
            cluster_size <- input$cluster_size_stratified
            icc <- input$intraclass_correlation_stratified
          }
        }
        
        dess <- 1 + (cluster_size - 1) * icc
        
        base_params <- c(base_params, "Cluster Size", "Intraclass Correlation (ICC)", "Design Effect (DEFF)")
        base_values <- c(base_values, cluster_size, round(icc, 3), round(dess, 3))
        base_formulas <- c(base_formulas, "Given", "Given", "1 + (m-1) × ICC")
      }
      
      calc_data <- data.frame(
        Parameter = base_params,
        Value = base_values,
        Formula = base_formulas
      )
      
      DT::datatable(
        calc_data,
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })
    
    # Strata tables
    output$strata_table <- DT::renderDataTable({
      if (input$sampling_method == "stratified") {
        DT::datatable(
          strata_data(),
          editable = list(target = "cell", disable = list(columns = c())),  # Allow editing all columns
          options = list(
            pageLength = 10,
            dom = 't',
            ordering = FALSE,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '40%', targets = 0, className = 'dt-center'),
              list(width = '60%', targets = 1, className = 'dt-center')
            )
          ),
          rownames = FALSE,
          selection = 'none',
          class = 'excel-table'
        ) %>%
          DT::formatStyle(1, fontWeight = 'bold') %>%
          DT::formatStyle(2, textAlign = 'right') %>%
          DT::formatCurrency(2, currency = '', digits = 0)
      }
    }, server = FALSE)
    
    output$strata_table_cluster <- DT::renderDataTable({
      if (input$sampling_method == "stratified_cluster") {
        DT::datatable(
          strata_data_cluster(),
          editable = list(target = "cell", disable = list(columns = c())),  # Allow editing all columns
          options = list(
            pageLength = 10,
            dom = 't',
            ordering = FALSE,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '40%', targets = 0, className = 'dt-center'),
              list(width = '60%', targets = 1, className = 'dt-center')
            )
          ),
          rownames = FALSE,
          selection = 'none',
          class = 'excel-table'
        ) %>%
          DT::formatStyle(1, fontWeight = 'bold') %>%
          DT::formatStyle(2, textAlign = 'right') %>%
          DT::formatCurrency(2, currency = '', digits = 0)
      }
    }, server = FALSE)
    
    # Handle strata table edits
    observeEvent(input$strata_table_cell_edit, {
      if (input$sampling_method == "stratified") {
        info <- input$strata_table_cell_edit
        current_data <- strata_data()
        
        # Validate row and column indices
        if (info$row > 0 && info$row <= nrow(current_data) && 
            info$col >= 0 && info$col < ncol(current_data)) {
          
          # Check which column was edited
          if (info$col == 1) {  # Population column
            # Validate the input for population
            new_value <- as.numeric(info$value)
            if (!is.na(new_value) && new_value > 0) {
              current_data[info$row, info$col + 1] <- new_value  # Convert to 1-based indexing
              strata_data(current_data)
              
              # Update Population Size (N) input based on total strata population
              total_population <- sum(current_data$Population)
              updateNumericInput(session, "population_size", value = total_population)
              
              showNotification("Population updated successfully!", type = "message", duration = 2)
            } else {
              showNotification("Please enter a valid positive number for population size.", type = "error", duration = 3)
              strata_data(strata_data())  # Revert
            }
          } else if (info$col == 0) {  # Stratum name column
            # Validate the input for stratum name
            new_name <- trimws(as.character(info$value))
            if (nchar(new_name) > 0 && !is.na(new_name)) {
              current_data[info$row, info$col + 1] <- new_name  # Convert to 1-based indexing
              strata_data(current_data)
              showNotification("Stratum name updated successfully!", type = "message", duration = 2)
            } else {
              showNotification("Please enter a valid stratum name.", type = "error", duration = 3)
              strata_data(strata_data())  # Revert
            }
          }
        } else {
          showNotification("Invalid cell selection. Please try again.", type = "error", duration = 3)
        }
      }
    })
    
    observeEvent(input$strata_table_cluster_cell_edit, {
      if (input$sampling_method == "stratified_cluster") {
        info <- input$strata_table_cluster_cell_edit
        current_data <- strata_data_cluster()
        
        # Validate row and column indices
        if (info$row > 0 && info$row <= nrow(current_data) && 
            info$col >= 0 && info$col < ncol(current_data)) {
          
          # Check which column was edited
          if (info$col == 1) {  # Population column
            # Validate the input for population
            new_value <- as.numeric(info$value)
            if (!is.na(new_value) && new_value > 0) {
              current_data[info$row, info$col + 1] <- new_value  # Convert to 1-based indexing
              strata_data_cluster(current_data)
              
              # Update Population Size (N) input based on total strata population
              total_population <- sum(current_data$Population)
              updateNumericInput(session, "population_size", value = total_population)
              
              showNotification("Population updated successfully!", type = "message", duration = 2)
            } else {
              showNotification("Please enter a valid positive number for population size.", type = "error", duration = 3)
              strata_data_cluster(strata_data_cluster())  # Revert
            }
          } else if (info$col == 0) {  # Stratum name column
            # Validate the input for stratum name
            new_name <- trimws(as.character(info$value))
            if (nchar(new_name) > 0 && !is.na(new_name)) {
              current_data[info$row, info$col + 1] <- new_name  # Convert to 1-based indexing
              strata_data_cluster(current_data)
              showNotification("Stratum name updated successfully!", type = "message", duration = 2)
            } else {
              showNotification("Please enter a valid stratum name.", type = "error", duration = 3)
              strata_data_cluster(strata_data_cluster())  # Revert
            }
          }
        } else {
          showNotification("Invalid cell selection. Please try again.", type = "error", duration = 3)
        }
      }
    })
    
    # Stratified breakdown table
    output$stratified_breakdown_table <- DT::renderDataTable({
      if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
        results <- calculate_sample_size()
        
        # Get strata information
        if (input$sampling_method == "stratified") {
          # Use uploaded data if data input method is upload and columns are selected
          if (input$data_input_method == "upload" && 
              !is.null(uploaded_data()) && 
              input$stratified_strata_column != "" && 
              input$stratified_population_column != "") {
            
            # Extract strata data from uploaded file
            strata_col <- input$stratified_strata_column
            pop_col <- input$stratified_population_column
            
            # Create strata summary using base R approach to avoid dplyr conflicts
            current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                          by = list(Stratum = uploaded_data()[[strata_col]]), 
                                          FUN = sum, na.rm = TRUE)
            names(current_strata_data)[2] <- "Population"
          } else {
            current_strata_data <- strata_data()
          }
          method_type <- "stratified"
          cluster_size <- 10  # Not used for regular stratified
          icc <- 0.06  # Not used for regular stratified
        } else {
          # Use uploaded data if data input method is upload and columns are selected
          if (input$data_input_method == "upload" && 
              !is.null(uploaded_data()) && 
              input$stratified_cluster_strata_column != "" && 
              input$stratified_cluster_population_column != "") {
            
            # Extract strata data from uploaded file
            strata_col <- input$stratified_cluster_strata_column
            pop_col <- input$stratified_cluster_population_column
            
            # Create strata summary using base R approach to avoid dplyr conflicts
            current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                          by = list(Stratum = uploaded_data()[[strata_col]]), 
                                          FUN = sum, na.rm = TRUE)
            names(current_strata_data)[2] <- "Population"
          } else {
            current_strata_data <- strata_data_cluster()
          }
          method_type <- "stratified_cluster"
          # Use uploaded data parameters if data input method is upload
          if (input$data_input_method == "upload" && 
              !is.null(uploaded_data()) && 
              input$stratified_cluster_strata_column != "" && 
              input$stratified_cluster_population_column != "") {
            cluster_size <- input$cluster_size_stratified_upload
            icc <- input$intraclass_correlation_stratified_upload
          } else {
            cluster_size <- input$cluster_size_stratified
            icc <- input$intraclass_correlation_stratified
          }
        }
        
        
        # Create breakdown table
        breakdown_data <- create_stratified_breakdown(
          current_strata_data,
          results$expected_proportion,
          results$confidence_level,
          results$margin_error,
          method_type,
          cluster_size,
          icc,
          results$non_response_rate
        )
        
        DT::datatable(
          breakdown_data,
          options = list(
            pageLength = 10,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(1, fontWeight = 'bold') %>%
          DT::formatRound(c("Population_Size", "Sample_Size"), 0) %>%
          DT::formatRound(c("Sampling_Fraction", "Confidence_Level", "Margin_of_Error", "Expected_Proportion", "Non_Response_Rate"), 2) %>%
          {if (input$sampling_method == "stratified_cluster") {
            DT::formatRound(., c("Cluster_Size", "ICC", "Design_Effect"), 3)
          } else {
            .
          }}
      }
    })
    
    # Download handlers
    output$download_excel <- downloadHandler(
      filename = function() {
        study_name_clean <- if (input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Sample_Size")
        } else {
          "sample_size_calculation"
        }
        paste0(study_name_clean, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        results <- calculate_sample_size()
        z <- qnorm(1 - (1 - results$confidence_level) / 2)
        n0 <- (z^2 * results$expected_proportion * (1 - results$expected_proportion)) / (results$margin_error^2)
        
        # Base parameters
        base_params <- c(
          "Population Size (N)",
          "Confidence Level",
          "Margin of Error",
          "Expected Proportion (p)",
          "Non-Response Rate",
          "Z-score (α/2)",
          "Initial Sample Size (n₀)",
          "Final Sample Size (n)"
        )
        
        base_values <- c(
          results$population_size,
          results$confidence_level,
          results$margin_error,
          results$expected_proportion,
          results$non_response_rate,
          z,
          n0,
          results$sample_size
        )
        
        # Add cluster-specific parameters if applicable
        if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
          cluster_size <- if (input$sampling_method == "cluster") input$cluster_size else input$cluster_size_stratified
          icc <- if (input$sampling_method == "cluster") input$intraclass_correlation else input$intraclass_correlation_stratified
          dess <- 1 + (cluster_size - 1) * icc
          
          base_params <- c(base_params, "Cluster Size", "Intraclass Correlation (ICC)", "Design Effect (DEFF)")
          base_values <- c(base_values, cluster_size, icc, dess)
        }
        
        calc_data <- data.frame(
          Parameter = base_params,
          Value = base_values
        )
        
        writexl::write_xlsx(calc_data, file)
      }
    )
    
    # Download stratified breakdown
    output$download_stratified_breakdown <- downloadHandler(
      filename = function() {
        study_name_clean <- if (input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Sample_Size")
        } else {
          "stratified_breakdown"
        }
        paste0(study_name_clean, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
          results <- calculate_sample_size()
          
          # Get strata information - use uploaded data if available
          if (input$sampling_method == "stratified") {
            # Use uploaded data if data input method is upload and columns are selected
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_strata_column != "" && 
                input$stratified_population_column != "") {
              
              # Extract strata data from uploaded file
              strata_col <- input$stratified_strata_column
              pop_col <- input$stratified_population_column
              
              # Create strata summary using base R approach to avoid dplyr conflicts
              current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                            by = list(Stratum = uploaded_data()[[strata_col]]), 
                                            FUN = sum, na.rm = TRUE)
              names(current_strata_data)[2] <- "Population"
            } else {
              current_strata_data <- strata_data()
            }
            method_type <- "stratified"
            cluster_size <- 10  # Not used for regular stratified
            icc <- 0.06  # Not used for regular stratified
          } else {
            # Use uploaded data if data input method is upload and columns are selected
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_cluster_strata_column != "" && 
                input$stratified_cluster_population_column != "") {
              
              # Extract strata data from uploaded file
              strata_col <- input$stratified_cluster_strata_column
              pop_col <- input$stratified_cluster_population_column
              
              # Create strata summary using base R approach to avoid dplyr conflicts
              current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                            by = list(Stratum = uploaded_data()[[strata_col]]), 
                                            FUN = sum, na.rm = TRUE)
              names(current_strata_data)[2] <- "Population"
            } else {
              current_strata_data <- strata_data_cluster()
            }
            method_type <- "stratified_cluster"
            # Use uploaded data parameters if data input method is upload
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_cluster_strata_column != "" && 
                input$stratified_cluster_population_column != "") {
              cluster_size <- input$cluster_size_stratified_upload
              icc <- input$intraclass_correlation_stratified_upload
            } else {
              cluster_size <- input$cluster_size_stratified
              icc <- input$intraclass_correlation_stratified
            }
          }
          
          # Create breakdown table
          breakdown_data <- create_stratified_breakdown(
            current_strata_data,
            results$expected_proportion,
            results$confidence_level,
            results$margin_error,
            method_type,
            cluster_size,
            icc,
            results$non_response_rate
          )
          
          # Create a list with both summary and breakdown
          sheets_list <- list(
            "Summary" = data.frame(
              Parameter = c("Total Population", "Total Sample Size", "Number of Strata", "Method"),
              Value = c(sum(current_strata_data$Population), results$sample_size, nrow(current_strata_data), results$method_name)
            ),
            "Stratified_Breakdown" = breakdown_data
          )
          
          writexl::write_xlsx(sheets_list, file)
        }
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        study_name_clean <- if (input$study_name != "") {
          paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Sample_Size")
        } else {
          "sample_size_report"
        }
        paste0(study_name_clean, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        results <- calculate_sample_size()
         
        # Build the same explanation string shown in the UI
        explanation_text <- paste0(
          "For a population of ", format(results$population_size, big.mark = ","), 
          " with ", results$confidence_level * 100, "% confidence level and ", 
          results$margin_error * 100, "% margin of error, you need a sample size of ",
          results$sample_size, " using ", results$method_name, 
          " (adjusted for ", input$non_response_rate, "% non-response rate)."
        )
        
        # Add design effect info for cluster methods
        if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
          cluster_size <- if (input$sampling_method == "cluster") input$cluster_size else input$cluster_size_stratified
          icc <- if (input$sampling_method == "cluster") input$intraclass_correlation else input$intraclass_correlation_stratified
          dess <- 1 + (cluster_size - 1) * icc
          
          explanation_text <- paste0(
            explanation_text,
            " Design Effect (DEFF): ", round(dess, 3),
            " Cluster Size: ", cluster_size, ", ICC: ", round(icc, 3), "."
          )
        }
        
        # Generate calculation data for the table
        z <- qnorm(1 - (1 - results$confidence_level) / 2)
        n0 <- (z^2 * results$expected_proportion * (1 - results$expected_proportion)) / (results$margin_error^2)
        n_adjusted <- n0 / (1 + (n0 - 1) / results$population_size)
        n_final <- n_adjusted / (1 - results$non_response_rate)
        
        # Create base calculation table
        if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
          # For stratified methods, get strata information to calculate average stratum size
          if (input$sampling_method == "stratified") {
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_strata_column != "" && 
                input$stratified_population_column != "") {
              strata_col <- input$stratified_strata_column
              pop_col <- input$stratified_population_column
              current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                            by = list(Stratum = uploaded_data()[[strata_col]]), 
                                            FUN = sum, na.rm = TRUE)
              names(current_strata_data)[2] <- "Population"
            } else {
              current_strata_data <- strata_data()
            }
          } else {
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_cluster_strata_column != "" && 
                input$stratified_cluster_population_column != "") {
              strata_col <- input$stratified_cluster_strata_column
              pop_col <- input$stratified_cluster_population_column
              current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                            by = list(Stratum = uploaded_data()[[strata_col]]), 
                                            FUN = sum, na.rm = TRUE)
              names(current_strata_data)[2] <- "Population"
            } else {
              current_strata_data <- strata_data_cluster()
            }
          }
          
          # Calculate average stratum size for per-stratum calculations
          avg_stratum_size <- mean(current_strata_data$Population)
          
          # Recalculate sample size for average stratum
          n0_avg <- (z^2 * results$expected_proportion * (1 - results$expected_proportion)) / (results$margin_error^2)
          n_adjusted_avg <- n0_avg / (1 + (n0_avg - 1) / avg_stratum_size)
          
          base_params <- c(
            "Average Stratum Population (N)",
            "Confidence Level",
            "Margin of Error", 
            "Expected Proportion (p)",
            "Non-Response Rate",
            "Z-score (alpha/2)",
            "Initial Sample Size (n0)"
          )
          
          base_values <- c(
            format(round(avg_stratum_size, 0), big.mark = ","),
            paste0(results$confidence_level * 100, "%"),
            paste0(results$margin_error * 100, "%"),
            as.character(results$expected_proportion),
            paste0(results$non_response_rate * 100, "%"),
            as.character(round(z, 3)),
            as.character(round(n0_avg, 0))
          )
        } else {
          base_params <- c(
            "Population Size (N)",
            "Confidence Level",
            "Margin of Error", 
            "Expected Proportion (p)",
            "Non-Response Rate",
            "Z-score (alpha/2)",
            "Initial Sample Size (n0)"
          )
          
          base_values <- c(
            format(results$population_size, big.mark = ","),
            paste0(results$confidence_level * 100, "%"),
            paste0(results$margin_error * 100, "%"),
            as.character(results$expected_proportion),
            paste0(results$non_response_rate * 100, "%"),
            as.character(round(z, 3)),
            as.character(round(n0, 0))
          )
        }
        
        base_formulas <- c(
          "Given",
          "Given", 
          "Given",
          "Given",
          "Given",
          "qnorm(1 - alpha/2)",
          "z^2*p*(1-p)/e^2"
        )
        
        # Add cluster-specific parameters if applicable (insert after initial sample size)
        if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
          cluster_size <- if (input$sampling_method == "cluster") input$cluster_size else input$cluster_size_stratified
          icc <- if (input$sampling_method == "cluster") input$intraclass_correlation else input$intraclass_correlation_stratified
          dess <- 1 + (cluster_size - 1) * icc
          
          # Insert cluster parameters after initial sample size
          cluster_params <- c("Cluster Size", "Intraclass Correlation (ICC)", "Design Effect (DEFF)")
          cluster_values <- c(as.character(cluster_size), as.character(round(icc, 3)), as.character(round(dess, 3)))
          cluster_formulas <- c("Given", "Given", "1 + (m-1) × ICC")
          
          # Combine base parameters with cluster parameters (base_params only has 7 elements, so no need for [8:length])
          all_params <- c(base_params, cluster_params)
          all_values <- c(base_values, cluster_values)
          all_formulas <- c(base_formulas, cluster_formulas)
        } else {
          all_params <- base_params
          all_values <- base_values
          all_formulas <- base_formulas
        }
        
        # Add remaining parameters
        if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
          # Use the average stratum calculations for remaining parameters
          remaining_params <- c(
            "Finite Population Correction",
            "Sample Size per Stratum (before non-response)",
            "Number of Strata",
            "Total Sample Size (n)"
          )
          
          remaining_values <- c(
            as.character(round(1 + (n0_avg - 1) / avg_stratum_size, 3)),
            as.character(round(n_adjusted_avg, 0)),
            as.character(nrow(current_strata_data)),
            as.character(results$sample_size)
          )
          
          remaining_formulas <- c(
            "1 + (n0-1)/N",
            "n0 / FPC",
            "Count of strata",
            "Sample per Stratum × Number of Strata"
          )
          
          # For cluster sampling, adjust the final calculation
          if (input$sampling_method == "stratified_cluster") {
            remaining_formulas[4] <- "Sample per Stratum × Number of Strata × DEFF"
          }
        } else {
          remaining_params <- c(
            "Finite Population Correction",
            "Sample Size (before non-response)",
            "Final Sample Size (n)"
          )
          
          remaining_values <- c(
            as.character(round(1 + (n0 - 1) / results$population_size, 3)),
            as.character(round(n_adjusted, 0)),
            as.character(results$sample_size)
          )
          
          remaining_formulas <- c(
            "1 + (n0-1)/N",
            "n0 / FPC",
            "n_adjusted / (1 - non_response_rate)"
          )
          
          # For cluster sampling, adjust the final calculation
          if (input$sampling_method == "cluster") {
            remaining_formulas[3] <- "n_adjusted / (1 - non_response_rate) × DEFF"
          }
        }
        
        # Combine all parameters
        final_params <- c(all_params, remaining_params)
        final_values <- c(all_values, remaining_values)
        final_formulas <- c(all_formulas, remaining_formulas)
        
        # Create the final table
        calc_table <- data.frame(
          Parameter = final_params,
          Value = final_values,
          Formula = final_formulas
        )
        
        # Convert to markdown table
        calc_text <- paste0(
          "| Parameter | Value | Formula |\n",
          "|-----------|-------|----------|\n",
          paste(apply(calc_table, 1, function(row) {
            paste0("| ", row[1], " | ", row[2], " | ", row[3], " |")
          }), collapse = "\n")
        )
        
        # Generate stratified breakdown if applicable
        stratified_breakdown_text <- ""
        if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
          # Get strata information
          if (input$sampling_method == "stratified") {
            # Use uploaded data if data input method is upload and columns are selected
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_strata_column != "" && 
                input$stratified_population_column != "") {
              
              # Extract strata data from uploaded file
              strata_col <- input$stratified_strata_column
              pop_col <- input$stratified_population_column
              
              # Create strata summary using base R approach to avoid dplyr conflicts
              current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                            by = list(Stratum = uploaded_data()[[strata_col]]), 
                                            FUN = sum, na.rm = TRUE)
              names(current_strata_data)[2] <- "Population"
            } else {
              current_strata_data <- strata_data()
            }
            method_type <- "stratified"
            cluster_size <- 10  # Not used for regular stratified
            icc <- 0.06  # Not used for regular stratified
          } else {
            # Use uploaded data if data input method is upload and columns are selected
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_cluster_strata_column != "" && 
                input$stratified_cluster_population_column != "") {
              
              # Extract strata data from uploaded file
              strata_col <- input$stratified_cluster_strata_column
              pop_col <- input$stratified_cluster_population_column
              
              # Create strata summary using base R approach to avoid dplyr conflicts
              current_strata_data <- aggregate(uploaded_data()[[pop_col]], 
                                            by = list(Stratum = uploaded_data()[[strata_col]]), 
                                            FUN = sum, na.rm = TRUE)
              names(current_strata_data)[2] <- "Population"
            } else {
              current_strata_data <- strata_data_cluster()
            }
            method_type <- "stratified_cluster"
            # Use uploaded data parameters if data input method is upload
            if (input$data_input_method == "upload" && 
                !is.null(uploaded_data()) && 
                input$stratified_cluster_strata_column != "" && 
                input$stratified_cluster_population_column != "") {
              cluster_size <- input$cluster_size_stratified_upload
              icc <- input$intraclass_correlation_stratified_upload
            } else {
              cluster_size <- input$cluster_size_stratified
              icc <- input$intraclass_correlation_stratified
            }
          }
          
          # Create breakdown table
          breakdown_data <- create_stratified_breakdown(
            current_strata_data,
            results$expected_proportion,
            results$confidence_level,
            results$margin_error,
            method_type,
            cluster_size,
            icc,
            results$non_response_rate
          )
          
          # Convert breakdown to markdown table
          breakdown_table <- data.frame(
            Stratum = breakdown_data$Stratum,
            Population_Size = format(breakdown_data$Population_Size, big.mark = ","),
            Sample_Size = breakdown_data$Sample_Size,
            Sampling_Fraction = paste0(breakdown_data$Sampling_Fraction, "%"),
            Confidence_Level = paste0(breakdown_data$Confidence_Level, "%"),
            Margin_of_Error = paste0(breakdown_data$Margin_of_Error, "%"),
            stringsAsFactors = FALSE
          )
          
          # Add cluster-specific columns if applicable
          if (method_type == "stratified_cluster") {
            breakdown_table$Cluster_Size <- breakdown_data$Cluster_Size
            breakdown_table$ICC <- round(breakdown_data$ICC, 3)
            breakdown_table$Design_Effect <- round(breakdown_data$Design_Effect, 3)
          }
          
          # Create markdown table for breakdown
          breakdown_headers <- c("Stratum", "Population Size", "Sample Size", "Sampling Fraction", "Confidence Level", "Margin of Error")
          if (method_type == "stratified_cluster") {
            breakdown_headers <- c(breakdown_headers, "Cluster Size", "ICC", "Design Effect")
          }
          
          breakdown_text <- paste0(
            "| ", paste(breakdown_headers, collapse = " | "), " |\n",
            "|", paste(rep("---", length(breakdown_headers)), collapse = "|"), "|\n",
            paste(apply(breakdown_table, 1, function(row) {
              paste0("| ", paste(row, collapse = " | "), " |")
            }), collapse = "\n")
          )
          
          stratified_breakdown_text <- paste0(
            "\n\n## Stratified Sampling Breakdown\n\n",
            breakdown_text, "\n\n",
            "### Summary\n\n",
            "- **Total Population:** ", format(sum(current_strata_data$Population), big.mark = ","), "\n",
            "- **Total Sample Size:** ", results$sample_size, "\n",
            "- **Number of Strata:** ", nrow(current_strata_data), "\n",
            "- **Sampling Method:** ", results$method_name
          )
        }
        
        # Create temporary Rmd file including the explanation and calculations
        rmd_content <- paste0(
          "---\n",
          "title: '", if (input$study_name != "") paste0(input$study_name, " Sample Size Calculation Report") else "Sample Size Calculation Report", "'\n",
          "subtitle: '", "ATR Consulting", "'\n",
          "date: '", Sys.Date(), "'\n",
          "output: pdf_document\n",
          "---\n\n",
          "## Study Information\n\n",
          "**Study Name:** ", if (input$study_name != "") input$study_name else "Not specified", "\n\n",
          "## Study Parameters\n\n",
          "- **Population Size:** ", format(results$population_size, big.mark = ","), "\n",
          "- **Confidence Level:** ", results$confidence_level * 100, "%\n",
          "- **Margin of Error:** ", results$margin_error * 100, "%\n",
          "- **Expected Proportion:** ", results$expected_proportion, "\n",
          "- **Non-Response Rate:** ", results$non_response_rate * 100, "%\n",
          "- **Sampling Method:** ", results$method_name,
          if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
            paste0("\n- **Number of Strata:** ", 
                   if (input$sampling_method == "stratified") {
                     if (input$data_input_method == "upload" && 
                         !is.null(uploaded_data()) && 
                         input$stratified_strata_column != "" && 
                         input$stratified_population_column != "") {
                       length(unique(uploaded_data()[[input$stratified_strata_column]]))
                     } else {
                       nrow(strata_data())
                     }
                   } else {
                     if (input$data_input_method == "upload" && 
                         !is.null(uploaded_data()) && 
                         input$stratified_cluster_strata_column != "" && 
                         input$stratified_cluster_population_column != "") {
                       length(unique(uploaded_data()[[input$stratified_cluster_strata_column]]))
                     } else {
                       nrow(strata_data_cluster())
                     }
                   })
          } else {
            ""
          }, "\n\n",
          "## Required Sample Size\n\n",
          "**", results$sample_size, "**\n\n",
          "## Explanation\n\n",
          explanation_text, "\n\n",
          "## Detailed Calculations\n\n",
          if (input$sampling_method %in% c("stratified", "stratified_cluster")) {
            paste0(
              "### Per-Stratum Calculations\n\n",
              "The following calculations show how the sample size is determined for each stratum:\n\n",
              calc_text, "\n\n",
              "### Total Sample Size Calculation\n\n",
              "The total sample size is calculated by multiplying the per-stratum sample size by the number of strata:\n\n",
              "**Total Sample Size = Sample per Stratum × Number of Strata", 
              if (input$sampling_method == "stratified_cluster") " × Design Effect", 
              "**\n\n"
            )
          } else {
            paste0(calc_text, "\n\n")
          },
          stratified_breakdown_text, "\n\n",
          "### Notes\n\n",
          "- **Z-score (alpha/2):** Critical value for the given confidence level\n",
          "- **Initial Sample Size (n0):** Sample size before finite population correction\n",
          "- **Finite Population Correction:** Adjustment factor for finite populations"
        )
        
        # Add cluster sampling note if applicable
        if (input$sampling_method %in% c("cluster", "stratified_cluster")) {
          rmd_content <- paste0(rmd_content, "\n- **Design Effect (DEFF):** Adjustment factor for cluster sampling")
        }
        
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(rmd_content, temp_rmd)
        
        # Render to PDF
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }
    )
  })
}
