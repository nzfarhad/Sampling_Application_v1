# Sample Size Calculator Module
# Monitoring calculator: sample size for a target margin of error.
# The formulas are in sample_size_functions.R.

# Dashed reference line for plotly charts: horizontal at y, or vertical at x
plotly_reference_line <- function(y = NULL, x = NULL, color = "#C73E1D", dash = "dash") {
  if (!is.null(y)) {
    list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = y, y1 = y, line = list(color = color, dash = dash))
  } else {
    list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = x, x1 = x, line = list(color = color, dash = dash))
  }
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
    uiOutput(ns("handover_note")),
    
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
                radioButtons(
                  ns("indicator_type"),
                  "Key indicator",
                  choices = list(
                    "Proportion (e.g. % of households receiving aid)" = "proportion",
                    "Mean (e.g. income, yield)" = "mean"
                  ),
                  selected = "proportion",
                  inline = TRUE
                ),
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
                  conditionalPanel(
                    condition = "input.indicator_type != 'mean'",
                    ns = ns,
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
                  conditionalPanel(
                    condition = "input.indicator_type != 'mean'",
                    ns = ns,
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
                  conditionalPanel(
                    condition = "input.indicator_type == 'mean'",
                    ns = ns,
                    class = "col-md-6",
                    numericInput(
                      ns("margin_error_mean"),
                      "Margin of Error (in the indicator's units)",
                      value = 5,
                      min = 0,
                      step = 1
                    )
                  ),
                  conditionalPanel(
                    condition = "input.indicator_type == 'mean'",
                    ns = ns,
                    class = "col-md-6",
                    numericInput(
                      ns("indicator_sd"),
                      "Standard Deviation (σ)",
                      value = 50,
                      min = 0,
                      step = 1
                    ),
                    p("Take σ from a previous survey of the same indicator, or use (max - min) / 4 as a rough guess.",
                      class = "text-muted small")
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
                        class = "alert alert-warning small",
                        tags$strong("Data protection: "),
                        "This app runs on a public cloud host. Do not upload names, phone numbers, GPS points or other identifiers. ",
                        "Use anonymised IDs and link back to the full list offline."
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

                # Precision target and allocation (both stratified methods)
                conditionalPanel(
                  condition = "input.sampling_method == 'stratified' || input.sampling_method == 'stratified_cluster'",
                  ns = ns,
                  div(
                    class = "row g-2 mb-2",
                    div(
                      class = "col-md-5",
                      radioButtons(
                        ns("precision_target"),
                        "Precision target",
                        choices = list(
                          "Each stratum (report every stratum separately)" = "per_stratum",
                          "Whole population only (split one sample across strata)" = "total"
                        ),
                        selected = "per_stratum"
                      )
                    ),
                    conditionalPanel(
                      condition = "input.precision_target == 'total'",
                      ns = ns,
                      class = "col-md-7",
                      div(
                        class = "row g-2",
                        div(
                          class = "col-md-6",
                          selectInput(
                            ns("strata_allocation"),
                            "Allocation across strata",
                            choices = list(
                              "Proportional to size" = "proportional",
                              "Neyman (optimal)" = "neyman",
                              "Equal" = "equal"
                            ),
                            selected = "proportional"
                          )
                        ),
                        div(
                          class = "col-md-6",
                          numericInput(ns("min_per_stratum"), "Minimum completed interviews per stratum",
                                       value = 0, min = 0, step = 1)
                        )
                      ),
                      p("Neyman puts more interviews in large and more variable strata. It needs an expected p (or σ) per ",
                        "stratum; strata left blank use the common value, and with one common value Neyman equals proportional.",
                        class = "text-muted small")
                    )
                  ),
                  p("\"Each stratum\" sizes every stratum to reach the margin of error, so totals add up and are larger. ",
                    "\"Whole population only\" is cheaper, but single strata will have wider margins (shown in the breakdown).",
                    class = "text-muted small")
                ),

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
                        ),
                        selectInput(
                          ns("stratified_variability_column"),
                          "Expected p or σ by stratum (optional)",
                          choices = c("None (use the common value)" = ""),
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
                    "Interviews per Cluster (m)",
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
                          "Interviews per Cluster (m)",
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
                        selectInput(
                          ns("stratified_cluster_variability_column"),
                          "Expected p or σ by stratum (optional)",
                          choices = c("None (use the common value)" = ""),
                          width = "100%"
                        ),
                        numericInput(
                          ns("cluster_size_stratified_upload"),
                          "Interviews per Cluster (m)",
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
        stratum_var_id <- paste0("stratum_var_", i)
        # Keep a value already typed in when the strata inputs are redrawn
        var_value <- isolate(input[[stratum_var_id]])
        if (is.null(var_value)) var_value <- NA

        input_elements[[i]] <- div(
          class = "stratum-input-group",
          div(
            class = "row",
            div(
              class = "col-md-5",
              textInput(ns(stratum_name_id), "Stratum Name", value = data$Stratum[i])
            ),
            div(
              class = "col-md-4",
              numericInput(ns(stratum_pop_id), "Population", value = data$Population[i], min = 1)
            ),
            div(
              class = "col-md-3",
              numericInput(ns(stratum_var_id), "p or σ (optional)", value = var_value, min = 0)
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
        
        # Show uploaded data section and fill the dropdowns of both stratified methods
        if (input$sampling_method == "stratified") {
          showElement("uploaded_data_section")
        } else if (input$sampling_method == "stratified_cluster") {
          showElement("uploaded_data_cluster_section")
        }
        for (prefix in c("stratified", "stratified_cluster")) {
          updateSelectInput(session, paste0(prefix, "_strata_column"), choices = c("Select a column..." = "", column_names()))
          updateSelectInput(session, paste0(prefix, "_population_column"), choices = c("Select a column..." = "", column_names()))
          updateSelectInput(session, paste0(prefix, "_variability_column"),
                            choices = c("None (use the common value)" = "", column_names()))
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
    
    # TRUE when the stratified sample size should come from the uploaded file
    using_upload <- function(prefix = input$sampling_method) {
      isTRUE(input$data_input_method == "upload") && !is.null(uploaded_data()) &&
        !is_blank_input(input[[paste0(prefix, "_strata_column")]]) &&
        !is_blank_input(input[[paste0(prefix, "_population_column")]])
    }

    # Strata for the current method: uploaded file (summed by stratum) or manual entry.
    # Variability is the expected p (or sd for a mean) per stratum, NA for "use the common value".
    current_strata <- reactive({
      prefix <- input$sampling_method
      if (using_upload(prefix)) {
        d <- uploaded_data()
        strata_col <- input[[paste0(prefix, "_strata_column")]]
        pop_col <- input[[paste0(prefix, "_population_column")]]
        if (!is.numeric(d[[pop_col]])) stop("Population column must be numeric.")
        out <- aggregate(d[[pop_col]], by = list(Stratum = d[[strata_col]]), FUN = sum, na.rm = TRUE)
        names(out)[2] <- "Population"
        out$Stratum <- as.character(out$Stratum)
        var_col <- input[[paste0(prefix, "_variability_column")]]
        out$Variability <- NA_real_
        if (!is_blank_input(var_col)) {
          if (!is.numeric(d[[var_col]])) stop("The p / σ column must be numeric.")
          v <- tapply(d[[var_col]], as.character(d[[strata_col]]), mean, na.rm = TRUE)
          out$Variability <- as.numeric(v[out$Stratum])
        }
        out
      } else {
        out <- if (prefix == "stratified") strata_data() else strata_data_cluster()
        out$Variability <- vapply(seq_len(nrow(out)), function(i) {
          v <- input[[paste0("stratum_var_", i)]]
          if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
        }, numeric(1))
        out
      }
    })

    # Interviews per cluster and ICC for the current method
    cluster_params <- function() {
      if (input$sampling_method == "cluster") {
        list(m = input$cluster_size, icc = input$intraclass_correlation)
      } else if (using_upload("stratified_cluster")) {
        list(m = input$cluster_size_stratified_upload, icc = input$intraclass_correlation_stratified_upload)
      } else {
        list(m = input$cluster_size_stratified, icc = input$intraclass_correlation_stratified)
      }
    }

    # Calculate sample size (all outputs and downloads use this one result)
    calculate_sample_size <- reactive({
      req(input$sampling_method, input$confidence_level)
      method <- input$sampling_method
      is_mean <- identical(input$indicator_type, "mean")
      spec <- list(
        method = method,
        N = input$population_size,
        confidence_level = input$confidence_level,
        margin_error = if (is_mean) input$margin_error_mean else input$margin_error,
        indicator = if (is_mean) "mean" else "proportion",
        p = input$expected_proportion,
        sd = input$indicator_sd,
        non_response_rate = input$non_response_rate / 100,
        target = if (is.null(input$precision_target)) "per_stratum" else input$precision_target,
        allocation = if (is.null(input$strata_allocation)) "proportional" else input$strata_allocation,
        min_per_stratum = if (is.null(input$min_per_stratum) || is.na(input$min_per_stratum)) 0 else input$min_per_stratum
      )

      n_source <- NULL
      if (method == "cluster" && !is.null(uploaded_data()) &&
          !is_blank_input(input$cluster_population_column) && !is_blank_input(input$cluster_name_column)) {
        pop_col <- input$cluster_population_column
        spec$N <- sum(uploaded_data()[[pop_col]], na.rm = TRUE)
        n_source <- paste0("N = ", format(spec$N, big.mark = ","), " was taken from the uploaded file (sum of \"", pop_col,
                           "\"), not from the Population Size box.")
      }
      if (method %in% c("cluster", "stratified_cluster")) {
        cp <- cluster_params()
        spec$cluster_size <- cp$m
        spec$icc <- cp$icc
      }

      res <- tryCatch({
        if (method %in% c("stratified", "stratified_cluster")) spec$strata <- current_strata()
        monitoring_calculation(spec)
      }, error = function(e) e)
      if (inherits(res, "error")) validate(need(FALSE, conditionMessage(res)))

      res$n_source <- n_source
      if (!is.null(n_source)) res$explanation <- paste(res$explanation, n_source)
      res
    })

    # Breakdown table with readable column names and formatted margins
    display_breakdown <- function(results) {
      b <- results$breakdown
      if (is.null(b)) return(NULL)
      b$Expected_Margin_of_Error <- if (results$indicator == "mean") {
        signif(b$Expected_Margin_of_Error, 4)
      } else {
        paste0(round(b$Expected_Margin_of_Error * 100, 2), "%")
      }
      names(b) <- gsub("_pct$", " (%)", names(b))
      names(b) <- gsub("_", " ", names(b))
      b
    }

    # Update results when calculate button is clicked
    observeEvent(input$calculate, {
      results <- tryCatch(calculate_sample_size(), error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      })
      req(results)

      shinyjs::html("sample_size_result", results$sample_size)
      shinyjs::html("sample_size_explanation", htmltools::htmlEscape(results$explanation))

      # Show results container
      showElement("results_container")

      # Show stratified breakdown if applicable
      if (!is.null(results$breakdown)) {
        showElement("stratified_breakdown_section")
      } else {
        hideElement("stratified_breakdown_section")
      }
    })

    # Sample size plot (native plotly: ggplotly breaks with some ggplot2/plotly version pairs)
    output$sample_size_plot <- renderPlotly({
      results <- calculate_sample_size()
      if (!is.null(results$breakdown)) create_stratified_plot(results) else create_standard_plot(results)
    })

    moe_label <- function(results, x) {
      if (results$indicator == "mean") format(signif(x, 4)) else paste0(round(x * 100, 2), "%")
    }
    moe_layout <- function(p, results, title) {
      plotly::layout(
        p,
        title = list(text = paste0(title, "<br><sup>Target margin of error: ", moe_label(results, results$margin_error), "</sup>")),
        xaxis = list(title = "Sample Size"),
        yaxis = list(title = "Expected Margin of Error", tickformat = if (results$indicator == "mean") "" else ".1%",
                     rangemode = "tozero"),
        shapes = list(plotly_reference_line(y = results$margin_error)),
        legend = list(orientation = "h", y = -0.25),
        margin = list(t = 60)
      )
    }

    # Stratified: one dot per stratum (its sample size and expected margin of error),
    # plus a diamond for the total
    create_stratified_plot <- function(results) {
      b <- results$breakdown
      b$hover <- paste0(b$Stratum, "<br>n = ", b$Sample_Size, "<br>Margin: ", moe_label(results, b$Expected_Margin_of_Error))
      p <- plot_ly(b, x = ~Sample_Size, y = ~Expected_Margin_of_Error, color = ~Stratum, type = "scatter",
                   mode = "markers", marker = list(size = 12, opacity = 0.8), text = ~hover, hoverinfo = "text",
                   showlegend = nrow(b) <= 10) %>%
        add_trace(x = results$sample_size, y = results$moe_achieved, type = "scatter", mode = "markers+text",
                  marker = list(symbol = "diamond", size = 14, color = "#000000"), text = "Total",
                  textposition = "top center", name = "Total", color = NULL, inherit = FALSE,
                  hovertext = paste0("Total<br>n = ", results$sample_size, "<br>Margin: ", moe_label(results, results$moe_achieved)),
                  hoverinfo = "text")
      moe_layout(p, results, "Sample Size vs Margin of Error by Stratum")
    }

    # Single-stage designs: margin of error against sample size
    create_standard_plot <- function(results) {
      step <- if (!is.null(results$cluster_size)) results$cluster_size else 10
      upper <- min(results$population_size, max(1000, results$sample_size * 2))
      sample_sizes <- unique(c(seq(step, upper, length.out = 80), results$sample_size))
      sample_sizes <- round(sort(sample_sizes[sample_sizes <= results$population_size]))
      margins <- calculate_margin_of_error(
        sample_sizes, results$population_size, confidence_level = results$confidence_level,
        non_response_rate = results$non_response_rate, deff = results$deff, variance = results$variance
      )

      p <- plot_ly(x = sample_sizes, y = margins, type = "scatter", mode = "lines", name = "Expected margin of error",
                   line = list(color = "#2E86AB", width = 2),
                   hovertext = paste0("n = ", sample_sizes, "<br>Margin: ", moe_label(results, margins)), hoverinfo = "text") %>%
        add_trace(x = results$sample_size, y = results$moe_achieved, type = "scatter", mode = "markers",
                  name = "Required sample", marker = list(color = "#C73E1D", size = 10), line = NULL,
                  hovertext = paste0("Required n = ", results$sample_size, "<br>Margin: ", moe_label(results, results$moe_achieved)))
      moe_layout(p, results, "Sample Size vs Margin of Error")
    }

    # Calculation table
    output$calculation_table <- DT::renderDataTable({
      results <- calculate_sample_size()
      DT::datatable(
        results$steps,
        options = list(pageLength = 30, dom = 't', ordering = FALSE, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })

    # Stratified breakdown table
    output$stratified_breakdown_table <- DT::renderDataTable({
      results <- calculate_sample_size()
      req(!is.null(results$breakdown))
      DT::datatable(
        display_breakdown(results),
        options = list(pageLength = 25, dom = 'tp', ordering = FALSE, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        DT::formatStyle(1, fontWeight = 'bold')
    })

    file_stem <- function(default) {
      if (!is.null(input$study_name) && input$study_name != "") {
        paste0(gsub("[^A-Za-z0-9_-]", "_", input$study_name), "_Sample_Size")
      } else {
        default
      }
    }

    summary_sheet <- function(results) {
      data.frame(
        Item = c("Study name", "Sampling method", "Required sample size", "Explanation", "Date", "App version"),
        Value = c(if (is.null(input$study_name) || input$study_name == "") "Not specified" else input$study_name,
                  results$method_name, results$sample_size, results$explanation, as.character(Sys.Date()),
                  if (exists("app_version")) app_version() else ""),
        stringsAsFactors = FALSE
      )
    }

    # Download handlers
    output$download_excel <- downloadHandler(
      filename = function() paste0(file_stem("sample_size_calculation"), "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        results <- calculate_sample_size()
        sheets <- list(Summary = summary_sheet(results), Calculation = results$steps)
        if (!is.null(results$breakdown)) sheets[["Stratified Breakdown"]] <- display_breakdown(results)
        writexl::write_xlsx(sheets, file)
      }
    )

    output$download_stratified_breakdown <- downloadHandler(
      filename = function() paste0(file_stem("stratified_breakdown"), "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        results <- calculate_sample_size()
        writexl::write_xlsx(list(Summary = summary_sheet(results),
                                 Stratified_Breakdown = display_breakdown(results)), file)
      }
    )

    output$download_pdf <- downloadHandler(
      filename = function() paste0(file_stem("sample_size_report"), "_", Sys.Date(), ".pdf"),
      content = function(file) {
        results <- calculate_sample_size()
        study <- if (is.null(input$study_name) || input$study_name == "") "" else input$study_name

        breakdown_text <- ""
        if (!is.null(results$breakdown)) {
          breakdown_text <- paste0("\n\n## Stratified Breakdown\n\n", markdown_table(display_breakdown(results)))
        }

        rmd_content <- paste0(
          "---\n",
          "title: '", gsub("'", "''", ascii_text(if (study != "") paste(study, "Sample Size Calculation Report") else "Sample Size Calculation Report")), "'\n",
          "subtitle: 'ATR Consulting'\n",
          "date: '", Sys.Date(), "'\n",
          "output: pdf_document\n",
          "---\n\n",
          "## Study Information\n\n",
          "**Study Name:** ", if (study != "") ascii_text(study) else "Not specified", "\n\n",
          "**Sampling Method:** ", results$method_name, "\n\n",
          "## Required Sample Size\n\n",
          "**", results$sample_size, "**\n\n",
          "## Explanation\n\n",
          ascii_text(results$explanation), "\n\n",
          "## Detailed Calculations\n\n",
          markdown_table(results$steps),
          breakdown_text, "\n\n",
          "### Notes\n\n",
          "- **Z-score (alpha/2):** critical value for the confidence level\n",
          "- **Initial Sample Size (n0):** sample size for an infinite population, before any adjustment\n",
          "- **Finite Population Correction:** reduces the sample when it is a large share of the population\n",
          if (!is.null(results$cluster_size)) "- **Design Effect (DEFF):** inflation for interviewing m units per cluster\n" else "",
          "- **Expected Margin of Error:** precision achieved if the non-response rate is as assumed\n",
          "\n*Generated by the ATR Sampling Tool", if (exists("app_version")) paste0(" (version ", app_version(), ")") else "", ".*\n"
        )

        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(rmd_content, temp_rmd, useBytes = TRUE)

        # Render to PDF
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }
    )

    # Settings sent from the Decision Tree
    handover <- reactiveVal(NULL)
    output$handover_note <- renderUI(handover_note_ui(handover(), session$ns("dismiss_handover")))
    observeEvent(input$dismiss_handover, handover(NULL))

    list(apply_handover = function(settings, design_name) {
      method_labels <- c(simple_random = "Simple Random Sampling", stratified = "Stratified Random Sampling",
                         cluster = "Cluster Sampling", stratified_cluster = "Stratified Cluster Sampling",
                         systematic = "Systematic Sampling")
      clustered <- settings$method %in% c("cluster", "stratified_cluster")
      stratified <- settings$method %in% c("stratified", "stratified_cluster")
      updateSelectInput(session, "sampling_method", selected = settings$method)
      updateNumericInput(session, "non_response_rate", value = settings$non_response)
      if (clustered) {
        for (suffix in c("", "_stratified")) {
          updateNumericInput(session, paste0("cluster_size", suffix), value = settings$cluster_size)
          updateNumericInput(session, paste0("intraclass_correlation", suffix), value = settings$icc)
        }
      }
      if (stratified) updateRadioButtons(session, "precision_target", selected = settings$precision_target)
      # Earlier results no longer match the settings; they return on the next calculation
      # (full id: this runs from the Decision Tree, so shinyjs would use that module's namespace)
      shinyjs::hide(session$ns("results_container"), asis = TRUE)
      handover(list(design = design_name, action = "then click Calculate Sample Size", details = c(
        paste("method:", method_labels[[settings$method]]),
        if (clustered) sprintf("interviews per cluster %d and ICC %.2f (design effect %.2f, a common starting point)",
                               settings$cluster_size, settings$icc, 1 + (settings$cluster_size - 1) * settings$icc),
        paste0("non-response ", settings$non_response, "%"),
        if (stratified) paste("precision target:", if (settings$precision_target == "total") "whole population" else "each stratum")
      )))
    })
  })
}
