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
                ),
                
                # Sampling Method Selection
                div(
                  class = "col-md-6",
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
                  div(
                    class = "row",
                    div(
                      class = "col-md-6",
                      numericInput(
                        ns("cluster_size"),
                        "Cluster Size",
                        value = 10,
                        min = 2,
                        step = 1,
                        width = "100%"
                      )
                    ),
                    div(
                      class = "col-md-6",
                      p("Number of observations per cluster", class = "text-muted small")
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
                h5("Sample Results", class = "mb-0")
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
                      class = "card bg-warning text-white",
                      div(
                        class = "card-body text-center",
                        h4(id = ns("method_result"), class = "mb-0"),
                        p("Method", class = "mb-0")
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
                )
              )
            )
          )
        )
      )
    )
  )
}

# Sampling Generator Server
sampling_generator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    uploaded_data <- reactiveVal(NULL)
    generated_sample <- reactiveVal(NULL)
    
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
    
     # Generate sample
     observeEvent(input$generate_sample, {
       req(uploaded_data(), input$sample_size)
       
       data <- uploaded_data()
       sample_size <- input$sample_size
       method <- input$sampling_method
       seed_value <- input$random_seed
       
       # Validate sample size
       if (sample_size > nrow(data)) {
         showNotification("Sample size cannot be larger than population size!", type = "error")
         return()
       }
       
       tryCatch({
         # Set seed for reproducibility
         set.seed(seed_value)
         
         if (method == "simple_random") {
           # Simple Random Sampling
           sample_indices <- sample(1:nrow(data), size = sample_size, replace = FALSE)
           sample_data <- data[sample_indices, ]
           
         } else if (method == "systematic") {
           # Systematic Sampling
           N <- nrow(data)
           k <- floor(N / sample_size)  # Sampling interval
           
           if (k < 1) {
             showNotification("Sample size too large for systematic sampling!", type = "error")
             return()
           }
           
           # Random start point
           start <- sample(1:k, 1)
           sample_indices <- seq(start, N, by = k)[1:sample_size]
           sample_data <- data[sample_indices, ]
           
         } else if (method %in% c("stratified", "cluster", "stratified_cluster")) {
           # Placeholder for future implementation
           showNotification("This sampling method will be implemented in a future update.", type = "warning")
           return()
         }
        
        # Store the generated sample
        generated_sample(sample_data)
        
        # Show results section
        shinyjs::show("results_section")
        
        # Update summary cards
        shinyjs::runjs(paste0('
          document.getElementById("', session$ns("sample_size_result"), '").textContent = "', nrow(sample_data), '";
          document.getElementById("', session$ns("population_size_result"), '").textContent = "', nrow(data), '";
          document.getElementById("', session$ns("sampling_fraction_result"), '").textContent = "', round(nrow(sample_data) / nrow(data) * 100, 2), '%";
          document.getElementById("', session$ns("method_result"), '").textContent = "', tools::toTitleCase(gsub("_", " ", method)), '";
          document.getElementById("', session$ns("seed_result"), '").textContent = "', seed_value, '";
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
        writexl::write_xlsx(generated_sample(), file)
      }
    )
  })
}
