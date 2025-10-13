# User Guide Module
# Comprehensive guide on how to use the ATR Sampling Tool

user_guide_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .guide-container {
          max-width: 1200px;
          margin: 0 auto;
          padding: 20px;
        }
        
        .guide-header {
          background: linear-gradient(135deg, #730202 0%, #f27304 100%);
          color: white;
          padding: 40px 20px;
          border-radius: 10px;
          margin-bottom: 30px;
          text-align: center;
          box-shadow: 0 4px 15px rgba(115, 2, 2, 0.1);
        }
        
        .guide-header h1 {
          font-size: 2.5rem;
          font-weight: 300;
          margin-bottom: 10px;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
        }
        
        .guide-header p {
          font-size: 1.1rem;
          opacity: 0.9;
          margin: 0;
        }
        
        .guide-section {
          background: white;
          border-radius: 10px;
          padding: 30px;
          margin-bottom: 30px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          border-left: 5px solid #730202;
        }
        
        .guide-section h2 {
          color: #730202;
          font-size: 1.8rem;
          margin-bottom: 20px;
          border-bottom: 2px solid #f27304;
          padding-bottom: 10px;
        }
        
        .guide-section h3 {
          color: #730202;
          font-size: 1.4rem;
          margin-top: 25px;
          margin-bottom: 15px;
        }
        
        .step-card {
          background: #f8f9fa;
          border-radius: 8px;
          padding: 20px;
          margin: 15px 0;
          border-left: 4px solid #f27304;
        }
        
        .step-number {
          background: #730202;
          color: white;
          border-radius: 50%;
          width: 30px;
          height: 30px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          font-weight: bold;
          margin-right: 15px;
        }
        
        .feature-highlight {
          background: linear-gradient(135deg, #f27304 0%, #ff9500 100%);
          color: white;
          padding: 15px;
          border-radius: 8px;
          margin: 15px 0;
          font-weight: 500;
        }
        
        .warning-box {
          background: #fff3cd;
          border: 1px solid #ffeaa7;
          border-radius: 8px;
          padding: 15px;
          margin: 15px 0;
          border-left: 4px solid #f39c12;
        }
        
        .info-box {
          background: #d1ecf1;
          border: 1px solid #bee5eb;
          border-radius: 8px;
          padding: 15px;
          margin: 15px 0;
          border-left: 4px solid #17a2b8;
        }
        
        .code-example {
          background: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          padding: 15px;
          margin: 15px 0;
          font-family: 'Courier New', monospace;
          font-size: 0.9rem;
        }
        
        .nav-link {
          color: #730202;
          text-decoration: none;
          font-weight: 500;
        }
        
        .nav-link:hover {
          color: #f27304;
          text-decoration: underline;
        }
      "))
    ),
    
    div(
      class = "guide-container",
      
      # Header
      div(
        class = "guide-header",
        h1("How to Use the ATR Sampling Tool"),
        p("A comprehensive guide to help you navigate and utilize all features of the sampling application")
      ),
      
      # Quick Start Section
      div(
        class = "guide-section",
        h2("Quick Start Guide"),
        p("Get started with the ATR Sampling Tool in just a few steps:"),
        
        div(
          class = "step-card",
          div(
            class = "step-number", "1"
          ),
          tags$strong("Choose Your Approach:"),
          "Start with the Decision Tree to get sampling method recommendations, or jump directly to the Sample Calculator or Sample Generator."
        ),
        
        div(
          class = "step-card",
          div(
            class = "step-number", "2"
          ),
          tags$strong("Calculate Sample Size:"),
          "Use the Sample Calculator to determine the appropriate sample size for your study parameters."
        ),
        
        div(
          class = "step-card",
          div(
            class = "step-number", "3"
          ),
          tags$strong("Generate Your Sample:"),
          "Upload your dataset and use the Sample Generator to create your actual sample."
        ),
        
        div(
          class = "step-card",
          div(
            class = "step-number", "4"
          ),
          tags$strong("Learn More:"),
          "Refer to the Sampling Guideline for detailed information about different sampling methods."
        )
      ),
      
      # Decision Tree Section
      div(
        class = "guide-section",
        h2("Decision Tree"),
        p("The Decision Tree helps you choose the most appropriate sampling method based on your study characteristics."),
        
        h3("How to Use:"),
        div(
          class = "step-card",
          tags$strong("1. Enter Study Information:"),
          "Fill in the study name (optional) and answer questions about your survey mode, population characteristics, and study objectives."
        ),
        
        div(
          class = "step-card",
          tags$strong("2. Review Recommendations:"),
          "The tool will provide recommended sampling methods with explanations and considerations."
        ),
        
        div(
          class = "step-card",
          tags$strong("3. Export Results:"),
          "Download the decision tree results as a PDF report for your study documentation."
        ),
        
        div(
          class = "feature-highlight",
          "💡 Tip: The Decision Tree provides indicative recommendations. Always consult with sampling experts for complex studies."
        )
      ),
      
      # Sample Calculator Section
      div(
        class = "guide-section",
        h2("Sample Calculator"),
        p("Calculate the required sample size for different sampling methods with various statistical parameters."),
        
        h3("Available Calculators:"),
        
        h3("Simple Random Sampling"),
        div(
          class = "step-card",
          tags$strong("Parameters to Set:"),
          tags$ul(
            tags$li("Population size (N)"),
            tags$li("Expected proportion (p) - default 0.5"),
            tags$li("Confidence level - default 95%"),
            tags$li("Margin of error - default 5%"),
            tags$li("Non-response rate - default 5%")
          )
        ),
        
        h3("Stratified Sampling"),
        div(
          class = "step-card",
          tags$strong("Additional Steps:"),
          tags$ul(
            tags$li("Define your strata"),
            tags$li("Enter population size for each stratum"),
            tags$li("Choose allocation method (proportional or equal)"),
            tags$li("Set cluster parameters if using cluster sampling within strata")
          )
        ),
        
        h3("Cluster Sampling"),
        div(
          class = "step-card",
          tags$strong("Key Parameters:"),
          tags$ul(
            tags$li("Cluster size (average number of units per cluster)"),
            tags$li("Intra-cluster correlation coefficient (ICC) - default 0.06"),
            tags$li("Design effect calculation"),
            tags$li("Non-response adjustment")
          )
        ),
        
        div(
          class = "info-box",
          "The calculator automatically adjusts for finite population correction and design effects where applicable."
        )
      ),
      
      # Sample Generator Section
      div(
        class = "guide-section",
        h2("Sample Generator"),
        p("Generate actual samples from your uploaded datasets using various sampling methods."),
        
        h3("Supported File Formats:"),
          p("CSV files (.csv)\n• Excel files (.xlsx, .xls)"),
        
        
        h3("Sampling Methods Available:"),
        
        h3("Simple Random Sampling"),
        div(
          class = "step-card",
          tags$strong("Process:"),
          tags$ul(
            tags$li("Upload your dataset"),
            tags$li("Set sample size and buffer percentage"),
            tags$li("Choose random seed for reproducibility"),
            tags$li("Generate and download your sample")
          )
        ),
        
        h3("Systematic Sampling"),
        div(
          class = "step-card",
          tags$strong("Process:"),
          tags$ul(
            tags$li("Upload your dataset"),
            tags$li("Set sample size and buffer percentage"),
            tags$li("The tool calculates sampling interval automatically"),
            tags$li("Generates systematic sample with random start")
          )
        ),
        
        h3("Cluster Sampling"),
        div(
          class = "step-card",
          tags$strong("Method 1 - Population Units:"),
          tags$ul(
            tags$li("Select cluster variable (e.g., village, district)"),
            tags$li("Choose cluster size (units per cluster)"),
            tags$li("Select PPS (Probability Proportional to Size) or equal probability"),
            tags$li("Generate sample with automatic MOS calculation")
          )
        ),
        
        div(
          class = "step-card",
          tags$strong("Method 2 - Cluster List:"),
          tags$ul(
            tags$li("Select cluster list variable"),
            tags$li("Select population variable for each cluster"),
            tags$li("Choose cluster size and sampling method"),
            tags$li("Generate sample based on cluster populations")
          )
        ),
        
        div(
          class = "warning-box",
          "⚠️ Important: Ensure your dataset has the required variables for cluster sampling. The tool will validate your selections."
        ),
        
        h3("Sample Output Features:"),
        div(
          class = "feature-highlight",
          tags$ul(
            tags$li("📊 Summary statistics (sample size, population size, sampling fraction)"),
            tags$li("📋 Sample preview with data table"),
            tags$li("💾 Download options (CSV and Excel formats)"),
            tags$li("🏷️ Sample type labeling (Sample vs Buffer)")
          )
        )
      ),
      
      # Sampling Guideline Section
      div(
        class = "guide-section",
        h2("Sampling Guideline"),
        p("Comprehensive knowledge base covering sampling theory, methodologies, and best practices."),
        
        h3("Available Topics:"),
        div(
          class = "step-card",
          tags$strong("Sampling Introduction:"),
          "Basic concepts, terminology, and principles of sampling"
        ),
        
        div(
          class = "step-card",
          tags$strong("Probability Sampling Methods:"),
          "Overview of different probability sampling techniques"
        ),
        
        div(
          class = "step-card",
          tags$strong("Simple Random Sampling:"),
          "Detailed guide to simple random sampling methodology"
        ),
        
        div(
          class = "step-card",
          tags$strong("Stratified Sampling:"),
          "Comprehensive guide to stratified sampling design and implementation"
        ),
        
        div(
          class = "step-card",
          tags$strong("Cluster Sampling:"),
          "In-depth coverage of cluster sampling methods and considerations"
        ),
        
        div(
          class = "step-card",
          tags$strong("Sampling in Fragile Contexts:"),
          "Special considerations for sampling in challenging environments"
        ),
        
        div(
          class = "info-box",
          "📖 Use the sidebar navigation to quickly jump between topics. Each section includes examples and practical guidance."
        )
      ),
      
      # Best Practices Section
      div(
        class = "guide-section",
        h2("Best Practices"),
        
        h3("Before Starting Your Study:"),
        div(
          class = "step-card",
          tags$ul(
            tags$li("Clearly define your research objectives and target population"),
            tags$li("Understand your data collection constraints and resources"),
            tags$li("Consider the level of precision required for your estimates"),
            tags$li("Plan for non-response and data quality issues")
          )
        ),
        
        h3("When Using the Tools:"),
        div(
          class = "step-card",
          tags$ul(
            tags$li("Always validate your input parameters"),
            tags$li("Use appropriate confidence levels and margins of error"),
            tags$li("Include buffer samples to account for non-response"),
            tags$li("Document your sampling decisions and rationale")
          )
        ),
        
        h3("Quality Assurance:"),
        div(
          class = "step-card",
          tags$ul(
            tags$li("Verify your sample represents the target population"),
            tags$li("Check for systematic biases in your sampling frame"),
            tags$li("Monitor response rates and adjust if necessary"),
            tags$li("Document any deviations from planned sampling procedures")
          )
        ),
        
        div(
          class = "warning-box",
          "⚠️ Remember: This tool provides statistical guidance. Always consult with sampling experts for complex studies or when working in challenging contexts."
        )
      ),
      
      # Troubleshooting Section
      div(
        class = "guide-section",
        h2("Troubleshooting"),
        
        h3("Common Issues and Solutions:"),
        
        h3("File Upload Problems:"),
        div(
          class = "step-card",
          tags$strong("Issue:"),
          "File not uploading or error messages",
          tags$br(),
          tags$strong("Solutions:"),
          tags$ul(
            tags$li("Check file format (CSV or Excel only)"),
             tags$li("Ensure file size is reasonable (< 100MB)"),
            tags$li("Verify file is not corrupted"),
            tags$li("Check column names don't contain special characters")
          )
        ),
        
        h3("Sample Size Calculations:"),
        div(
          class = "step-card",
          tags$strong("Issue:"),
          "Unrealistic sample sizes",
          tags$br(),
          tags$strong("Solutions:"),
          tags$ul(
            tags$li("Check population size input"),
            tags$li("Adjust margin of error if too small"),
            tags$li("Consider stratified sampling for large populations"),
            tags$li("Verify confidence level is appropriate")
          )
        ),
        
        h3("Cluster Sampling Issues:"),
        div(
          class = "step-card",
          tags$strong("Issue:"),
          "Cluster sampling not working",
          tags$br(),
          tags$strong("Solutions:"),
          tags$ul(
            tags$li("Ensure cluster variable is properly selected"),
            tags$li("Check that cluster variable has multiple unique values"),
            tags$li("Verify cluster size is appropriate for your data"),
            tags$li("For Method 2, ensure population variable is numeric")
          )
        ),
        
        h3("Getting Help:"),
        div(
          class = "info-box",
          "If you encounter issues not covered here, please contact the ATR Data management team or refer to the Sampling Guideline for detailed methodology information."
        )
      ),
      
      # Footer
      div(
        class = "text-center mt-5 mb-4",
        style = "color: #6c757d;",
        p("ATR Sampling Tool - Comprehensive Sampling Solutions"),
        p("For technical support or methodological questions, please contact the ATR Data Management team.")
      )
    )
  )
}

# User Guide Server
user_guide_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # This module is primarily informational, so no complex server logic needed
    # Could add interactive elements here if needed in the future
  })
}

