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
        p("Get started with the ATR Sampling Tool in a few steps:"),

        div(class = "step-card", div(class = "step-number", "1"), tags$strong("Choose Your Approach: "),
            "Start with the Decision Tree for a method recommendation, or go straight to a calculator or the Sample Generator."),
        div(class = "step-card", div(class = "step-number", "2"), tags$strong("Calculate Sample Size: "),
            "Use the Sample Calculator for monitoring surveys (a margin of error for a proportion or a mean), or the ",
            "Evaluation Calculator to compare groups or survey rounds (power and minimum detectable difference)."),
        div(class = "step-card", div(class = "step-number", "3"), tags$strong("Generate Your Sample: "),
            "Upload your sampling frame and draw the sample with the Sample Generator. Check the frame report, then confirm the draw."),
        div(class = "step-card", div(class = "step-number", "4"), tags$strong("Document and Field: "),
            "Download the Excel file. It holds the sample with weights, a Methodology sheet and a Replacement Log for the field team."),

        div(class = "warning-box",
            tags$strong("Data protection: "),
            "the app runs on a public cloud host. Upload anonymised frames only: no names, phone numbers or GPS points. ",
            "Keep the full list offline and link it back through the unit ID.")
      ),

      # Decision Tree Section
      div(
        class = "guide-section",
        h2("Decision Tree"),
        p("The Decision Tree suggests sampling methods from six questions about your study."),

        h3("How to Use:"),
        div(class = "step-card", tags$strong("1. Answer the questions: "),
            "survey mode, whether results must be generalised, subgroup comparisons, what list (frame) you have, ",
            "how many communities are involved and your main constraint."),
        div(class = "step-card", tags$strong("2. Review the recommendation: "),
            "each method comes with requirements, pros and cons. The yellow 'Points to consider' box lists caveats for ",
            "your answers, for example a coverage warning for a partial frame, or the limits of random digit dialing in Afghanistan."),
        div(class = "step-card", tags$strong("3. Export: "),
            "download a PDF report of your answers and the recommended methods."),

        div(class = "info-box",
            tags$ul(
              class = "mb-0",
              tags$li("If results need not be generalised, only non-probability methods (purposive, quota) are suggested; snowball only for hidden groups with no list."),
              tags$li("If you can visit every community, the tree suggests including all sites and sampling within each one.")
            )),
        div(class = "feature-highlight",
            "Tip: the Decision Tree gives indicative recommendations. Consult a sampling specialist for complex studies.")
      ),

      # Sample Calculator Section
      div(
        class = "guide-section",
        h2("Sample Calculator (monitoring)"),
        p("Sample size for estimating an indicator with a given margin of error, for one survey round."),

        h3("Common Parameters"),
        div(class = "step-card",
            tags$ul(
              tags$li(tags$strong("Key indicator: "), "a proportion (e.g. % of households that received aid) or a mean (e.g. income, yield)."),
              tags$li(tags$strong("Proportion: "), "expected proportion p (0.5 is the safe default) and the margin of error (0.05 = ±5 points)."),
              tags$li(tags$strong("Mean: "), "the standard deviation σ (from an earlier survey, or (max - min) / 4 as a rough guess) and the margin of error in the indicator's units."),
              tags$li("Population size (N), confidence level (default 95%) and non-response rate (default 5%).")
            )),

        h3("Cluster Sampling"),
        div(class = "step-card",
            tags$ul(
              tags$li(tags$strong("Interviews per cluster (m): "), "the number of units you will interview in each selected cluster, not the cluster's total size."),
              tags$li(tags$strong("ICC: "), "how alike units in the same cluster are (default 0.06). The design effect is 1 + (m - 1) × ICC."),
              tags$li("The result is rounded up to whole clusters of m interviews.")
            )),

        h3("Stratified Sampling"),
        div(class = "step-card",
            tags$ul(
              tags$li("Enter strata by hand (name, population and, optionally, an expected p or σ per stratum), or upload a file and pick the strata and population columns."),
              tags$li(tags$strong("Precision target - Each stratum: "), "every stratum reaches the margin of error on its own. Use it when you will report each stratum separately. Totals add up and are larger."),
              tags$li(tags$strong("Precision target - Whole population only: "), "one sample sized for the overall estimate, split across strata by proportional, Neyman (optimal) or equal allocation, with an optional minimum per stratum. Cheaper, but single strata are less precise."),
              tags$li("The breakdown table shows each stratum's sample, clusters and expected margin of error.")
            )),

        div(class = "info-box",
            "The calculator applies the design effect, then the finite population correction, then the non-response adjustment. ",
            "The Detailed Calculations table shows every step; the Excel and PDF downloads contain the same table.")
      ),

      # Evaluation Calculator Section
      div(
        class = "guide-section",
        h2("Evaluation Calculator"),
        p("Sample size, or the smallest detectable difference, when comparing a proportion between groups or survey rounds."),

        h3("Designs"),
        div(class = "step-card",
            tags$ul(
              tags$li(tags$strong("Two independent groups: "), "treatment vs control at one time, or baseline vs endline with a new sample each round."),
              tags$li(tags$strong("Panel: "), "the same households at baseline and endline. Set the baseline-endline correlation (ρ; use 0 if unknown) and the expected attrition."),
              tags$li(tags$strong("Difference-in-differences: "), "treatment and control areas at baseline and endline, as repeated cross-sections or as a panel. Enter both baselines and the change expected without the programme.")
            )),

        h3("What to Enter"),
        div(class = "step-card",
            tags$ul(
              tags$li("Power (default 80%), significance level (default 5%) and a one- or two-sided test."),
              tags$li("The baseline proportion and the detectable difference D (default 0.10 = 10 percentage points), as an increase or a decrease; or type the second proportion directly."),
              tags$li("Response rate, and for clustered samples the interviews per cluster (m) and ICC."),
              tags$li("Allocation ratio, if one group should be larger (e.g. 2 control households per treatment household).")
            )),

        h3("Two Modes"),
        div(class = "step-card",
            tags$ul(
              tags$li(tags$strong("Sample size: "), "the sample per group, clusters per group and total interviews needed."),
              tags$li(tags$strong("Minimum detectable difference (MDE): "), "enter the clusters (or interviews) per group your budget allows, and see the smallest difference you can detect.")
            )),

        div(class = "info-box",
            "The power curve shows how power changes with the sample (or with the true difference in MDE mode). ",
            "Download the Excel file for the full calculation, the group table and the curve.")
      ),

      # Sample Generator Section
      div(
        class = "guide-section",
        h2("Sample Generator"),
        p("Draws the sample from your uploaded frame (CSV or Excel) and documents how it was drawn."),

        h3("Basic Settings"),
        div(class = "step-card",
            tags$ul(
              tags$li(tags$strong("Sample size: "), "the main sample (the total across strata for stratified designs)."),
              tags$li(tags$strong("Buffer (%): "), "reserve units for replacements, drawn separately and numbered in the order they should be used (reserve_order)."),
              tags$li(tags$strong("Random seed: "), "makes the draw reproducible. If left blank, a random seed is chosen and recorded."),
              tags$li(tags$strong("Unit ID variable: "), "used to check for duplicate units and to fill the replacement log.")
            )),

        h3("Methods"),
        div(class = "step-card",
            tags$ul(
              tags$li(tags$strong("Simple random: "), "every unit has the same chance."),
              tags$li(tags$strong("Systematic: "), "a random start and a fixed (fractional) interval; sort the list by a variable such as district to spread the sample."),
              tags$li(tags$strong("Cluster: "), "select clusters (e.g. villages), with probability proportional to size (PPS) or equal probability, then m units within each. ",
                      "Use 'population units' when the file lists every unit, or 'cluster list' when it lists clusters with a population column."),
              tags$li(tags$strong("Stratified: "), "choose the strata variable and the allocation (proportional, equal, Neyman or manual) with an optional minimum per stratum. ",
                      "The allocation preview shows the split; for Neyman or manual, double-click the Input column to edit it. Units are selected by simple random or systematic sampling within each stratum."),
              tags$li(tags$strong("Stratified cluster: "), "the cluster method runs separately in each stratum, each with its own seed derived from the master seed.")
            )),

        h3("Frame Checks"),
        div(class = "step-card",
            "Clicking Generate first shows a frame report: duplicate or missing IDs, rows with no stratum or cluster ",
            "(excluded and counted), clusters with no size, clusters smaller than m, and village names repeated across strata. ",
            "Errors block the draw; otherwise review the report and click 'Confirm and Draw Sample'."),

        h3("Outputs"),
        div(class = "feature-highlight",
            tags$ul(
              tags$li("Design columns at the start of the sample: sample_type, reserve_order, stratum, cluster_id, selection probabilities (p_stage1, p_stage2, p_incl) and base_weight. Weights add up to the frame size."),
              tags$li("Excel sheets: Sample, Methodology (settings, seed, exclusions, warnings, version), Allocation, Cluster Summary, Frame Checks, Column Guide and Replacement Log."),
              tags$li("CSV download of the sample, and a cluster summary for cluster designs.")
            )),

        div(class = "warning-box",
            "Important: if many clusters have fewer units than m, the sample falls short. The frame check warns about this; ",
            "reduce m or merge small neighbouring clusters before drawing.")
      ),

      # Sampling Guideline Section
      div(
        class = "guide-section",
        h2("Sampling Guideline"),
        p("A knowledge base on sampling methods and practice. The search box looks through the full text of every article."),

        h3("Available Topics:"),
        div(class = "step-card", tags$strong("Fundamentals: "), "introduction to sampling; probability vs non-probability sampling."),
        div(class = "step-card", tags$strong("Methods: "), "simple random, stratified, cluster and systematic sampling."),
        div(class = "step-card", tags$strong("Best practice: "), "quality control in sampling."),
        div(class = "step-card", tags$strong("Field guidance: "), "site and sub-project verification, phone and remote monitoring, and the replacement protocol."),
        div(class = "step-card", tags$strong("Resources: "), "recommended books, online resources and field data collection tools."),

        div(class = "info-box",
            "Use the sidebar to move between topics. Each article includes examples and practical guidance.")
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
             tags$li("Keep the file under 30 MB: drop columns you do not need for sampling, or save as CSV"),
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

