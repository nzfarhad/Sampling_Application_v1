# User Guide Module
# How to use the ATR Sampling Tool. Uses the same layout as the Sampling
# Guideline (wiki_module.R); the articles are markdown files in guide_content/.

GUIDE_SECTIONS <- c(
  start = "Getting Started",
  tools = "Using the Tools",
  help = "Help"
)

GUIDE_ARTICLES <- list(
  getting_started = list(title = "Getting Started", content = "getting_started.md", category = "start",
                         description = "What each tab does, a typical workflow and data protection"),
  decision_tree = list(title = "Decision Tree", content = "decision_tree.md", category = "tools",
                       description = "Get a recommended sampling method for your study"),
  sample_calculator = list(title = "Sample Calculator", content = "sample_calculator.md", category = "tools",
                           description = "Sample size for a target margin of error"),
  evaluation_calculator = list(title = "Evaluation Calculator", content = "evaluation_calculator.md", category = "tools",
                               description = "Sample size or minimum detectable difference for comparisons"),
  sample_generator = list(title = "Sample Generator", content = "sample_generator.md", category = "tools",
                          description = "Draw a documented sample from your frame"),
  sampling_guideline = list(title = "Sampling Guideline", content = "sampling_guideline.md", category = "tools",
                            description = "The knowledge base on sampling methods and practice"),
  best_practices = list(title = "Best Practices", content = "best_practices.md", category = "help",
                        description = "Planning, using the tools and quality assurance"),
  troubleshooting = list(title = "Troubleshooting", content = "troubleshooting.md", category = "help",
                         description = "Common problems and how to solve them")
)

user_guide_ui <- function(id) {
  wiki_ui(id, title = "How to Use the ATR Sampling Tool",
          subtitle = "A guide to each tab, from choosing a method to drawing and documenting the sample",
          search_placeholder = "Search the guide...")
}

user_guide_server <- function(id) {
  wiki_server(id, articles = GUIDE_ARTICLES, sections = GUIDE_SECTIONS,
              content_dir = "guide_content", home_label = "User Guide")
}
