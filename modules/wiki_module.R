# Wiki Module
# Modern bookdown-style knowledge base with sampling fundamentals and methodologies

wiki_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Include external CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  tags$style(HTML('
        .wiki-container {
          max-width: 1200px;
          margin: 0 auto;
          padding: 20px;
        }
        
        .wiki-header {
          background: linear-gradient(135deg, #730202 0%, #f27304 100%);
          color: white;
          padding: 40px 20px;
          border-radius: 10px;
          margin-bottom: 30px;
          text-align: center;
          box-shadow: 0 4px 15px rgba(115, 2, 2, 0.1);
        }
        
        .wiki-header h1 {
          font-size: 2.5rem;
          font-weight: 300;
          margin-bottom: 10px;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
        }
        
        .wiki-header p {
          font-size: 1.1rem;
          opacity: 0.9;
          margin: 0;
        }
        
        .wiki-sidebar {
          background: #f8f9fa;
          border-radius: 10px;
          padding: 20px;
          height: fit-content;
          position: sticky;
          top: 20px;
        }
        
        .wiki-content {
          background: white;
          border-radius: 10px;
          padding: 28px 32px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.06);
          min-height: 560px;
          color: #343a40;
          font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial;
          line-height: 1.65;
        }
        
        .toc-item {
          padding: 8px 12px;
          margin: 2px 0;
          border-radius: 6px;
          cursor: pointer;
          transition: all 0.2s ease;
        }
        
        .toc-item:hover {
          background: #fff1e6;
          transform: translateX(5px);
          color: #730202;
        }
        
        .toc-item.active {
          background: #730202;
          color: white;
        }
        
        .toc-section {
          font-weight: bold;
          color: #495057;
          margin-top: 15px;
          margin-bottom: 8px;
          padding-left: 8px;
          border-left: 3px solid #f27304;
        }
        
        .search-container {
          margin-bottom: 20px;
        }
        
        .search-input {
          border-radius: 25px;
          border: 2px solid #e9ecef;
          padding: 12px 20px;
          width: 100%;
          font-size: 16px;
          transition: border-color 0.3s ease;
        }
        
        .search-input:focus {
          border-color: #f27304;
          outline: none;
          box-shadow: 0 0 0 2px rgba(242, 115, 4, 0.1);
        }
        
        .search-input:focus {
          border-color: #2E86AB;
          outline: none;
          box-shadow: 0 0 0 3px rgba(46, 134, 171, 0.1);
        }
        
        .content-section {
          margin-bottom: 24px;
          max-width: 900px;
        }

        /* Headings: scale down slightly for a cleaner page */
        .content-section h1,
        .wiki-content h1 {
          font-size: 1.75rem;
          font-weight: 600;
          margin-bottom: 0.5rem;
          color: #1f62a0;
        }

        .content-section h2,
        .wiki-content h2 {
          font-size: 1.25rem;
          font-weight: 600;
          color: #2E86AB;
          border-bottom: 2px solid #f1f3f5;
          padding-bottom: 0.4rem;
          margin-top: 1.25rem;
          margin-bottom: 0.75rem;
        }

        .content-section h3,
        .wiki-content h3 {
          font-size: 1.05rem;
          font-weight: 600;
          color: #495057;
          margin-top: 1rem;
          margin-bottom: 0.5rem;
        }

        .wiki-content p {
          font-size: 0.98rem;
          margin-bottom: 0.9rem;
          color: #495057;
        }

        .wiki-content a { color: #1f62a0; text-decoration: underline; }

        /* Lists */
        .wiki-content ul, .wiki-content ol {
          margin-left: 1.15rem;
          margin-bottom: 0.9rem;
        }

        /* Code and pre blocks */
        pre, code {
          font-family: Consolas, "Courier New", monospace;
          background: #f8f9fa;
          border-radius: 6px;
        }

        code { padding: 0.15rem 0.35rem; font-size: 0.92rem; }

        pre {
          padding: 12px 14px;
          overflow: auto;
          font-size: 0.9rem;
          margin-bottom: 1rem;
        }

        blockquote {
          border-left: 4px solid #e9ecef;
          padding-left: 12px;
          color: #6c757d;
          margin: 1rem 0;
          background: #fbfbfb;
          border-radius: 4px;
        }

        /* Tables */
        table {
          width: 100%;
          border-collapse: collapse;
          margin-bottom: 1rem;
          font-size: 0.95rem;
        }

        table th, table td {
          padding: 8px 10px;
          border: 1px solid #e9ecef;
        }

        table thead th {
          background: #f8f9fa;
          color: #343a40;
        }

        /* Images */
        .wiki-content img { max-width: 100%; height: auto; display: block; margin: 0.6rem 0; border-radius: 6px; }

        /* Small meta tags */
        .meta { font-size: 0.85rem; color: #6c757d; margin-bottom: 0.5rem; }

  .content-header h1 { margin: 0 0 6px 0; }
  .content-header { padding-bottom: 6px; border-bottom: 1px solid #f1f3f5; margin-bottom: 14px; }

  .highlight-box { background: #f8f9fa; border-left: 4px solid #e9ecef; padding: 10px 12px; border-radius: 6px; margin-bottom: 12px; }
        
        .highlight-box {
          background: #f8f9fa;
          border-left: 4px solid #2E86AB;
          padding: 20px;
          margin: 20px 0;
          border-radius: 0 8px 8px 0;
        }
        
        .method-card {
          background: white;
          border: 1px solid #e9ecef;
          border-radius: 8px;
          padding: 20px;
          margin: 15px 0;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        
        .method-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        .method-card h4 {
          color: #2E86AB;
          margin-bottom: 10px;
        }
        
        .tag {
          display: inline-block;
          background: #e9ecef;
          color: #495057;
          padding: 4px 8px;
          border-radius: 12px;
          font-size: 0.8rem;
          margin: 2px;
        }
        
        .tag.primary {
          background: #2E86AB;
          color: white;
        }
        
        .tag.success {
          background: #28a745;
          color: white;
        }
        
        .tag.warning {
          background: #ffc107;
          color: #212529;
        }
        
        .breadcrumb {
          background: none;
          padding: 0;
          margin-bottom: 20px;
        }
        
        .breadcrumb-item a {
          color: #2E86AB;
          text-decoration: none;
        }
        
        .breadcrumb-item.active {
          color: #6c757d;
        }
        
        .no-results {
          text-align: center;
          padding: 40px;
          color: #6c757d;
        }
        
        .no-results i {
          font-size: 3rem;
          margin-bottom: 15px;
          opacity: 0.5;
        }
      '))
    ),
    
    div(
      class = "wiki-container",
      
      # Header
      div(
        class = "wiki-header",
        h1("Sampling Guideline"),
        p("A quick guide to sampling methodologies, best practices, and statistical foundations")
      ),
      
      # Main content area
      div(
        class = "row",
        
        # Sidebar
        div(
          class = "col-lg-3",
          div(
            class = "wiki-sidebar",
            
            # Search
            div(
              class = "search-container",
              # Use a proper Shiny input so it's available in server as input$search_input
              textInput(ns("search_input"), label = NULL, value = "", placeholder = "Search knowledge base...")
            ),

            # Table of Contents (rendered by server)
            uiOutput(ns("toc_container"))
          )
        ),
        
        # Content
        div(
          class = "col-lg-9",
          div(
            class = "wiki-content",
            
            # Breadcrumb (rendered by server)
            uiOutput(ns("breadcrumb_container")),

            # Content area (rendered by server)
            uiOutput(ns("content_area"))
          )
        )
      )
    )
  )
}

# Simple markdown to HTML conversion function
convert_markdown_to_html <- function(markdown_lines) {
  html_lines <- character(length(markdown_lines))
  
  for (i in seq_along(markdown_lines)) {
    line <- markdown_lines[i]
    
    # Convert headers
    if (grepl("^# ", line)) {
      html_lines[i] <- gsub("^# (.+)$", "<h1>\\1</h1>", line)
    } else if (grepl("^## ", line)) {
      html_lines[i] <- gsub("^## (.+)$", "<h2>\\1</h2>", line)
    } else if (grepl("^### ", line)) {
      html_lines[i] <- gsub("^### (.+)$", "<h3>\\1</h3>", line)
    } else if (grepl("^#### ", line)) {
      html_lines[i] <- gsub("^#### (.+)$", "<h4>\\1</h4>", line)
    } else if (grepl("^##### ", line)) {
      html_lines[i] <- gsub("^##### (.+)$", "<h5>\\1</h5>", line)
    } else if (grepl("^###### ", line)) {
      html_lines[i] <- gsub("^###### (.+)$", "<h6>\\1</h6>", line)
    } else if (line == "") {
      html_lines[i] <- "<br>"
    } else {
      # Convert bold and italic
      line <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", line)
      line <- gsub("\\*(.+?)\\*", "<em>\\1</em>", line)
      line <- gsub("`(.+?)`", "<code>\\1</code>", line)
      
      # Convert lists
      if (grepl("^- ", line)) {
        html_lines[i] <- gsub("^- (.+)$", "<li>\\1</li>", line)
      } else if (grepl("^\\d+\\. ", line)) {
        html_lines[i] <- gsub("^\\d+\\. (.+)$", "<li>\\1</li>", line)
      } else {
        html_lines[i] <- paste0("<p>", line, "</p>")
      }
    }
  }
  
  # Wrap in content section div
  result <- paste0(
    "<div class='content-section'>",
    paste(html_lines, collapse = "\n"),
    "</div>"
  )
  
  return(result)
}

wiki_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Wiki content structure
    wiki_content <- list(
      "fundamentals" = list(
        "introduction" = list(
          title = "Introduction to Sampling",
          content = "sampling_introduction.md",
          category = "fundamentals",
          description = "Learn the basics of sampling theory and methodology"
        ),
        "probability_vs_nonprobability" = list(
          title = "Probability vs Non-Probability Sampling",
          content = "probability_sampling.md",
          category = "fundamentals",
          description = "Understanding the differences between sampling approaches"
        )
        # "sampling_error" = list(
        #   title = "Sampling Error and Bias",
        #   content = "sampling_error.md",
        #   category = "fundamentals",
        #   description = "Types of errors and how to minimize them"
        # )
      ),
      "methods" = list(
        "simple_random" = list(
          title = "Simple Random Sampling",
          content = "simple_random_sampling.md",
          category = "methods",
          description = "The most basic form of probability sampling"
        ),
        "stratified" = list(
          title = "Stratified Sampling",
          content = "stratified_sampling.md",
          category = "methods",
          description = "Sampling with population subgroups"
        ),
        "cluster" = list(
          title = "Cluster Sampling",
          content = "cluster_sampling.md",
          category = "methods",
          description = "Sampling groups of units together"
        ),
        "systematic" = list(
          title = "Systematic Sampling",
          content = "systematic_sampling.md",
          category = "methods",
          description = "Selecting every kth unit from a population"
        )
      ),
      "best_practices" = list(
        # "sample_size" = list(
        #   title = "Determining Sample Size",
        #   content = "sample_size_determination.md",
        #   category = "best_practices",
        #   description = "How to calculate the right sample size"
        # ),
        # "sampling_frame" = list(
        #   title = "Creating a Sampling Frame",
        #   content = "sampling_frame.md",
        #   category = "best_practices",
        #   description = "Building comprehensive population lists"
        # ),
        "quality_control" = list(
          title = "Quality Control in Sampling",
          content = "quality_control.md",
          category = "best_practices",
          description = "Ensuring sampling quality and validity"
        )
      )
    )
    
    # Flatten content for easier searching
    flat_content <- list()
    for (category in names(wiki_content)) {
      for (article_id in names(wiki_content[[category]])) {
        flat_content[[article_id]] <- wiki_content[[category]][[article_id]]
      }
    }
    
    # Current article
    current_article <- reactiveVal("introduction")
    
    # Filter content based on search
    filtered_content <- reactive({
      content <- flat_content
      
      # Filter by search term
      if (!is.null(input$search_input) && input$search_input != "") {
        search_term <- tolower(input$search_input)
        content <- content[sapply(content, function(x) 
          grepl(search_term, tolower(x$title)) || 
          grepl(search_term, tolower(x$description)))]
      }
      
      return(content)
    })
    
    # Update table of contents
    observe({
      content <- filtered_content()
      
      # Create organized TOC
      toc_html <- list()
      
      # Add sections
      sections <- list(
        "fundamentals" = "Fundamentals",
        "methods" = "Sampling Methods", 
        "best_practices" = "Best Practices"
      )
      
      for (section_id in names(sections)) {
        section_articles <- content[sapply(content, function(x) x$category == section_id)]
        if (length(section_articles) > 0) {
          toc_html <- append(toc_html, list(
            tags$div(
              class = "toc-section",
              sections[[section_id]]
            )
          ))
          
          for (article_id in names(section_articles)) {
            article <- section_articles[[article_id]]
            is_active <- current_article() == article_id
            
            toc_html <- append(toc_html, list(
              tags$div(
                  class = paste("toc-item", if(is_active) "active" else ""),
                  `data-article` = article_id,
                  role = "button",
                  `aria-pressed` = if (is_active) "true" else "false",
                  article$title
              )
            ))
          }
        }
      }
      
      output$toc_container <- renderUI({
        if (length(content) == 0) {
          div(
            class = "no-results",
            icon("search"),
            h4("No Results Found"),
            p("Try adjusting your search terms or browse the categories below.")
          )
        } else {
          tagList(toc_html)
        }
      })
    })
    
    # Display article content
    display_article <- function(article_id) {
      article <- flat_content[[article_id]]
      if (is.null(article)) return()
      
      # Update breadcrumb
      breadcrumb_html <- tags$nav(
        class = "breadcrumb",
        tags$ol(
          class = "breadcrumb",
          tags$li(
            class = "breadcrumb-item",
            tags$a(href = "#", "Knowledge Base")
          ),
          tags$li(
            class = "breadcrumb-item",
            tags$a(href = "#", stringr::str_to_title(article$category))
          ),
          tags$li(
            class = "breadcrumb-item active",
            article$title
          )
        )
      )
      
      output$breadcrumb_container <- renderUI({
        breadcrumb_html
      })
      
      # Try to read markdown file
      content_file <- file.path(getwd(), "wiki_content", article$content)
      if (file.exists(content_file)) {
        # Read markdown file and collapse into single string
        md_lines <- readLines(content_file, warn = FALSE)
        md_text <- paste(md_lines, collapse = "\n")

        # Try to render markdown using preferred packages, falling back to simple converter
        content_html <- NULL
        tryCatch({
          if (requireNamespace("markdown", quietly = TRUE)) {
            # markdown::markdownToHTML requires a file or text input; use markdownToHTML(text = ...)
            content_html <- markdown::markdownToHTML(text = md_text, fragment.only = TRUE)
          } else if (requireNamespace("rmarkdown", quietly = TRUE)) {
            # Use rmarkdown to render to HTML fragment if possible
            tmp_in <- tempfile(fileext = ".md")
            tmp_out <- tempfile(fileext = ".html")
            writeLines(md_text, con = tmp_in)
            rmarkdown::render(tmp_in, output_file = tmp_out, quiet = TRUE, envir = new.env())
            content_html <- paste(readLines(tmp_out, warn = FALSE), collapse = "\n")
          } else {
            content_html <- convert_markdown_to_html(md_lines)
          }
        }, error = function(e) {
          # Final fallback
          content_html <<- convert_markdown_to_html(md_lines)
        })
      } else {
        # Fallback content
        content_html <- paste0(
          "<div class='content-section'>",
          "<h1>", article$title, "</h1>",
          "<div class='highlight-box'>",
          "<p><strong>Description:</strong> ", article$description, "</p>",
          "</div>",
          "<p>This article is coming soon. Content will be added in future updates.</p>",
          "</div>"
        )
      }
      
      # Debug: Print content info
      cat("Displaying article:", article$title, "\n")
      cat("Content file:", content_file, "\n")
      cat("File exists:", file.exists(content_file), "\n")

      # Build a nicer article header and metadata
      header_html <- paste0(
        "<div class='content-header' style='margin-bottom:12px;'>",
        "<h1>", article$title, "</h1>",
        if (!is.null(article$description) && nzchar(article$description)) {
          paste0("<div class='meta'>", article$description, "</div>")
        } else {
          ""
        },
        "</div>"
      )

      # If content_html includes its own outer content-section wrapper (from fallback converter),
      # strip it to avoid nested wrappers.
      if (is.character(content_html) && grepl("^\\s*<div[^>]*class=['\"]?content-section['\"]?[^>]*>", content_html, perl = TRUE)) {
        content_html <- sub("^\\s*<div[^>]*class=['\"]?content-section['\"]?[^>]*>", "", content_html, perl = TRUE)
        content_html <- sub("</div>\\s*$", "", content_html, perl = TRUE)
      }

      # Remove a leading H1 in the content if it duplicates the header title
      if (is.character(content_html) && nzchar(article$title)) {
       # Escape regex metacharacters in the title for safe insertion into a pattern
       esc_title <- gsub("([\\\\.\\^\\$\\|\\(\\)\\[\\]\\{\\}\\*\\+\\?\\-])", "\\\\\\1", article$title, perl = TRUE)
        h1_pattern <- sprintf("^\\s*<h1[^>]*>\\s*%s\\s*</h1>\\s*", esc_title)
        content_html <- sub(h1_pattern, "", content_html, perl = TRUE, ignore.case = TRUE)
      }

      full_html <- paste0(
        "<div class='content-section'>",
        header_html,
        content_html,
        "</div>"
      )

      output$content_area <- renderUI({
        HTML(full_html)
      })
      # Scroll the content area into view (namespaced id) so new articles start at the top
      js_id <- session$ns("content_area")
  # Run slightly after render to ensure DOM is updated
  shinyjs::runjs(sprintf("setTimeout(function(){ var el = document.getElementById('%s'); if(el){ el.scrollIntoView({behavior: 'smooth', block: 'start'}); el.setAttribute('tabindex','-1'); el.focus(); window.scrollBy(0, -10); } }, 60);", js_id))
    }
    
    # Handle TOC clicks
    observeEvent(input$toc_item, {
      article_id <- input$toc_item
      current_article(article_id)
      display_article(article_id)
    })
    
    # Initialize with first article
    observe({
      content <- filtered_content()
      if (length(content) > 0 && current_article() == "introduction") {
        first_article <- names(content)[1]
        current_article(first_article)
        display_article(first_article)
      }
    })
    
    # Update content when search changes
    observeEvent(input$search_input, {
      content <- filtered_content()
      if (length(content) > 0) {
        first_article <- names(content)[1]
        current_article(first_article)
        display_article(first_article)
      }
    })
    
    # Add click handlers for TOC items (use namespaced input so module receives events)
    observe({
      js_id <- session$ns("toc_item")
      js_code <- sprintf("$(document).on('click', '.toc-item', function() { var articleId = $(this).data('article'); if (articleId) { Shiny.setInputValue('%s', articleId, {priority: 'event'}); } });", js_id)
      shinyjs::runjs(js_code)
    })
    
    # Initialize with first article on startup
    observe({
      if (current_article() == "introduction") {
        display_article("introduction")
      }
    })
    
    # Force display of introduction article when module loads
    observeEvent(session, {
      display_article("introduction")
    }, once = TRUE)
  })
}

