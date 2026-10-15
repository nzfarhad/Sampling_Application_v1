# Wiki Module
# Knowledge-base layout (search, table of contents, markdown articles). Used by
# the Sampling Guideline tab and, with its own articles, by the How to Use tab.

# Sampling Guideline articles, grouped by section; each article is a markdown file in wiki_content/
WIKI_SECTIONS <- c(
  fundamentals = "Fundamentals",
  methods = "Sampling Methods",
  best_practices = "Best Practices",
  field_guidance = "Field Guidance",
  further_reading = "Further Reading"
)

WIKI_ARTICLES <- list(
  introduction = list(title = "Introduction to Sampling", content = "sampling_introduction.md",
                      category = "fundamentals", description = "Learn the basics of sampling theory and methodology"),
  probability_vs_nonprobability = list(title = "Probability vs Non-Probability Sampling", content = "probability_sampling.md",
                                       category = "fundamentals", description = "Understanding the differences between sampling approaches"),
  simple_random = list(title = "Simple Random Sampling", content = "simple_random_sampling.md",
                       category = "methods", description = "The most basic form of probability sampling"),
  stratified = list(title = "Stratified Sampling", content = "stratified_sampling.md",
                    category = "methods", description = "Sampling with population subgroups"),
  cluster = list(title = "Cluster Sampling", content = "cluster_sampling.md",
                 category = "methods", description = "Sampling groups of units together"),
  systematic = list(title = "Systematic Sampling", content = "systematic_sampling.md",
                    category = "methods", description = "Selecting every kth unit from a population"),
  quality_control = list(title = "Quality Control in Sampling", content = "quality_control.md",
                         category = "best_practices", description = "Ensuring sampling quality and validity"),
  site_verification = list(title = "Site and Sub-Project Verification", content = "site_verification_sampling.md",
                           category = "field_guidance", description = "Census or sample of sites, frames from client lists, back-checks"),
  phone_monitoring = list(title = "Phone and Remote Monitoring", content = "phone_remote_monitoring.md",
                          category = "field_guidance", description = "Beneficiary-list frames, call rules, coverage bias, female respondents"),
  replacement_protocol = list(title = "Replacement Protocol", content = "replacement_protocol.md",
                              category = "field_guidance", description = "When and how to replace units, and the Replacement Log"),
  sampling_textbooks = list(title = "Resources", content = "Resources.md",
                            category = "further_reading", description = "Books, online resources and field data collection tools")
)

#' Knowledge-base page
#'
#' @param title,subtitle Page header
#' @param search_placeholder Placeholder text of the search box
wiki_ui <- function(id, title = "Sampling Guideline",
                    subtitle = "A quick guide to sampling methodologies, best practices, and statistical foundations",
                    search_placeholder = "Search knowledge base...") {
  ns <- NS(id)

  fluidPage(
    useShinyjs(),
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

        /* Phones: less padding so the article keeps a readable width */
        @media (max-width: 576px) {
          .wiki-container { padding: 8px 0; }
          .wiki-header { padding: 20px 12px; margin-bottom: 16px; }
          .wiki-header h1 { font-size: 1.6rem; }
          .wiki-sidebar { position: static; margin-bottom: 16px; }
          .wiki-content { padding: 16px; }
          .wiki-content ul, .wiki-content ol { margin-left: 0; padding-left: 1.1rem; }
        }
      '))
    ),

    div(
      class = "wiki-container",
      div(class = "wiki-header", h1(title), p(subtitle)),
      div(
        class = "row",
        div(
          class = "col-lg-3",
          div(
            class = "wiki-sidebar",
            div(class = "search-container",
                textInput(ns("search_input"), label = NULL, value = "", placeholder = search_placeholder)),
            uiOutput(ns("toc_container"))
          )
        ),
        div(
          class = "col-lg-9",
          div(
            class = "wiki-content",
            uiOutput(ns("breadcrumb_container")),
            uiOutput(ns("content_area"))
          )
        )
      )
    )
  )
}

# Simple markdown to HTML conversion function (fallback when the markdown package is missing)
convert_markdown_to_html <- function(markdown_lines) {
  html_lines <- character(length(markdown_lines))

  for (i in seq_along(markdown_lines)) {
    line <- markdown_lines[i]
    if (grepl("^###### ", line)) {
      html_lines[i] <- gsub("^###### (.+)$", "<h6>\\1</h6>", line)
    } else if (grepl("^##### ", line)) {
      html_lines[i] <- gsub("^##### (.+)$", "<h5>\\1</h5>", line)
    } else if (grepl("^#### ", line)) {
      html_lines[i] <- gsub("^#### (.+)$", "<h4>\\1</h4>", line)
    } else if (grepl("^### ", line)) {
      html_lines[i] <- gsub("^### (.+)$", "<h3>\\1</h3>", line)
    } else if (grepl("^## ", line)) {
      html_lines[i] <- gsub("^## (.+)$", "<h2>\\1</h2>", line)
    } else if (grepl("^# ", line)) {
      html_lines[i] <- gsub("^# (.+)$", "<h1>\\1</h1>", line)
    } else if (line == "") {
      html_lines[i] <- "<br>"
    } else {
      line <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", line)
      line <- gsub("\\*(.+?)\\*", "<em>\\1</em>", line)
      line <- gsub("`(.+?)`", "<code>\\1</code>", line)
      if (grepl("^- ", line)) {
        html_lines[i] <- gsub("^- (.+)$", "<li>\\1</li>", line)
      } else if (grepl("^\\d+\\. ", line)) {
        html_lines[i] <- gsub("^\\d+\\. (.+)$", "<li>\\1</li>", line)
      } else {
        html_lines[i] <- paste0("<p>", line, "</p>")
      }
    }
  }
  paste(html_lines, collapse = "\n")
}

#' Render one markdown file to an HTML fragment
render_markdown_file <- function(path) {
  md_lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  tryCatch(
    markdown::markdownToHTML(text = paste(md_lines, collapse = "\n"), fragment.only = TRUE),
    error = function(e) convert_markdown_to_html(md_lines)
  )
}

#' Knowledge-base server
#'
#' @param articles Named list of articles (title, content file, category, description)
#' @param sections Named vector: category id = section heading, in display order
#' @param content_dir Folder of the markdown files
#' @param home_label First breadcrumb item
wiki_server <- function(id, articles = WIKI_ARTICLES, sections = WIKI_SECTIONS,
                        content_dir = "wiki_content", home_label = "Knowledge Base") {
  moduleServer(id, function(input, output, session) {

    # Articles in section order, with their text (lower case) for the search box
    flat_content <- list()
    for (category in names(sections)) {
      for (article_id in names(articles)) {
        article <- articles[[article_id]]
        if (article$category != category) next
        md_file <- file.path(content_dir, article$content)
        article$search_text <- if (file.exists(md_file)) {
          tolower(paste(readLines(md_file, warn = FALSE, encoding = "UTF-8"), collapse = " "))
        } else ""
        flat_content[[article_id]] <- article
      }
    }

    current_article <- reactiveVal(names(flat_content)[1])

    # Filter content based on search: title, description or article text (fixed string, not a regex)
    filtered_content <- reactive({
      content <- flat_content
      if (!is.null(input$search_input) && input$search_input != "") {
        search_term <- tolower(input$search_input)
        content <- content[vapply(content, function(x) {
          grepl(search_term, tolower(x$title), fixed = TRUE) ||
            grepl(search_term, tolower(x$description), fixed = TRUE) ||
            grepl(search_term, x$search_text, fixed = TRUE)
        }, logical(1))]
      }
      content
    })

    # Table of contents
    output$toc_container <- renderUI({
      content <- filtered_content()
      if (length(content) == 0) {
        return(div(class = "no-results", h4("No Results Found"),
                   p("Try other search terms, or clear the search box to see every article.")))
      }
      toc <- list()
      for (section_id in names(sections)) {
        section_articles <- content[vapply(content, function(x) x$category == section_id, logical(1))]
        if (length(section_articles) == 0) next
        toc <- c(toc, list(tags$div(class = "toc-section", sections[[section_id]])))
        for (article_id in names(section_articles)) {
          is_active <- identical(current_article(), article_id)
          toc <- c(toc, list(tags$div(
            class = paste("toc-item", if (is_active) "active" else ""),
            `data-article` = article_id,
            role = "button",
            `aria-pressed` = if (is_active) "true" else "false",
            section_articles[[article_id]]$title
          )))
        }
      }
      tagList(toc)
    })

    output$breadcrumb_container <- renderUI({
      article <- flat_content[[current_article()]]
      req(article)
      tags$nav(
        tags$ol(
          class = "breadcrumb",
          tags$li(class = "breadcrumb-item", home_label),
          tags$li(class = "breadcrumb-item", sections[[article$category]]),
          tags$li(class = "breadcrumb-item active", article$title)
        )
      )
    })

    output$content_area <- renderUI({
      article <- flat_content[[current_article()]]
      req(article)
      content_file <- file.path(content_dir, article$content)
      content_html <- if (file.exists(content_file)) {
        render_markdown_file(content_file)
      } else {
        paste0("<p>This article is coming soon.</p>")
      }

      # The header already shows the article title, so drop the article's own leading H1
      content_html <- sub("^\\s*<h1[^>]*>.*?</h1>\\s*", "", content_html, perl = TRUE)

      div(
        class = "content-section",
        div(class = "content-header",
            h1(article$title),
            if (!is.null(article$description) && nzchar(article$description)) div(class = "meta", article$description)),
        HTML(content_html)
      )
    })

    show_article <- function(article_id, scroll = TRUE) {
      if (is.null(flat_content[[article_id]])) return(invisible())
      current_article(article_id)
      if (scroll) {
        # Bring the new article into view
        shinyjs::runjs(sprintf(
          "setTimeout(function(){ var el = document.getElementById('%s'); if (el) { el.scrollIntoView({behavior: 'smooth', block: 'start'}); } }, 60);",
          session$ns("content_area")
        ))
      }
    }

    # Table-of-contents clicks; the handler only listens inside this module's sidebar
    shinyjs::runjs(sprintf(
      "$(document).on('click', '#%s .toc-item', function() { var a = $(this).data('article'); if (a) { Shiny.setInputValue('%s', a, {priority: 'event'}); } });",
      session$ns("toc_container"), session$ns("toc_item")
    ))
    observeEvent(input$toc_item, show_article(input$toc_item))

    # Searching jumps to the first matching article
    observeEvent(input$search_input, {
      content <- filtered_content()
      if (length(content) > 0 && !(current_article() %in% names(content))) {
        show_article(names(content)[1], scroll = FALSE)
      }
    }, ignoreInit = TRUE)
  })
}
