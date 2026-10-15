# Tests for the wiki search (modules/wiki_module.R)

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressMessages({
  library(shiny)
  library(shinyjs)
  source(file.path(root, "modules", "wiki_module.R"))
})

test_that("wiki: search matches article text, not only titles", {
  withr::local_dir(root)   # the module reads wiki_content/ from the app folder
  testServer(wiki_server, {
    session$setInputs(search_input = "design effect")
    expect_true("cluster" %in% names(filtered_content()))
    session$setInputs(search_input = "KoboToolbox")
    expect_equal(names(filtered_content()), "sampling_textbooks")
    session$setInputs(search_input = "(")   # not treated as a regex
    expect_type(filtered_content(), "list")
    session$setInputs(search_input = "")
    expect_gt(length(filtered_content()), 5)
  })
})

test_that("wiki: every listed article exists and renders, including tables", {
  withr::local_dir(root)
  testServer(wiki_server, {
    session$setInputs(search_input = "")
    for (a in filtered_content()) {
      path <- file.path("wiki_content", a$content)
      expect_true(file.exists(path), info = path)
      html <- markdown::markdownToHTML(text = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                                       fragment.only = TRUE)
      expect_match(html, "<h[12]", info = path)
    }
    expect_true(all(c("site_verification", "phone_monitoring", "replacement_protocol") %in% names(filtered_content())))
    session$setInputs(search_input = "reserve_order")
    expect_true("replacement_protocol" %in% names(filtered_content()))
  })
  html <- markdown::markdownToHTML(file.path(root, "wiki_content", "replacement_protocol.md"), fragment.only = TRUE)
  expect_match(html, "<table", fixed = TRUE)
})
