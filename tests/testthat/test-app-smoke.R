# Smoke test: launch the whole app in a separate R process and check that it serves every tab

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

test_that("app launches and serves every tab", {
  skip_if_not_installed("callr")
  skip_if_not_installed("curl")

  port <- httpuv::randomPort()
  app <- callr::r_bg(function(dir, port) shiny::runApp(dir, port = port, launch.browser = FALSE),
                     args = list(dir = root, port = port))
  on.exit(app$kill(), add = TRUE)

  url <- paste0("http://127.0.0.1:", port)
  page <- NULL
  for (i in 1:60) {
    page <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
    if (!is.null(page) || !app$is_alive()) break
    Sys.sleep(1)
  }
  if (is.null(page)) fail(paste("App did not start:", paste(app$read_all_error_lines(), collapse = "\n")))

  expect_equal(page$status_code, 200)
  html <- rawToChar(page$content)
  for (tab in c("How to Use", "Decision Tree", "Sample Calculator", "Evaluation Calculator",
                "Sample Generator", "Sampling Guideline")) {
    expect_match(html, tab, fixed = TRUE)
  }
  expect_match(html, "Data protection", fixed = TRUE)
  expect_match(html, paste("ATR Sampling Tool - version", readLines(file.path(root, "VERSION"))[1]), fixed = TRUE)
})

test_that("app runs in a browser (shinytest2)", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(root, name = "smoke", load_timeout = 60000)
  on.exit(app$stop(), add = TRUE)
  app$set_inputs(main_navbar = "Evaluation Calculator")
  app$wait_for_idle()
  expect_match(app$get_text("#evaluation_calculator-headline"), "Sample per group")
})
