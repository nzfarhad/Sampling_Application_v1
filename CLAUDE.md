You are already in the project root. Do not use `cd`.
Use relative paths, and use `git -C <subdir>` instead of `cd <subdir> && git ...`.
Never read or search files outside this project folder.

# Project

ATR Sampling Tool: an R Shiny app (version in `VERSION`) for a third-party monitoring firm in Afghanistan. It has six tabs: How to Use, Decision Tree, Sample Calculator, Evaluation Calculator, Sample Generator and Sampling Guideline. It is deployed on shinyapps.io, so users must upload anonymised data only, with a 30 MB upload limit.

## Layout

- `app.R`: loads packages, then sources `sampling_generation_functions.R`, `sample_size_functions.R` and `decision_tree_functions.R` before the modules, and defines the navbar.
- `sampling_generation_functions.R`: the sample selection engine, allocation, frame checks and weights. No Shiny code.
- `sample_size_functions.R`: monitoring (precision) and evaluation (power/MDE) formulas. No Shiny code.
- `decision_tree_functions.R`: decision tree questions, designs (`DESIGNS`) and rules (`DESIGN_RULES`, first match wins). No Shiny code. The module hands settings to other tabs through the `apply_handover()` function each receiving module returns, wired in `app.R`.
- `modules/`: one file per tab. `wiki_module.R` is a generic knowledge-base page used by both the Sampling Guideline (`wiki_content/`) and How to Use (`guide_content/`).
- `www/styles.css`: the app's own styles, linked from `wiki_module.R`. Brand colours are set in `bs_theme()` in `app.R` (primary `#730202`). Decision tree questions use the `.dt-q` block style, so each question stands out from its options; questions are not numbered.
- `IMPLEMENTATION_PLAN.md` and `REVIEW_REPORT.md`: the review and the plan behind version 2.0.0. Decisions D1–D9 are recorded there.

## Conventions

- Put formulas and sampling logic in the function files, keep them free of Shiny code, and add a testthat test for each one.
- Guide and guideline content is markdown: register new articles in `WIKI_ARTICLES` or `GUIDE_ARTICLES`. Use no emoji or decorative icons in the content.
- Put temporary files inside the project (e.g. `*.tmp.*`) and delete them afterwards. Never commit data files: `*.xlsx`, `*.csv` and `resources_temp/` are git-ignored.
- Commits: no "Co-Authored-By: Claude" or any other Claude attribution line.

## Tools on this machine (Windows)

- R: `& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"`. Run it from the PowerShell tool, because Rscript crashes when started from Bash.
- Python does not work here. Node.js 22 does.
- For git commit messages, use Bash heredocs (`git commit -F - <<'EOF'`). PowerShell here-strings do not pipe into git correctly.
- Edit files with the Edit tool rather than PowerShell `Replace`. Some files use CRLF line endings, so multi-line string replacements in PowerShell fail silently.

## Tests

- All R tests (including an app launch test): `& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" -e "testthat::test_dir('tests/testthat', reporter = 'summary')"`.
  - "W" marks for packages built under a newer R version are harmless.
  - The shinytest2 test is skipped because shinytest2 is not installed.
  - The run takes about 95 seconds. About 50 of them are `test-decision-tree.R`, which calls `recommend_design()` on every distinct set of answers (157,443). Keep `recommend_design()` and `decision_facts()` cheap: for example, `trimws()` costs about 0.14 ms a call, so it only runs when there is text to trim.
  - When a new answer or tick box is added to the decision tree, update the expected counts in that test.
- Full check: `Rscript test_app.R`.
- UI check with Playwright, which uses the installed Microsoft Edge:
  1. Start the app with `shiny::runApp(port = 8787, launch.browser = FALSE)` in the background. Restart it after changing R code; CSS changes only need a page reload.
  2. Run `npm --prefix tests/playwright run check`.
  3. Look at the screenshots in `tests/playwright/screenshots/`.
- Known values:
  - SRS default (N 10,000, p 0.5, 95%, e 5%, 5% non-response) = 390.
  - Evaluation regression case (D 0.10, p1 0.15, power 0.8, α 0.05, response rate 0.8, ICC 0.1, m 6) = 310 → DEFF 1.5 → 78 clusters → 468 per group.
- plotly 4.10.4 with ggplot2 4.x breaks `ggplotly()`, so draw charts with native `plot_ly()`.
