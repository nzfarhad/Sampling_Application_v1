You are already in the project root. Do not use `cd`.
Use relative paths, and use `git -C <subdir>` instead of `cd <subdir> && git ...`.
Never read or search files outside this project folder.

# Project

ATR Sampling Tool: an R Shiny app (version in `VERSION`) for a third-party monitoring firm in Afghanistan. It has six tabs: How to Use, Decision Tree, Sample Calculator, Evaluation Calculator, Sample Generator and Sampling Guideline. It is deployed on shinyapps.io, so users must upload anonymised data only, with a 30 MB upload limit.

## Layout

- `app.R`: loads packages, then sources `sampling_generation_functions.R`, `sample_size_functions.R` and `decision_tree_functions.R` before the modules, and defines the navbar.
- `sampling_generation_functions.R`: the sample selection engine, allocation, frame checks and weights. No Shiny code. `generate_sample_design(data, spec)` is the entry point; the spec is a plain list.
  - Cluster designs take one of two frames (`spec$cluster_method`):
    - `"population_units"` (Method 1): one row per unit; `two_stage_cluster_sampling()` selects clusters, then units within them.
    - `"cluster_list"` (Method 2): one row per cluster with an optional size column; `cluster_list_sampling()` returns one row per selected cluster with an `interviews` column, and units are chosen in the field. IDs come from `cluster_list_ids()`, which adds the row number to repeated names, and are computed on the whole list so they match the cluster summary. Allocation uses each stratum's total size.
  - Reserve clusters (`spec$reserve_clusters`, per stratum) come from `draw_reserve_clusters()`. They are drawn after the main sample, so adding them never changes the main sample for the same seed; keep it that way.
  - `sample_type` is "Sample", "Buffer", "Reserve cluster" or "Reserve cluster buffer". Count the main sample with `== "Sample"`, never with `!= "Buffer"`.
  - When `DESIGN_COLUMNS` changes, update `DESIGN_COLUMN_NOTES` in the generator module: its meanings are listed in the same order.
- `sample_size_functions.R`: monitoring (precision) and evaluation (power/MDE) formulas. No Shiny code. Evaluation scenarios are plain lists; `evaluation_run()` sizes each key indicator and keeps the largest, then adds the indicator, subgroup and analysis-comparison tables, warnings, steps and the methods text (`evaluation_report_text()`). Add new optional scenario fields with `eval_opt()` defaults, so older calls keep working.
- `decision_tree_functions.R`: decision tree questions, designs (`DESIGNS`) and rules (`DESIGN_RULES`, first match wins). No Shiny code. The module hands settings to other tabs through the `apply_handover()` function each receiving module returns, wired in `app.R`.
- `modules/`: one file per tab.
  - Evaluation Calculator key indicators are blocks of inputs with ids `ind_<id>_<field>`. Ids are never reused after a block is removed (`ind_ids()`), so earlier values are kept; `indicator_table()` collects them.
  - Sample Generator draw warnings and notes go to one box (`draw_messages`, grouped by `group_draw_messages()`), with one short pop-up. Do not add a `showNotification()` per warning.
  - `wiki_module.R` is a generic knowledge-base page used by both the Sampling Guideline (`wiki_content/`) and How to Use (`guide_content/`).
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
- Write R scripts with the Write tool or a Bash heredoc, not PowerShell `Set-Content` (its BOM breaks Rscript). Inside R strings, an escaped bracket such as `"\\("` is easy to break when the script passes through a heredoc or another script; write a regex class such as `"[(]"` instead.

## Tests

- All R tests (including an app launch test): `& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" -e "testthat::test_dir('tests/testthat', reporter = 'summary')"`.
  - "W" marks for packages built under a newer R version are harmless.
  - The shinytest2 test is skipped because shinytest2 is not installed.
  - The run takes about 2 minutes. About 20 seconds are `test-evaluation-validation.R` (simulations and grids against `power.prop.test`, `power.t.test` and `pwr`), and about 50 are `test-decision-tree.R`, which calls `recommend_design()` on every distinct set of answers (157,443). Keep `recommend_design()` and `decision_facts()` cheap: for example, `trimws()` costs about 0.14 ms a call, so it only runs when there is text to trim.
  - When a new answer or tick box is added to the decision tree, update the expected counts in that test.
- Full check: `Rscript test_app.R`.
- UI check with Playwright, which uses the installed Microsoft Edge:
  1. Start the app with `shiny::runApp(port = 8787, launch.browser = FALSE)` in the background. Restart it after changing R code; CSS changes only need a page reload.
  2. Run `npm --prefix tests/playwright run check`.
     PDF downloads are checked by fetching the link inside the page; a real browser download leaves a tab that crashes `context.close()`.
  3. Look at the screenshots in `tests/playwright/screenshots/`.
  - In ad-hoc scripts, set selectize inputs with `el.selectize.setValue(v)` in the page. A DT cell edit is saved on blur, not on Enter: double-click the cell, fill it, then blur it.
- Dummy frames for manual checks (git-ignored, in the project root): `Sample_From_Cluster_List_dummy_data.xlsx` (1,292 rows: 1,291 villages plus one row without a cluster name; 8,161 households in `Population`; 30 village names repeated across provinces) and `Sample_from_population_list_dummy_data.xlsx` (8,162 households). readxl names the columns `0.1 Province Name-Coded` and so on, not the dotted names openxlsx gives.
- Known values:
  - SRS default (N 10,000, p 0.5, 95%, e 5%, 5% non-response) = 390.
  - Evaluation regression case (D 0.10, p1 0.15, power 0.8, α 0.05, response rate 0.8, ICC 0.1, m 6) = 310 → DEFF 1.5 → 78 clusters → 468 per group.
  - Evaluation Calculator defaults in the browser: 660 per group; mean (40, SD 12, change 3) = 430; the two default indicators = 810, set by the proportion.
  - Evaluation reported case (several indicators, known DEFF 1.5, m 6, response rate 90%, matching 1.3) = 546 and 1,014 per group.
  - Cluster list dummy file, Method 2, PPS, m 5, n 600, seed 123456: 115 clusters, 577 interviews; weight × interviews adds up to 8,161.
- plotly 4.10.4 with ggplot2 4.x breaks `ggplotly()`, so draw charts with native `plot_ly()`.
