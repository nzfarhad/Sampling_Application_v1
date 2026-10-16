# ATR Sampling Tool

An R Shiny application for choosing a sampling method, calculating sample sizes and drawing documented samples, built for project monitoring, surveys and evaluations by a third-party monitoring team.

## Features

| Tab | What it does |
|-----|--------------|
| **How to Use** | User guide for every tab. |
| **Decision Tree** | Recommends one sampling design from questions about the study purpose, list, reporting groups, mode and constraints: the selection stages, reasons, alternatives and field warnings. Sends the settings to the calculators and the Sample Generator. PDF report. |
| **Sample Calculator** | Monitoring surveys: sample size for a target margin of error, for a proportion or a mean. Simple random, systematic, cluster, stratified and stratified cluster designs; stratified precision per stratum or for the total (proportional, Neyman or equal allocation). Step-by-step table, chart, Excel and PDF downloads. |
| **Evaluation Calculator** | Comparing proportions: two independent groups, panel, and difference-in-differences (cross-section or panel). Sample size or minimum detectable difference, with clustering, non-response, attrition and a power curve. Excel download. |
| **Sample Generator** | Draws the sample from an uploaded frame (CSV/Excel): simple random, systematic, cluster (PPS or equal probability), stratified and stratified cluster. Frame checks before the draw, reserve units, selection probabilities and base weights, and an Excel workbook with Methodology, Allocation, Cluster Summary, Frame Checks, Column Guide and Replacement Log sheets. |
| **Sampling Guideline** | Wiki on sampling fundamentals, methods, quality control and resources, with full-text search. |

## Quick Start

1. **Install R (4.1 or later) and the packages**:
   ```r
   install.packages(c("shiny", "bslib", "shinyjs", "DT", "plotly", "dplyr", "readr",
                      "readxl", "writexl", "openxlsx", "rmarkdown", "knitr",
                      "htmltools", "stringr", "markdown"))
   ```

2. **For PDF reports** (Decision Tree and Sample Calculator), install a LaTeX distribution. The simplest is TinyTeX:
   ```r
   install.packages("tinytex")
   tinytex::install_tinytex()
   ```
   Pandoc is also needed; it comes with RStudio. Without LaTeX the app works, but the PDF downloads fail.

3. **Run the application** from the project folder:
   ```r
   shiny::runApp()
   ```

## Data Protection

The app is hosted on shinyapps.io, a public cloud service. Upload anonymised frames only: no names, phone numbers or GPS points. Keep the full list offline and link it back through the unit ID.

## Project Structure

| Path | Contents |
|------|----------|
| `app.R` | Loads packages, sources the functions and modules, defines the navigation. |
| `sampling_generation_functions.R` | Sample selection, allocation, frame checks, weights. |
| `sample_size_functions.R` | Sample size, margin of error, power and MDE formulas. |
| `decision_tree_functions.R` | Decision tree questions, designs and rules. |
| `modules/` | One file per tab. |
| `wiki_content/` | Markdown articles for the Sampling Guideline. |
| `tests/testthat/` | Automated tests. |
| `VERSION` | App version, shown in the exports. |

## Tests

From the project folder:

```r
testthat::test_dir("tests/testthat")
```

The tests also need `testthat` and `withr`; the evaluation checks use `pwr` when it is installed. `tests/manual_check_wheat.R` runs every design on a local frame file that is not stored in git.

**UI check (Playwright).** Clicks through every tab at desktop and phone width and checks the layout (footer position, overlaps, horizontal scrolling, errors). It needs Node.js and Microsoft Edge:

```sh
PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1 npm --prefix tests/playwright install
Rscript -e "shiny::runApp(port = 8787)"      # in a second terminal
npm --prefix tests/playwright run check
```

Screenshots are saved to `tests/playwright/screenshots/`.

## Contributing

- Put formulas and sampling logic in `sample_size_functions.R` or `sampling_generation_functions.R`, without Shiny code, and add tests for them.
- Put screens in a module under `modules/` and register it in `app.R`.
- Add guideline articles to `wiki_content/` and list them in `modules/wiki_module.R`.

## License

No license has been chosen yet. Add a `LICENSE` file before sharing the code outside ATR Consulting.

## Support

For questions, contact the ATR Data Management team, or see the Sampling Guideline tab in the app.
