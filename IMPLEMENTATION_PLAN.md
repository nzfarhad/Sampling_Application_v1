# Sampling Application — Implementation Plan

**Based on:** `REVIEW_REPORT.md` (2026-10-14) and `resources_temp/evaluation_sample_size_calculator.xlsx` (used only as a reference for the formula)
**Status:** Decisions recorded 2026-10-14 — edit freely. Tick boxes as work completes.

**How to read this:**
- Phases run in order. Within a phase, tasks can run in parallel.
- `[R#]` refers to the issue number in `REVIEW_REPORT.md` section 2.
- Size: **S** < half a day, **M** 1–2 days, **L** 3+ days.

---

## 0. Decisions

| # | Question | Decision (2026-10-14) |
|---|---|---|
| D1 | Should stratified and stratified-cluster generation be built now? | **Build both now** (Phase 2). |
| D2 | Where will the app run when users upload beneficiary lists? | **Stay on shinyapps.io.** Add a warning and require anonymised IDs only (1.3, 5.3.1). |
| D3 | Which evaluation designs are in the first release? | **Two proportions, MDE mode, and panel/DiD.** Two means is out of scope for now. |
| D4 | Should LQAS be included? | **No.** Removed from the plan. |
| D5 | Should the wiki reference pages be kept? | **Removed**, except `Resources.md` (done, see 4.4). |
| D6 | Is there a target date? | **No fixed date.** Work through the phases in order. |
| D7 | Which monitoring calculator items stay? | **Single mean and stratified allocation.** Site verification calculator removed. |
| D8 | What role does the Excel calculator play? | **Formula reference only.** Its values don't matter. The detectable difference defaults to 10% and must be adjustable. |
| D9 | Which TPM guidance pages to write (Phase 5.2)? | **Core field pages only** (5.2.1–5.2.3), answered 2026-10-15. |

---

## Phase 1 — Correctness fixes (must finish before any field use)

**Goal:** what the app already offers is statistically correct and honest about what it produced.

### 1.1 Sample generator — `sampling_generator_module.R`, `sampling_generation_functions.R`

**Sample/buffer and dropped rows**
- [x] **[R1] Fix the Sample/Buffer split (M).** Do not label the "first n rows" as Sample.
  - SRS: draw *n + b* and then shuffle.
  - Systematic: draw *n* systematically, then draw the buffer at random from the units not selected (or as a second systematic pass with an offset start).
  - Cluster: draw reserves *within* each selected cluster.
  - Add a `reserve_order` column (1, 2, 3 …) so field teams use reserves in order.
- [x] **[R2] Remove `complete.cases()` (S).** Never drop sampled rows. Add a warning that counts the rows with missing values in key columns (ID, cluster, size measure) only.

**Selection logic**
- [x] **[R3] Systematic: use a fractional interval (S).**
  - Set `k = N/n` and a random start `r ~ U(0, k]`.
  - Select `ceiling(r + (i-1)k)`.
  - Add an optional "sort by" variable for implicit stratification.
- [x] **[R8] PPS undersized clusters (S).** When a cluster is selected *k* times and has fewer than *k*·m units, take all of them and show a warning that lists those clusters and the shortfall. Remove the duplicate `if` branch.
- [x] **[R9] Cluster-list PPS duplicates (S).** For a cluster hit *k* times, draw *k*·m units once, without replacement, not *k* separate draws.
- [x] **[R10] Cluster-list size measure (S).**
  - Detect whether the population value is constant within each cluster. If it is, use it once; otherwise sum it.
  - Show which rule was applied.
  - Add a UI note explaining the expected file shape.

**Display and inputs**
- [x] **[R4] Cluster summary (S).** Change the checks to `method %in% c("cluster", "stratified_cluster")`, so the summary shows on screen and is included as a sheet in the Excel export.
- [x] **[R11] Seed (S).** Require a seed for all methods: either validate it or default it, then display and export it.
- [x] **[R12] Summary cards (S).** Use the achieved *n* for the sample size, sampling fraction and buffer counts.

### 1.2 Calculator — `sample_calculator_module.R`

- [x] **[R5] Cluster size meaning (S).**
  - Rename the input to **"Interviews per cluster (m)"**.
  - Update the help text, user guide (`user_guide_module.R:259`) and wiki (`cluster_sampling.md:79-89`).
  - Output **number of clusters = ceiling(n / m)** and round *n* up to clusters × m.
- [x] **[R7] Margin-of-error plots (S).** Compute the margin of error from the *effective* *n*: completed interviews ÷ DEFF, with the finite population correction applied.
- [x] **[R13] Cluster *N* override (S).** Don't silently replace the user's *N*. Show "N taken from uploaded file" or let the user choose.

### 1.3 Safety and housekeeping

- [x] **Data-protection notice on upload screens (S).** The app stays on shinyapps.io (D2), so state it plainly: "This app runs on a public cloud host. Do not upload names, phone numbers, GPS points or other identifiers. Use anonymised IDs and link back to the full list offline." Show it on both the calculator and generator upload screens.
- [x] **Data files out of git (S).** Add `*.xlsx`, `*.csv` and `resources_temp/` to `.gitignore`. Keep `All Wheat Package … HARMONIZED.xlsx` out of git.
- [x] **Remove debug output (S).** Delete the `cat()` calls at `wiki_module.R:631-633`.
- [x] **Declare packages (S).** Load `stringr` and `markdown` in `app.R` (or replace the `stringr` call).

**Phase 1 status (2026-10-14):** implemented. 18 tests in `tests/testthat/` pass: sampling functions, generator and calculator server logic. The Wheat PDM frame was checked with `tests/manual_check_wheat.R`. Not yet checked by hand in a browser. The calculator's stratified detailed-calculation table still uses total N for its intermediate rows; that is left to 3b.2.

**Exit criteria for Phase 1:**
- Unit tests (Phase 5.1) pass for SRS, systematic and both cluster modes.
- A manual run on the Wheat PDM file produces the expected *n*, no dropped rows, and a cluster summary.

---

## Phase 2 — Outputs TPM clients need

**Goal:** every sample file is defensible and can be analysed.

- [x] **2.1 Stratified generation (L)** *(decision D1)*
  - Choose the strata variable and an allocation method: equal, proportional, Neyman (if an SD/proportion per stratum is given), or a manual table.
  - Set a minimum *n* per stratum.
  - SRS or systematic within each stratum.
  - Reserves drawn per stratum.
- [x] **2.2 Stratified-cluster generation (L).** Run the cluster logic within each stratum, with independent seeds derived from the master seed and recorded in the output.
- [x] **2.3 Design variables in every output (M).** Add these columns:
  - `stratum`, `cluster_id`
  - `p_stage1`, `p_stage2`, `p_incl`
  - `base_weight = 1/p_incl`
  - `selection_hits` (PPS with replacement)
  - `sample_type`, `reserve_order`
- [x] **2.4 Methodology sheet in the Excel export (M).** Include:
  - Method and parameters, seed, and allocation table
  - Frame file name, frame row count, frame rows excluded and why
  - Date and time, app version (git hash or `VERSION` file), R version, user-entered study name
- [x] **2.5 Frame checks before drawing (M).** Report:
  - Duplicate IDs, missing cluster or stratum values
  - Zero or NA size measures
  - Clusters smaller than *m*
  - A summary by stratum

  The user confirms before the draw.
- [x] **2.6 Replacement log template (S).** Add a downloadable sheet with columns: original unit, replacement unit, reason (insecure / refused / not found / other), date, enumerator.

**Phase 2 status (2026-10-14):** implemented. 31 tests in `tests/testthat/` pass, including base weights summing to N per stratum. An Excel export from the server test opens with all sheets: Sample, Methodology, Allocation, Cluster Summary, Frame Checks, Column Guide and Replacement Log. The app starts and serves the new controls. `tests/manual_check_wheat.R` runs every design on the Wheat PDM frame. Not yet clicked through in a browser.

Implementation notes:
- **Frame checks (2.5):** shown in a confirmation dialog after "Generate Sample". Errors block the draw.
- **Neyman and manual inputs (2.1):** entered by double-clicking the Input column of the allocation preview.
- **Version (2.4):** read from the new `VERSION` file, plus the git commit when git is available.
- **Weights (2.3):** PPS-with-replacement weights follow the Hansen-Hurwitz estimator. They stay correct when a cluster falls short.
- **Wheat PDM frame finding:** 832 of 1,267 villages have fewer than 8 units. A PPS draw of 400 at m = 8 gave 332 interviews. The frame check now suggests a smaller m or merging clusters when more than 20% are too small. 28 village names appear in more than one province.

**Exit criteria for Phase 2:**
- Base weights sum to roughly the frame *N*, per stratum, within rounding. A test checks this.
- The export opens in Excel with all sheets.

---

## Phase 3 — Evaluation and monitoring calculators

**Goal:** cover sample-size needs beyond a single proportion. Scope follows D3, D4, D7 and D8.

### 3a. Evaluation calculator — first release

**Formula reference.** `resources_temp/evaluation_sample_size_calculator.xlsx` serves only as a formula example (D8). Its values do not matter.

```latex
n_{\text{per group}} = \frac{(z_{1-\alpha/2} + z_{\beta})^2 \,[\,p_1(1-p_1) + p_2(1-p_2)\,]}{(p_1 - p_2)^2} \cdot \frac{1}{RR} \cdot \big(1 + (m-1)\,ICC\big)
```

This is the standard two-sample comparison of proportions (unpooled variance, two-sided). It holds for **independent groups**: repeated cross-sections at baseline and endline, or treatment vs control at one time point. Panel and DiD designs need the extensions in 3a.3 and 3a.4.

**Design rules carried over from the spreadsheet review:**
- **Rounding:** round clusters up, then set *n* = clusters × m. The spreadsheet left fractions (e.g. 77.5 clusters).
- **Direction:** allow an increase or a decrease (e.g. stunting), or let the user enter p2 directly.
- **Steps shown in order:** base *n* → non-response adjustment → DEFF → clusters → final *n*, as in the existing calculator.
- **Labels:** say "n per group (per survey round / per arm)", not "n/2".
- **Cluster size:** *m* = interviews per cluster (same fix as [R5]).

**Tasks:**
- [x] **3a.1 Two-proportion calculator (M).**
  - Inputs: power, α, one- or two-sided test, p1, detectable difference D (**default 10%, adjustable**, increase or decrease) or p2, response rate, ICC, m, and an allocation ratio (default 1:1).
  - Outputs: *n* per group, total *n*, DEFF, clusters per group, and the steps shown.
  - Offer a pooled-variance (Fleiss) option alongside unpooled.
- [x] **3a.2 MDE mode (M).** Given *n* per group, or a fixed number of clusters and *m*, solve for the minimum detectable difference. TPM budgets are often fixed, so this matters. Covers all designs in 3a.
- [x] **3a.3 Panel design (M).** The same households are interviewed at baseline and endline. Use the baseline–endline correlation ρ to reduce the variance of the change: Var(diff) ∝ 2p(1–p)(1–ρ). Default ρ = 0, with guidance on choosing it. Add an attrition rate on top of non-response.
- [x] **3a.4 Difference-in-differences, four groups (M).**
  - Groups: treatment and control × baseline and endline.
  - Inputs: the four expected proportions, or a baseline value plus the expected change in each arm. Optional ρ for panel designs.
  - Output: *n* per cell and in total, plus clusters per arm.
- [x] **3a.5 Power curve (S).** A plot of power against *n* (or D) for the current inputs.
- [x] **3a.6 Regression tests (S).**
  - Known-value check, inputs D = 10%, p1 0.15, power 0.80, α 0.05, response rate 0.80, ICC 0.10, m 6. Expected: 310 per group before clustering, DEFF 1.5, 78 clusters, 468 per group after rounding.
  - Check panel/DiD and MDE results against Stata `power` or R `pwr` for at least three cases each.

*Two means is out of scope for this release (D3).*

### 3b. Monitoring calculators

- [x] **3b.1 Single mean (S).** A one-sample calculator for continuous indicators (income, yield), alongside the existing proportion one. Fixes the README claim.
- [x] **3b.2 Stratified allocation in the calculator [R6] (M).** Choose between "precision per stratum" (current behaviour, now labelled) and "precision for the total" with proportional, Neyman or equal allocation. Make the user guide match.

*LQAS (D4) and the site-verification calculator (D7) are removed from scope.*

**Phase 3 status (2026-10-15):** implemented. All tests in `tests/testthat/` pass; there are two new files, `test-sample-size.R` and `test-evaluation-server.R`, plus extra calculator server tests.

- **Regression case (3a.6):** gives 310 → DEFF 1.5 → 78 clusters → 468 per group.
- **Checks against R:**
  - Pooled independent groups and pooled MDE match `stats::power.prop.test`, 3 cases each.
  - Panel matches `pwr::pwr.t.test(type = "paired")` within 2.5, 3 cases. DiD matches `power.t.test` within 2.5, 3 cases. These two use a t-test, so their n is 1–2 higher.
  - In simulation, the unpooled and panel (correlated pairs) tests reach 80% ± 3% power.
- **Browser:** both calculator screens were checked in headless Edge.

Implementation notes:
- **Formulas:** all in the new `sample_size_functions.R`, which has no Shiny code. `monitoring_calculation()` builds one result, including the step table. The calculator's screen table, Excel and PDF all use it, which also fixes the leftover per-stratum N issue in the step table.
- **Evaluation calculator:** a new **Evaluation Calculator** tab (`modules/evaluation_calculator_module.R`).
  - Designs: independent groups, panel, DiD as repeated cross-sections, and DiD as a panel.
  - Modes: sample size or MDE.
  - D defaults to 0.10 and can be changed, as an increase or a decrease, or you can enter p₂ instead.
  - Options: unpooled or pooled variance, allocation ratio, one- or two-sided test, response rate, attrition (panel designs), ρ, ICC and m.
  - Clusters are rounded up per group. A power curve and an Excel export (Summary, Calculation, Groups, Power Curve) are included.
- **Sample Calculator:**
  - New indicator choice: proportion or mean. For a mean you enter σ and a margin in the indicator's units.
  - New "Precision target" choice for the stratified methods:
    - Each stratum: the previous behaviour, now labelled.
    - Whole population only: proportional, Neyman or equal allocation, with a minimum per stratum.
  - Each stratum can take an optional p or σ, entered by hand or taken from a file column.
  - The breakdown shows each stratum's expected margin of error.
- **Charts:** drawn with native `plot_ly`. `ggplotly()` fails with plotly 4.10.4 and ggplot2 4.0.1 (the versions installed here), so the old calculator chart was already broken in this environment.

**Exit criteria for Phase 3:** all 3a.6 tests pass, and each calculator matches Stata `power`/R `pwr` or a published table for at least three cases.

---

## Phase 4 — Decision tree, wiki, guide, README

- [x] **4.1 Decision tree fixes (M)** — `decision_tree_module.R`
  - Phone branch: respect the generalisation answer and any constraints.
  - Partial frame: show a coverage warning and suggest frame-building or a dual-frame design.
  - Restrict convenience and snowball recommendations to suitable cases (snowball only for hidden populations).
  - Remove the dead branch at 513-517.
  - Fix the SRS card text at line 279.
  - Add an Afghanistan caveat to RDD (no frame, low phone access for women).
  - Add a "census of all sites" option when the number of sites is small.
  - Escape the study name in the PDF YAML and print readable labels instead of raw codes.
- [x] **4.2 Wiki corrections (M)**
  - Systematic: remove "more efficient than SRS" or qualify it ("when the list is ordered by a related variable").
  - Stratified: correct the equal-allocation definition and remove "more expensive".
  - SRS: remove "independent selections".
  - Cluster: define *m* as interviews per cluster and add a worked example with clusters.
- [x] **4.3 References (M)** *(decision D5)*
  - `Resources.md` (the only reference page left): fix Lohr (CRC Press, 2021) and the typos ("Wold Bank", "Profissionals").
  - Check every URL by opening it.
  - Optionally add a short list of field tools (KoboToolbox, SurveyCTO) to `Resources.md`, since the software page was deleted.
- [x] **4.4 Orphan pages (S).** Done 2026-10-14. Deleted `sampling_in_fragile_contexts.md`, `case_studies.md`, `online_resources.md`, `sampling_textbooks.md` and `software_tools.md`. Removed the links to the fragile-contexts page from `user_guide_module.R` and `sampling_introduction.md`.
- [x] **4.5 Wiki search (S).** Include article text in the search.
- [x] **4.6 User guide (S).** Match the guide to the app: allocation options, interviews per cluster, the new calculators and generator features.
- [x] **4.7 README (S).**
  - Current feature status and the correct package list.
  - LaTeX/tinytex requirement for PDF export.
  - Add a LICENSE file, or remove the MIT claim.

**Phase 4 status (2026-10-15):** implemented. All tests pass, with two new files: `test-decision-tree.R` (it checks every one of the 288 answer combinations) and `test-wiki.R`.

- **Decision tree (4.1):**
  - The logic is now a standalone function, `recommend_sampling()`, that returns the methods plus a list of caveats. The app shows the caveats in a "Points to consider" box and adds them to the PDF.
  - If generalisation is not needed, only non-probability methods are suggested, for phone surveys too.
  - Snowball is suggested only for the new "hidden or hard-to-reach group" frame option. Convenience is suggested only on a limited budget, with a "pilots only" note.
  - A partial frame gives a coverage warning and suggests a dual-frame design.
  - RDD carries an Afghanistan caveat.
  - The new "few enough to visit all" option recommends the new "All sites, sample within each" method.
  - Remote or insecure areas get a note about reserves and recording inaccessible sites.
  - The dead branch is gone, and the SRS card text is fixed.
  - The PDF escapes the study name (tested with quotes) and prints readable answers instead of codes.
- **Wiki (4.2):** the systematic-sampling efficiency claims are now qualified, and its R and Excel examples use a fractional interval. The equal-allocation definition is fixed and the "more expensive" claims are removed. "Independent" is gone from the SRS definition. The cluster page already defined m (Phase 1).
- **Resources (4.3):** Lohr is now CRC Press/Chapman & Hall, 3rd ed. 2021 (© 2022). The typos are fixed and KoboToolbox and SurveyCTO were added. Each link was checked by opening it: the World Bank document page and StatTrek resolve, and so do Routledge (for Lohr) and SurveyCTO. KoboToolbox blocks automated requests (HTTP 403), so it was not confirmed this way.
- **Search (4.5):** the wiki search now covers the article text, as a fixed-string match.
- **User guide (4.6):** rewritten to match the app, including the Evaluation Calculator, precision targets, mean indicators, generator allocation, frame checks, outputs and the data-protection notice.
- **README (4.7):** describes the current features, the correct package list, the TinyTeX requirement for PDFs, the project structure and how to run the tests. The MIT claim is removed and no LICENSE was added; the licence is ATR's decision.
- `library(ggplot2)` was removed from `app.R`; no module uses it now.
---

## Phase 5 — Testing and TPM content

### 5.1 Tests

- [x] **5.1.1 `tests/testthat/` (M).** Cover:
  - Calculator formulas against known values (including the 3a.6 check).
  - Systematic draws: every unit selectable, with inclusion probability ≈ n/N over 10,000 seeds.
  - PPS: empirical selection frequency is proportional to the size measure.
  - No duplicate units; weights sum to *N*.
  - Same seed → same sample.
- [x] **5.1.2 Fix `test_app.R` (S).** Remove `global.R` and `plyr`, add the generator and user-guide modules, and add a `shinytest2` smoke test that launches the app.

### 5.2 TPM guidance content (new wiki pages)

*Scope (D9, 2026-10-15): core field pages only. 5.2.4–5.2.8 are dropped.*

- [x] **5.2.1 Site and sub-project verification sampling (M).**
- [x] **5.2.2 Phone and remote monitoring sampling (M).** Cover:
  - Beneficiary-list frames
  - Call-attempt and call-back rules
  - Coverage bias
  - Female respondents
- [x] **5.2.3 Replacement protocol (S).** Link to the log template from 2.6.
- ~~**5.2.4 Gender-disaggregated sampling under current restrictions (S).**~~ *Dropped (D9).*
- ~~**5.2.5 Qualitative sampling for KIIs and FGDs (S).** Number of groups, saturation and selection criteria.~~ *Dropped (D9).*
- ~~**5.2.6 ICC guidance (S).** A reference table of typical ICC values by indicator type, with sources (e.g. DHS/MICS published DEFFs). Justify or change the 0.06 default.~~ *Dropped (D9).*
- ~~**5.2.7 Ethics and do-no-harm (S).** Short and practical.~~ *Dropped (D9).*
- ~~**5.2.8 Case studies (M).** The old `case_studies.md` was deleted (4.4). If wanted, add 2–3 anonymised ATR-style examples: a PDM survey, site verification and a baseline/endline.~~ *Dropped (D9).*

### 5.3 Deployment

- [x] **5.3.1 Hosting (S)** *(decision D2: stay on shinyapps.io)*. Reduce the upload limit to fit the shinyapps.io instance memory (e.g. 30 MB), and test with the largest expected frame.
- [x] **5.3.2 Version stamp (S).** Show the version in the footer and in exports (ties into 2.4).

**Phase 5 status (2026-10-15):** implemented. `Rscript test_app.R` checks the packages, loads every module, prints known values and runs the full suite, which passes.

- **Tests (5.1.1):**
  - Systematic inclusion probability is checked over 10,000 seeds. It is within 0.012 of n/N for every unit, and every unit is selectable.
  - New test: for all five designs, the same seed gives the same sample, a different seed gives a different one, and no unit is selected twice.
  - Already covered: PPS frequency ∝ size, weights sum to N, and the 3a.6 regression case.
- **Smoke test (5.1.2):** `test_app.R` is rewritten with the current package list and all six modules, and no longer uses `global.R` or plyr. `test-app-smoke.R` launches the real app in a background R process and checks that all six tabs, the data-protection notice and the version footer are served. A shinytest2 browser test is included but skips because shinytest2 is not installed; install it and Chrome to run it.
- **Field guidance pages (5.2):** three new pages under a new "Field Guidance" wiki section: `site_verification_sampling.md`, `phone_remote_monitoring.md` and `replacement_protocol.md`. The worked numbers were checked in R, and the Replacement Log column list matches the generator export.
- **Hosting (5.3.1):** the upload limit is now 30 MB, and the user guide says so. Load test with a synthetic 29.3 MB CSV (330,000 rows) and a stratified cluster design: frame checks, the draw and the Excel export took 4.7 s, with about 300 MB peak R memory. Weights summed to exactly 330,000. On shinyapps.io, keep the instance at 1 GB or more.
- **Version (5.3.2):** shown in the app footer, in the Methodology sheet, in the calculator Excel and PDF exports, and in the decision-tree PDF. Consider bumping `VERSION` from 1.1.0 for this release.
---

## Suggested order and rough effort

| Phase | Content | Effort | Ready for field use? |
|---|---|---|---|
| 1 | Correctness fixes and safety | ~4–5 days | Yes, for SRS, systematic and cluster |
| 2 | Stratified generation, weights, methodology sheet | ~6–8 days | Yes, for all methods |
| 3 | Evaluation (two proportions, MDE, panel/DiD) and monitoring calculators | ~5–7 days | Adds evaluation support |
| 4 | Decision tree, wiki, guide, README | ~4–5 days | — |
| 5 | Tests and TPM content | ~5–7 days | — |

There is no fixed date (D6), so the phases run in order. The estimates assume one R/Shiny developer. Phase 5.1 tests should be written alongside Phases 1–3 rather than at the end.

---

## Notes and open questions

- D9 answered 2026-10-15: core field pages only.
- Two means (evaluation) is out of scope for the first release. Revisit after Phase 3.
