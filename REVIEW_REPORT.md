# Sampling Application — Review for Third-Party Monitoring Use

**Date:** 2026-10-14 · **Scope:** all R code, modules, wiki content, README, tests (working tree, incl. uncommitted changes)

## 1. Summary

The app is a useful teaching and planning tool. It is **not yet safe to use for producing samples on TPM assignments.** It has five problems that will affect fieldwork:

1. **Stratified and stratified-cluster sample generation is not implemented.** Both appear in the menu but only show a "future update" message. Most TPM designs are stratified (by province, district or sex).
2. **The Sample/Buffer split is biased.** For systematic and cluster draws, the main sample does not cover the whole frame.
3. **Rows are silently dropped.** Any sampled unit with a blank cell in any column is removed.
4. **No weights or selection probabilities are exported.** The analysis team cannot weight results or document the design.
5. **The calculator only handles proportions.** There is no sample size for means, comparisons, power/MDE or LQAS, which evaluations and verification work need.

## 2. Incorrect — code and statistics

| # | Issue | Where | Effect |
|---|---|---|---|
| 1 | The first *n* rows are labelled "Sample" and the rest "Buffer". Systematic and cluster outputs are in frame or cluster order, not random order. | `sampling_generator_module.R:913-918` | **Systematic:** the main sample covers only the top ~n/(n+b) of the list. **Cluster:** the buffer is whole clusters, not reserves within clusters. Both bias the sample. |
| 2 | `complete.cases()` drops any sampled row with an NA in *any* column. | `sampling_generator_module.R:887` | Sample comes back short, with no warning. Units with incomplete records (often the hardest to reach) are excluded, which adds bias. |
| 3 | Systematic interval uses `k = floor(N/n)` and keeps the first *n* hits. | `sampling_generation_functions.R:171-179` | Units at the end of the list have zero chance of selection. Use a fractional interval or a circular systematic draw. |
| 4 | Cluster summary never shows: the code checks for `"cluster_sampling"`, but the method value is `"cluster"`. | `sampling_generator_module.R:927, 939` | No cluster table on screen or in the Excel export. |
| 5 | The design effect uses "cluster size", which the UI and guide describe as the *average units per cluster*. The correct *m* is the **number of interviews per cluster**. | `sample_calculator_module.R:31`, `user_guide_module.R:259`, `cluster_sampling.md:79-89` | Entering village size (e.g. 200) gives a DEFF of about 13 and a hugely inflated *n*. |
| 6 | Stratified *n* applies the full SRS formula to every stratum and adds them up. There is no proportional, Neyman or national-level allocation. The user guide promises proportional/equal allocation. | `sample_calculator_module.R:14-24, 39-51`, `user_guide_module.R:249` | Over-samples when only a national estimate is needed. The guide and the app disagree. |
| 7 | The margin-of-error plots compute MoE from *n* including the non-response inflation. They ignore DEFF and FPC. | `sample_calculator_module.R:1381, 1399, 1442` | Precision is overstated, especially for cluster designs. |
| 8 | PPS-with-replacement cluster draw: a cluster picked *k* times needs *k*×m units. If it is smaller, `slice_sample` silently returns fewer. Both `if` branches are identical. | `sampling_generation_functions.R:108-114` | Sample comes back short, with no warning. |
| 9 | Cluster-list PPS draws `n_units` again for each hit, independently. | `sampling_generation_functions.R:349-365` | The same household can appear twice in the sample. |
| 10 | Cluster-list population is `sum()` of the population column over rows. | `sampling_generation_functions.R:285`, `sampling_generator_module.R:599` | If the file is unit-level with the cluster population repeated on each row, the size measure is multiplied by the row count and the PPS probabilities are wrong. |
| 11 | Seed NA handling only exists in the PPS branch. | `sampling_generator_module.R:794, 834` | A blank seed gives an error for the other methods. |
| 12 | "Sampling fraction" and the summary cards use the requested *n*, not the achieved *n*. | `sampling_generator_module.R:948-953` | The reported numbers do not match the downloaded file. |
| 13 | The cluster calculator's *N* is silently replaced by the uploaded population sum. | `sample_calculator_module.R:1182-1188` | The user-entered *N* is ignored. |

## 3. Incorrect — wiki, guide, decision tree

- **Decision tree logic** (`decision_tree_module.R`):
  - **Phone branch (466-476):** ignores the generalisation answer, so it can still recommend SRS or RDD when "no generalisation" was chosen.
  - **Partial frame:** treated like a complete frame (482-488), with no coverage warning.
  - **Convenience and snowball:** recommended too broadly (507-512).
  - **Dead branch:** lines 513-517 can never run.
  - **Method cards:** say SRS suits "comparison of subgroups" (279), which contradicts the tree's own logic.
  - **RDD:** offered with no caveat for Afghanistan, where there is no number-allocation frame and women have low phone access.
- **Wiki statements:**
  - Systematic sampling is called "more efficient than SRS" and also "DEFF ≈ 1" (`systematic_sampling.md`). These contradict each other.
  - Equal allocation is described as "for strata of similar size". It is actually for equal precision per stratum.
  - Stratified sampling is called "more expensive than SRS", which is unfounded.
  - SRS selections are called "independent", which is false without replacement.
- **References** (`sampling_textbooks.md`, `Resources.md`):
  - Several books appear not to exist: Kalton's "…Health/Agricultural/Business Surveys", Lohr's "…Health Surveys/Market Research", and a NIST sampling handbook.
  - Lohr 3rd edition is CRC Press, 2021, not Cengage 2019.
  - Typos: "Wold Bank", "Profissionals".
  - Several URLs look invented or outdated (`online_resources.md` statcan/worldbank paths).
- **Software list:** outdated entries — SamplePower (discontinued), Stata `sampsi` (replaced by `power`), PC CARP, RStudio Cloud (now Posit Cloud). **KoboToolbox and SurveyCTO are missing.**
- **Wiki pages not shown in the app:** `sampling_in_fragile_contexts.md`, `case_studies.md`, `online_resources.md`, `sampling_textbooks.md`, `software_tools.md`. The user guide and introduction link to the fragile-contexts page, which is not registered.
- **README:**
  - Says the sampling tool is "under development". This is stale.
  - Claims mean estimation, which is not implemented.
  - The install list still includes packages only the deleted `sampling_functions_v2.r` used, and omits `markdown` and `stringr`.
  - PDF export needs LaTeX/tinytex, which is not documented.
  - Claims an MIT licence, but there is no LICENSE file.

## 4. Missing for TPM work

**Sample design and calculation**
- Stratified and stratified-cluster generation, with allocation options (proportional, equal, Neyman, power) and a minimum *n* per stratum.
- Sample size for **means**, **two-group differences**, **baseline vs endline**, and **power/MDE** for evaluations.
- **LQAS** decision rules and sample tables, for service-delivery and verification checks.
- **Site/sub-project verification sampling:** census vs PPS of sites, back-check percentages, and sampling from the client's sub-project lists.
- **Phone/remote monitoring:** sampling from beneficiary lists, call-attempt and call-back rules, and coverage bias.
- **Qualitative and purposive sampling** for KIIs and FGDs: number of groups, saturation, and selection criteria.

**Fieldwork protocols**
- A **replacement protocol:** randomised reserve lists per cluster or stratum, ordered for use, plus a log of replaced units and reasons such as insecurity, refusal or not found.
- **Gender disaggregation:** separate female samples or quotas, and guidance on reaching women under current restrictions.

**Outputs, analysis and data handling**
- **Design weights and inclusion probabilities** in the output (stage-1 and stage-2 probabilities, base weight, cluster/stratum IDs). Optionally non-response weight adjustment.
- **A methodology/audit sheet in exports:** parameters, seed, R version, app version, date, user, and frame row count. TPM clients ask for this to verify randomisation.
- **Data protection:** a warning about uploading beneficiary personal data to a public host (shinyapps.io), and guidance on anonymised frames. There are no do-no-harm or ethics notes beyond one line.
- **Frame checks:** duplicates, missing IDs, zero or blank size measures, and a coverage summary before drawing.

**Context and testing**
- **Context defaults:** ICC values for typical Afghan indicators (the 0.06 default has no justification) and Afghan or humanitarian case studies (current case studies are US-centric).
- **Tests:**
  - `test_app.R` sources a missing `global.R`.
  - It loads `plyr` after `dplyr`, so plyr functions mask dplyr's.
  - It skips the generator module.
  - There are no unit tests checking that selection probabilities are correct.

## 5. Code quality / housekeeping

- Unused/legacy code: `sampling_functions_v2.r` is deleted. Nothing still sources it, but the README depends on it.
- `stringr::` is used (`wiki_module.R:577`) but not declared. The fallback when `markdown` is missing renders a full HTML page with broken styling.
- The 100 MB upload limit (`app.R:7`) on shinyapps.io risks running out of memory.
- Debug `cat()` calls write file paths to the logs (`wiki_module.R:631-633`).
- The PDF report puts the study name into the YAML title without escaping, so a `"` breaks rendering. It also prints raw codes such as `less_70`.
- `.gitignore` has no `*.xlsx`/`*.csv` rule. The untracked `All Wheat Package Wheat PDM Dataset_subset_HARMONIZED.xlsx` could be committed by accident.
- The wiki search only matches titles and descriptions, not article text.

## 6. Prioritised actions

| Priority | Action |
|---|---|
| P1 — before field use | Fix the Sample/Buffer split (shuffle, or draw reserves separately per cluster/stratum). Remove the `complete.cases` drop. Fix the systematic interval. Fix the cluster summary method name. Relabel cluster size as "interviews per cluster". |
| P1 | Implement stratified and stratified-cluster generation, or hide them from the menu. |
| P1 | Export selection probabilities, weights and a methodology sheet (seed, parameters, versions). |
| P1 | Add a data-protection warning and decide on hosting for personal data. |
| P2 | Calculator: allocation options, means, two-group comparison/MDE, LQAS. MoE plots should use the effective *n*. |
| P2 | Fix the decision-tree branches (phone, partial frame, convenience/snowball), with Afghanistan caveats. |
| P2 | Register or remove the orphan wiki pages. Remove the fabricated references and invented URLs. Update the software list (add Kobo/SurveyCTO). |
| P3 | Add TPM content: site verification, remote monitoring, replacement protocol, gender, qualitative sampling, ICC guidance. Fix the README and `test_app.R`, and add unit tests. |
