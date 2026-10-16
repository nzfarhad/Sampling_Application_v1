# Evaluation Sample Sizes

## Overview

An evaluation or PDM comparison asks whether a change happened: between treatment and comparison households, or between baseline and endline. The sample must be large enough to detect the change the programme expects, with a stated power (usually 80%) and significance level (usually 5%). This page explains the choices behind the Evaluation Calculator.

## Proportions and Means

- **Proportions** (share of households with poor food consumption, share with a latrine): the variance is p(1 - p), so only the baseline proportion and the change are needed.
- **Means** (food consumption score, rCSI, HDDS, income, spending, yield): the variance is the standard deviation squared (SD²). Enter the baseline mean, the SD and the change in the indicator's units.

### Choosing the SD

- Take the SD from an earlier survey of a similar population: an ATR baseline or PDM, or a national dataset such as the food security assessments.
- Use the SD of the same unit and recall period (household FCS, monthly income).
- Income and spending are skewed: their SD is often as large as the mean. A larger SD gives a larger sample, so do not round it down.
- If no SD is known, state the change as a **standardised effect** (Cohen's d = change ÷ SD). As a guide, d = 0.2 is a small effect, 0.5 medium and 0.8 large. Most programme effects on household outcomes are small (0.1 to 0.3).

### Relative changes and rare outcomes

- A target such as "reduce stunting by 20%" is a relative change: 20% of a 0.40 baseline is 0.08, or 8 percentage points. The calculator shows both.
- For proportions below 0.10 or above 0.90, the normal approximation is weak. The calculator shows a warning and, for two independent groups, the arcsine (Cohen's h) sample size for comparison. Use the larger of the two, and consider a larger sample.

## Designs

| Design | Variance factor (means) | When to use it |
|--------|-------------------------|----------------|
| Two independent groups | σ²(1 + 1/k) | Treatment and comparison once, or baseline and endline with new samples |
| Panel | 2σ²(1 - ρ) | The same households at baseline and endline, no comparison group |
| Difference-in-differences, cross-sections | 2σ²(1 + 1/k) | Treatment and comparison areas, new samples in each round |
| Difference-in-differences, panel | 2σ²(1 - ρ)(1 + 1/k) | The same treatment and comparison households in both rounds |
| ANCOVA | σ²(1 - ρ²)(1 + 1/k) | As panel DiD, but the endline outcome is compared controlling for baseline |

k is the allocation ratio (comparison : treatment) and ρ the correlation between a household's baseline and endline values.

### ANCOVA and when to use it

With a panel of treatment and comparison households, there are three ways to analyse the data:

- **Endline only:** compare endline outcomes. Variance factor 1.
- **Difference-in-differences:** compare the changes. Variance factor 2(1 - ρ).
- **ANCOVA:** compare endline outcomes, controlling for each household's baseline value. Variance factor 1 - ρ².

ANCOVA always needs the smallest sample. Difference-in-differences needs **more** households than an endline-only comparison whenever ρ is below 0.5, which is common for household outcomes (McKenzie, 2012). The calculator shows the three side by side when panel DiD is selected and ρ is below 0.5.

Use difference-in-differences when the treatment and comparison groups differ at baseline and you rely on parallel trends (no random assignment). Use ANCOVA when the groups are similar at baseline, for example after random assignment, and to gain power.

### Choosing ρ

- ρ is the correlation between the same household's baseline and endline values. It depends on the indicator and the time between rounds.
- Typical values for household survey outcomes are 0.2 to 0.5: towards 0.5 for income, consumption and assets; towards 0.2 for food security scores, rCSI and other volatile indicators, and for long gaps between rounds.
- Use a value from earlier panel data when there is one. If not, use 0.3 for planning and do not go above 0.5: overstating ρ leaves the study underpowered.

## Several Key Indicators

Most evaluations have several key indicators. The calculator sizes each one under the same design and uses the largest sample, and names the indicator that drives it. Each indicator can have its own ICC, since village-level services cluster more than household behaviour.

**Bonferroni correction.** When several indicators are tested and any significant result would be read as success, the chance of a false positive rises. The Bonferroni correction divides α by the number of indicators. It is conservative and raises the sample, so it is off by default. Use it when no single primary indicator has been named in advance.

## Subgroups

Donors often want results for female-headed households, provinces or programme components.

- **Detect the difference within each subgroup:** each subgroup must reach the full sample on its own, so the total is the full sample ÷ the smallest subgroup's share. A subgroup that is 20% of the population makes the sample five times larger.
- **Report the overall result only:** the sample is sized for the whole group, and the calculator shows the minimum detectable difference in each subgroup, as information.
- **Oversampling:** when a small subgroup matters, give it a fixed sample (for example, 400 female-headed households drawn as a separate stratum) rather than inflating the whole sample. Weight the results in the analysis.

## Design Effect

The design effect (DEFF) inflates the sample for clustering and unequal weights.

- **ICC and interviews per cluster:** DEFF = 1 + (m - 1) × ICC.
- **Unequal cluster sizes:** when the number of interviews varies between clusters, DEFF = 1 + ((CV² + 1) × m̄ - 1) × ICC, where m̄ is the mean number per cluster and CV the coefficient of variation of the cluster sizes (SD ÷ mean).
- **Known DEFF:** from an earlier survey of the same indicator and design.
- **Weighting effect (Kish):** unequal weights, for example from disproportionate stratification or oversampled subgroups, multiply the DEFF by 1 + CV² of the weights. Compute the CV from the `base_weight` column of the Sample Generator output.

## Field Adjustments

- **Inaccessible clusters:** insecurity can make whole villages unreachable. This is separate from household non-response. The calculator selects clusters needed ÷ (1 - inaccessible share), so that enough clusters remain. Follow the Replacement Protocol for clusters that become inaccessible.
- **Few clusters:** with fewer than 20 clusters per group, normal (z) values understate the sample. The calculator uses t-values with the cluster-level degrees of freedom (the clusters in all groups minus the number of groups).
- **Small programmes:** when a group has few eligible units, enter its population. The finite population correction lowers the sample. When the sample is more than half the population, a census of that group may be simpler.
- **Matched comparisons (propensity score matching):** some units fall outside common support and are dropped. A factor of 1.2 to 1.5 allows for this. Use it only when the comparison group is built by matching after data collection.

## Reporting

The calculator writes a methods paragraph for the inception report, with the design, the indicators, every assumption and its value, the design effect, the adjustments and the final sample. Copy it into the report and keep the Excel or PDF download in the study file.

## Reference

McKenzie, D. (2012). Beyond baseline and follow-up: The case for more T in experiments. *Journal of Development Economics*, 99(2), 210-221.
