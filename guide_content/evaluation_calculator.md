# Evaluation Calculator

Sample size, or the smallest detectable difference, when comparing a proportion or a mean between groups or survey rounds. The Evaluation Sample Sizes page in the Sampling Guideline explains the choices in more detail.

## Designs

- **Two independent groups:** treatment and control at one time, or a baseline and an endline that each draw a new sample.
- **Panel:** the same households at baseline and endline. Set the baseline-endline correlation (ρ) and the expected attrition.
- **Difference-in-differences:** treatment and control areas at baseline and endline, as repeated cross-sections or as a panel. Enter both baselines and the change expected without the programme.
- **ANCOVA:** a panel of treatment and control households, with the endline outcome compared controlling for baseline. It needs fewer households than difference-in-differences. When panel difference-in-differences is selected and ρ is below 0.5, the calculator shows both, and an endline-only comparison, side by side.

## What to Enter

- Power (default 80%), significance level (default 5%) and a one- or two-sided test.
- **One key indicator:** a proportion, or a mean with its standard deviation. Give the baseline and the change to detect in one of these ways:
  - a difference (0.10 = 10 points for a proportion, or units for a mean);
  - a relative change (% of the baseline, e.g. 20 for "reduce stunting by 20%");
  - a standardised effect (Cohen's d, for means when the SD is uncertain);
  - the second value directly.
- **Several key indicators:** click **Add indicator** for each one. For every indicator give:
  - its name and type (proportion or mean);
  - the baseline (proportions from 0 to 1, so 0.30 for 30%) and, for a mean, the standard deviation;
  - whether you expect an increase or a decrease, and the size of the change;
  - what the size is measured in: points or units, % of the baseline, or Cohen's d (means only).

  The line under each indicator says what the entries mean, for example "0.3 → 0.22: a decrease of 8 percentage points (26.7% of the baseline)". Check it: a size typed in the wrong unit shows up there at once. Leave the ICC blank to use the common ICC. For difference-in-differences and ANCOVA, the control baseline and trend can be given; blank means the same baseline as treatment and no trend. The largest sample is used, and the result names the indicator that drives it. Tick Bonferroni to divide α by the number of indicators.
- The response rate, and for panels the attrition and ρ (typically 0.2 to 0.5; 0.3 is a reasonable planning value).
- The allocation ratio, if one group should be larger (e.g. 2 control households per treatment household).

## Design Effect and Field Adjustments

- **Clustered sample:** the design effect from the ICC and interviews per cluster, from the ICC with unequal cluster sizes (CV), or a known DEFF from an earlier survey.
- **Expected inaccessible clusters:** extra clusters are selected so that enough remain. This is separate from non-response.
- **Fewer than 20 clusters per group:** t-values are used in place of z values (on by default).
- **Unequal weights:** the CV of the weights adds a weighting effect (Kish).
- **Population per group:** for small programmes, the finite population correction lowers the sample.
- **Matched comparison group:** a factor of 1.2 to 1.5 for units lost in matching.

## Subgroups

Tick **Report results by subgroup** and list the subgroups with their share of the sample, or a fixed sample when you will oversample them. Choose the goal:

- **Detect the difference within each subgroup:** the sample grows so that the smallest subgroup reaches the full sample.
- **Report the overall result:** the sample is unchanged, and the minimum detectable difference of each subgroup is shown.

## Two Modes

- **Sample size:** the sample per group, clusters per group and total interviews needed.
- **Minimum detectable difference (MDE):** enter the clusters (or interviews) per group your budget allows, and see the smallest difference you can detect, for every indicator and subgroup.

## Results and Downloads

- Warnings flag rare outcomes, few clusters, samples near the population size, large subgroup factors, and a change so small (or a sample so large) that an input is probably on the wrong scale.
- The **methods text** describes the design, every assumption and the final sample, ready for an inception report. Use **Copy** to copy it.
- The power curve shows how power changes with the sample size, or with the true difference in MDE mode.
- The **Excel** download holds the methods text, the full calculation, the group, indicator and subgroup tables and the curve. The **PDF report** holds the same content for the study file.
