# Sample Calculator

Sample size for estimating an indicator with a given margin of error, in one survey round. Use it for monitoring surveys such as post-distribution monitoring.

## Common Parameters

- **Key indicator:** a proportion (e.g. % of households that received aid) or a mean (e.g. income, yield).
- **Proportion:** the expected proportion p (0.5 is the safe default when unknown) and the margin of error (0.05 means ±5 percentage points).
- **Mean:** the standard deviation σ, from an earlier survey or roughly (max - min) / 4, and the margin of error in the indicator's units.
- **Population size (N), confidence level** (default 95%) **and non-response rate** (default 5%).

## Cluster Sampling

- **Interviews per cluster (m):** the number of units you will interview in each selected cluster, not the cluster's total size.
- **ICC:** how alike units in the same cluster are (default 0.06). The design effect is 1 + (m - 1) × ICC.
- The result is rounded up to whole clusters of m interviews.

## Stratified Sampling

- Enter strata by hand (name, population and, optionally, an expected p or σ per stratum), or upload a file and choose the strata and population columns.
- **Precision target, Each stratum:** every stratum reaches the margin of error on its own. Use it when you will report each stratum separately. The stratum sample sizes add up, so the total is larger.
- **Precision target, Whole population only:** one sample sized for the overall estimate and split across strata by proportional, Neyman (optimal) or equal allocation, with an optional minimum per stratum. Cheaper, but single strata are less precise.
- The breakdown table shows each stratum's sample, clusters and expected margin of error.

## Results and Downloads

The calculator applies the design effect, then the finite population correction, then the non-response adjustment. The Detailed Calculations table shows every step. The Excel and PDF downloads contain the same table.
