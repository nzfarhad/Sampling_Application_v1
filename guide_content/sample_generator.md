# Sample Generator

Draws the sample from your uploaded frame (CSV or Excel, up to 30 MB) and records how it was drawn.

## Basic Settings

- **Sample size:** the main sample (the total across strata for stratified designs).
- **Buffer (%):** reserve units for replacements, drawn separately and numbered in the order they should be used (`reserve_order`).
- **Random seed:** makes the draw reproducible. If you leave it blank, a random seed is chosen and recorded.
- **Unit ID variable:** used to check for duplicate units and to fill the replacement log.

## Methods

- **Simple random:** every unit has the same chance of selection.
- **Systematic:** a random start and a fixed interval. Sort the list by a variable such as district to spread the sample.
- **Cluster:** select clusters (e.g. villages) with probability proportional to size (PPS) or equal probability, then m units within each. Use "population units" when the file lists every unit, or "cluster list" when it lists clusters with a population column.
- **Stratified:** choose the strata variable and the allocation (proportional, equal, Neyman or manual), with an optional minimum per stratum. The allocation preview shows the split; for Neyman or manual allocation, double-click the Input column to edit it. Units are selected by simple random or systematic sampling within each stratum.
- **Stratified cluster:** the cluster method runs separately in each stratum, each with its own seed derived from the master seed.

## Frame Checks

Clicking Generate first shows a frame report: duplicate or missing IDs, rows with no stratum or cluster (excluded and counted), clusters with no size, clusters smaller than m, and village names repeated across strata. Errors block the draw. Otherwise, review the report and click **Confirm and Draw Sample**.

If many clusters have fewer units than m, the sample falls short. Reduce m, or merge small neighbouring clusters, before drawing.

## Outputs

- **Design columns** at the start of the sample: `sample_type`, `reserve_order`, `stratum`, `cluster_id`, the selection probabilities (`p_stage1`, `p_stage2`, `p_incl`) and `base_weight`. The weights add up to the frame size.
- **Excel sheets:** Sample, Methodology (settings, seed, exclusions, warnings, version), Allocation, Cluster Summary, Frame Checks, Column Guide and Replacement Log.
- A CSV download of the sample, and a cluster summary for cluster designs.
