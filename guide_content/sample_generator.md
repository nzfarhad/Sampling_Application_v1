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
- **Cluster:** select clusters (e.g. villages) with probability proportional to size (PPS) or equal probability, then m interviews in each. There are two kinds of file:
  - **Method 1, Sample from Population Units:** the file lists every unit (e.g. each household) with its village. The app selects the villages and then the households within them.
  - **Method 2, Sample from Cluster List:** each row of the file is one village, with its size (e.g. number of households). The sample lists the selected villages and the `interviews` to do in each (m per selection, or every household when the village is smaller). Households are chosen in the field, e.g. from a listing. Reserves are whole reserve villages. The size is needed for PPS and for the weights. Without it, villages are selected with equal probability and the weights are left blank until the villages are listed. A village name that appears on more than one row (e.g. in two provinces) stays a separate cluster, with its row number added to its ID.
- **Stratified:** choose the strata variable and the allocation (proportional, equal, Neyman or manual), with an optional minimum per stratum. The allocation preview shows the split; for Neyman or manual allocation, double-click a cell in the Input column, type the value, then click outside the cell or press Tab. Units are selected by simple random or systematic sampling within each stratum.
- **Stratified cluster:** the cluster method runs separately in each stratum, each with its own seed derived from the master seed. With a cluster list, interviews are allocated by the households in each stratum (or by the number of villages when there is no size), and the preview shows the villages to select per stratum.

## Frame Checks

Clicking Generate first shows a frame report: duplicate or missing IDs, rows with no stratum or cluster (excluded and counted), clusters with no size, clusters smaller than m, village names repeated across strata, and a file that looks like the wrong kind for the method chosen. Errors block the draw. Otherwise, review the report and click **Confirm and Draw Sample**.

If many clusters have fewer units than m, the sample falls short. Reduce m, or merge small neighbouring clusters, before drawing.

## Outputs

- **Design columns** at the start of the sample: `sample_type`, `reserve_order`, `stratum`, `cluster_id`, the selection probabilities (`p_stage1`, `p_stage2`, `p_incl`) and `base_weight`. The weights add up to the frame size; for a cluster list, `base_weight` is the weight of each interview, and the weights times `interviews` add up to the total size of the list.
- **Excel sheets:** Sample, Methodology (settings, seed, exclusions, warnings, version), Allocation, Cluster Summary, Frame Checks, Column Guide and Replacement Log.
- A CSV download of the sample, and a cluster summary for cluster designs.
