# Site and Sub-Project Verification Sampling

## Overview

Third-party monitoring often checks whether sub-projects (schools, clinics, canals, roads, water points) exist, were built to standard and are in use. The sampling question is which sites to visit, and what the results say about the whole portfolio.

## Census or Sample?

| Situation | Approach |
|-----------|----------|
| Few sites (roughly 30 or fewer) that can all be reached | Visit every site (census). There is no sampling error. |
| The client needs a verdict on **each** site (e.g. payment depends on it) | Census of the sites concerned. A sample cannot give site-level verdicts. |
| Many sites, and the question is about the **portfolio** (e.g. "what share of completed sites are functional?") | Probability sample of sites, with results weighted back to the portfolio. |
| A few sites are flagged as high-risk (complaints, large contracts) | Visit all flagged sites, and sample the rest. Report the two groups separately. |

## Building the Frame

The frame is usually the client's sub-project list or MIS export. Before sampling:

- Agree the **frame date** and the definition of an eligible site (e.g. "completed between January and June").
- Remove duplicates, and check that every row has a unique sub-project ID. The Sample Generator's frame check reports duplicate and missing IDs.
- Check that the stratification fields (province, sector, implementing partner) are filled in.
- Record any sites excluded before sampling (e.g. cancelled contracts) and the reason. They are out of scope, not "inaccessible".

## Design

1. **Stratify** by what the report must break down: province or region, sector, and implementing partner are common. Use the Stratified method in the Sample Generator.
2. **Allocate:**
   - *Proportional* when only the portfolio-level result matters.
   - *Equal* or a *minimum per stratum* when each stratum (e.g. each partner) needs its own result.
3. **Select within strata** by simple random sampling. Select with probability proportional to contract value only when the question is about money, e.g. "what share of funds went to sites that are not functional?". The results must then be weighted by the inverse of each site's selection probability.
4. **Draw reserves** (Buffer %) for sites that turn out to be inaccessible. Use them in the order given by `reserve_order` (see the Replacement Protocol page).

## Sample Size

The question is usually a proportion of sites (e.g. % functional), so use the Sample Calculator with simple random or stratified sampling. With a small portfolio the finite population correction matters.

**Worked example:** 400 completed sites, expected proportion 0.5 (unknown), 95% confidence, ±10% margin:

- n₀ = 1.96² × 0.25 / 0.10² = 96.0
- n = 96.0 / (1 + 95.0 / 400) = 77.6, so **78 sites** before reserves.
- At ±5% the same portfolio needs 197 sites, almost half of it. Consider a census of the strata that matter most instead.

## Field Verification Quality

- Give field teams a **fixed checklist** per sector with photo, GPS and timestamp evidence, so a site visit is comparable across teams.
- Plan **back-checks** on a share of visited sites (for example 5–10%, chosen at random from the completed list), by a different team or by phone. Record differences.
- Note when a site cannot be identified at the given location. "Site not found" is a finding, not a reason for replacement, unless access was the problem.

## Reporting

- State the frame (source, date, number of sites) and the exclusions.
- Give the number of sites sampled, visited, replaced (by reason) and not reached.
- Weight results when strata were not sampled in proportion (use `base_weight` from the Sample Generator export), or report by stratum only.
- Report inaccessible sites as a coverage gap, by stratum, rather than hiding them through replacement.

## In This Tool

- **Sample Calculator:** number of sites for a target margin of error (stratified "Each stratum" if every stratum needs its own result).
- **Sample Generator:** stratified draw with the sub-project ID as the Unit ID, a buffer for reserves, and a Methodology sheet that records the frame, seed and exclusions.
