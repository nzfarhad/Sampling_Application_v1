# Replacement Protocol

## Overview

Some selected units cannot be interviewed: the area is insecure, the household refuses or has moved. Replacing them keeps the sample size, but each replacement can bias the results if it is not done by a fixed rule. This page sets the rules, and explains the Replacement Log sheet in the Sample Generator's Excel export.

## Principles

1. **Replace only by the rule, never by choice.** The enumerator never picks a "nearby" or "easier" household.
2. **Use reserves in order.** The Sample Generator draws reserve units at random and numbers them in `reserve_order`. Use the lowest unused number in the **same stratum** (and the same cluster, for cluster designs).
3. **Every replacement is logged and approved.** If it isn't in the Replacement Log, it didn't happen.
4. **Replacement does not remove non-response bias.** People who refuse or cannot be found often differ from those who take part. Report replacement rates so readers can judge this.

## When a Unit May Be Replaced

| Reason (as in the log) | Replace? | Notes |
|------------------------|----------|-------|
| Insecure / inaccessible | Yes, after supervisor approval | Record what made it inaccessible and when it was checked. If a whole area is inaccessible, see "Whole clusters" below. |
| Refused | Yes | Only after the refusal is final (not "come back later"). |
| Not found / moved | Yes, after the set number of visits or calls | Record the number of attempts. |
| Other | Only with supervisor approval | Describe the reason in `reason_details`. |
| Not eligible (e.g. not a beneficiary) | Do not replace as non-response | This is a frame error. Record it and count it separately; it reduces the eligible frame. |

Set the **number of attempts** before fieldwork, for example 2 visits on different days for households, or 3 to 5 calls for phone surveys (see the Phone and Remote Monitoring page).

## Whole Clusters

The Sample Generator draws reserves **within** each selected cluster, which covers household-level problems. When a whole selected village becomes inaccessible:

- Agree with the sampling lead whether to replace it or report it as a coverage gap.
- If it is replaced, draw the replacement village **by the same method** (for example, rerun the Sample Generator on the unselected villages of the same stratum with a new, recorded seed). Do not let the field team pick a neighbouring village.
- Record it in the log with the cluster ID, and note the change in the Methodology sheet.

## The Replacement Log

The Excel export from the Sample Generator includes a **Replacement Log** sheet with these columns:

| Column | What to enter |
|--------|---------------|
| `original_unit_id` | ID of the selected unit that could not be interviewed |
| `stratum`, `cluster_id` | Copied from the sample sheet |
| `replacement_unit_id` | ID of the reserve unit used |
| `reserve_order_used` | The `reserve_order` number of that reserve |
| `reason` | Chosen from the dropdown: Insecure / inaccessible, Refused, Not found / moved, Other |
| `reason_details` | Short description (e.g. "road closed by flooding, checked 12 May") |
| `date_replaced` | Date of the replacement |
| `enumerator` | Enumerator code |
| `supervisor_approval` | Name or code of the approving supervisor |

Keep one log per survey, filled in daily by supervisors, and send it with the data.

## Thresholds for Review

Review the fieldwork with the sampling lead when replacements pass a set level, for example:

- more than 20% of the units in a stratum, or
- most replacements in a stratum having the same reason, or
- one enumerator replacing far more often than others.

High replacement in one stratum may mean the frame is out of date, or that the stratum should be reported as a coverage gap.

## Analysis and Reporting

- A reserve used as a replacement takes the place of the unit it replaces, and its `base_weight` in the export is the weight it should carry.
- Report, by stratum: units selected, interviewed, replaced (by reason) and not replaced.
- Report inaccessible areas as coverage gaps in the methodology section, with their share of the frame.
