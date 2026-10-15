# Stratified Sampling

## Overview

Stratified sampling divides the population into homogeneous subgroups (strata) and then selects samples from each stratum. This method ensures representation from all important population subgroups.

## Key Characteristics

- **Population Division**: Population split into non-overlapping strata
- **Every Stratum Represented**: Each stratum gets its own sample (proportional or not)
- **Homogeneous Strata**: Units within strata are similar
- **Heterogeneous Between Strata**: Strata differ from each other
- **Independent Sampling**: Random selection within each stratum

## When to Use

Stratified sampling is ideal when:

- Population has distinct subgroups
- You need estimates for each subgroup
- Strata have different characteristics
- You want to improve precision
- Subgroups have different variances
- Administrative regions are important

## Types of Stratification

### 1. Proportional Stratified Sampling
- Sample size proportional to stratum size
- Each stratum contributes proportionally to total sample
- Maintains population structure in sample

### 2. Equal Allocation
- The same sample size in every stratum, whatever its population
- Useful when each stratum needs its own estimate (e.g. one per province), including small strata
- Gives roughly equal precision per stratum, but needs weights for the overall estimate, which is then less precise than with proportional allocation

### 3. Optimal Stratified Sampling
- Sample allocation based on stratum variance
- Minimizes total sampling error
- Requires knowledge of stratum variances

## Advantages

1. **Improved Precision**: Often more precise than simple random sampling
2. **Subgroup Analysis**: Enables analysis of population subgroups
3. **Representation**: Ensures all subgroups are included
4. **Flexibility**: Different sampling methods within strata
5. **Quality Control**: Easier to monitor and control

## Disadvantages

1. **Complexity**: More complex than simple random sampling
2. **Stratum Definition**: Requires clear stratum boundaries
3. **Sampling Frame**: Needs stratum information for all units
4. **Analysis Complexity**: Needs weights when allocation is not proportional
5. **Stratum Size**: Small strata may limit analysis

## Implementation Steps

1. **Define Strata**: Identify relevant population subgroups
2. **Create Stratified Frame**: Organize sampling frame by strata
3. **Determine Allocation**: Decide sample sizes for each stratum
4. **Select Samples**: Random selection within each stratum
5. **Combine Results**: Aggregate data from all strata
6. **Weight Analysis**: Apply appropriate weights in analysis

## Sample Size Calculation

### Proportional Allocation
```
n_i = (N_i / N) × n
```

### Equal Allocation
```
n_i = n / k
```

Every stratum gets the same sample size regardless of its population. The overall estimate must then be weighted by N_i / N.

### Optimal Allocation
```
n_i = n × (N_i × σ_i) / Σ(N_j × σ_j)

Where
Σ(N_j × σ_j) = (N1 × σ1)+(N2 × σ2)+(N3 ×σ3)+⋯+(Nk ×σk)
```

Where:
- n_i = sample size for stratum i
- N_i = population size for stratum i
- σ_i = standard deviation for stratum i
- k = number of strata

## Stratification Variables

### Common Stratification Variables
- **Demographics**: Age, gender, income, education
- **Geographic**: Region, urban/rural, climate
- **Behavioral**: Usage patterns, preferences
- **Administrative**: Districts, departments, categories
- **Temporal**: Time periods, seasons

### Selection Criteria
- **Relevance**: Related to study objectives
- **Measurability**: Can be identified and measured
- **Mutual Exclusivity**: No overlap between strata
- **Practicality**: Feasible to implement

## Quality Control

- **Stratum Validation**: Verify stratum definitions
- **Frame Accuracy**: Ensure correct stratum assignment
- **Sample Verification**: Check stratum representation
- **Response Monitoring**: Track participation by stratum
- **Weight Validation**: Verify weighting procedures

## Analysis Considerations

### Weighting
- **Design Weights**: Inverse of selection probabilities
- **Post-Stratification**: Adjusting for known population totals
- **Non-Response Adjustment**: Compensating for non-participation

### Variance Estimation
- **Stratum-Specific**: Calculate within each stratum
- **Combined**: Pool estimates across strata
- **Design Effect**: Account for stratification impact

## Common Pitfalls

- **Over-Stratification**: Too many small strata
- **Under-Stratification**: Missing important subgroups
- **Stratum Confusion**: Unclear stratum boundaries
- **Inadequate Allocation**: Poor sample size distribution
- **Analysis Errors**: Incorrect weighting or variance estimation

## Example Applications

- **Public Health**: Disease prevalence by region
- **Education**: Student performance by school type
- **Social Research**: Opinion surveys by age groups
- **Quality Control**: Product defects by production line
- **Market Research**: Customer segments by demographics

## Statistical Software

### R Packages
- `survey`: Complex survey analysis
- `sampling`: Survey sampling methods
- `srvyr`: Tidy survey analysis

### Other Software
- **SPSS**: Complex samples module
- **SAS**: Survey procedures
- **Stata**: Survey commands

## Best Practices

1. **Clear Stratum Definition**: Well-defined, measurable criteria
2. **Adequate Stratum Sizes**: Avoid very small strata
3. **Balanced Allocation**: Consider precision and cost
4. **Quality Frame**: Accurate stratum information
5. **Proper Analysis**: Use appropriate statistical methods
6. **Documentation**: Record all stratification decisions
7. **Validation**: Check stratum representativeness

## Comparison with Other Methods

### vs. Simple Random Sampling
- **Precision**: Generally more precise
- **Complexity**: More complex to implement
- **Cost**: About the same per interview when strata are geographic; a larger total sample only if every stratum needs its own precision
- **Analysis**: Requires weights unless allocation is proportional

### vs. Cluster Sampling
- **Precision**: Usually more precise for the same number of interviews
- **Cost**: Higher travel cost per interview, because units are spread out
- **Implementation**: More complex
- **Geographic Spread**: Better distribution

## Fragile-context adaptations

When working in fragile or humanitarian settings, stratified sampling can still be useful but needs adaptation:

- Use broad, operationally meaningful strata (e.g., accessible vs inaccessible areas, displacement status) rather than many small administrative strata.
- Where stratum membership is uncertain, collect stratum indicators during listing and apply post-stratification weights.
- Combine stratification with cluster sampling when frames within strata are unavailable.
- Prioritise strata critical to program decisions and be explicit about any excluded zones.


## Conclusion

Stratified sampling is a powerful method that improves precision and enables subgroup analysis. Success depends on careful stratum definition, appropriate sample allocation, and proper statistical analysis. While more complex than simple random sampling, the benefits often justify the additional effort.
