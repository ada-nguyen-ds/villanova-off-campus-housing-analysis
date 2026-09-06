# Villanova Off-Campus Housing Analysis

> **Shared housing was the main path to affordability near Villanova: it accounted for 90% of sampled units at or below $1,500 per person.**

## Project at a glance

| Study component | Result |
|---|---:|
| Eligible Zillow listings | **104** |
| Listings sampled | **40** |
| Rental units observed | **146** |
| Geographic strata | **4** |
| Bootstrap iterations | **1,000** |
| Units at or below $1,500 | **54%** |
| Units at or below $1,200 | **27%** |

## Project question

How do rent per person, housing availability, and affordability vary across Ardmore, Bryn Mawr, Haverford, and Wayne near Villanova University?

I designed the sampling strategy, collected and validated the rental listings, conducted the estimation and bootstrap analysis in R, and built the Tableau dashboard.

## Sampling design

The study used **stratified one-stage cluster sampling**.

- **Strata:** Ardmore, Bryn Mawr, Haverford, and Wayne
- **Clusters:** eligible Zillow rental listings
- **Elements:** individual rental units within listings
- **Sampling frame:** 104 active apartment and condominium listings
- **Final sample:** 40 listings containing 146 units
- **Allocation:** minimum of five listings per stratum, followed by proportional allocation
- **Inference:** cluster bootstrap within strata using 1,000 iterations

Villanova had no eligible apartment listings under the study criteria and was excluded from the final sampling strata.

## Key findings

### Rent per person

The design-based estimate of mean monthly rent per person was approximately **$1,630** (95% bootstrap CI: **$1,414–$1,920**).

| Housing type | Estimate | 95% bootstrap CI |
|---|---:|---:|
| Shared | **$1,233** | $1,124–$1,390 |
| Private | **$1,950** | $1,679–$2,248 |

The intervals show a clear cost difference between shared and private options.

### Estimated rental supply

The sample supported an estimated **357 rental units** across the sampling frame. Estimates for shared and private supply were similar, but their confidence intervals were wide because cluster sizes varied and only 40 listings were sampled.

### Affordability

- At **$1,500 per person**, 54% of sampled units qualified; 90% of those affordable units were shared.
- At **$1,200 per person**, 27% qualified; 97.5% of those units were shared.
- Wayne had the lowest affordability at both thresholds.
- In the descriptive Tableau view, Bryn Mawr combined the lowest average rent per person with the largest observed supply.
- The dashboard estimated the largest private-versus-shared monthly savings in Wayne.

## Interactive dashboard

[Open the Student Housing Market Near Villanova University dashboard on Tableau Public](https://public.tableau.com/app/profile/khanh.nguyen5891/viz/StudentHousingMarketNearVillanovaUni/StudentHousing)

The dashboard lets students and families compare observed rent, housing type, supply, and potential savings across the four communities. Dashboard totals are descriptive summaries of the 146 observed units; inferential estimates and bootstrap intervals are reported separately above.

## Analytical workflow

1. Define the target population and eligibility rules.
2. Construct the Zillow listing sampling frame.
3. Allocate the listing sample across geographic strata.
4. Collect every eligible unit within selected listing clusters.
5. Calculate rent per person and affordability indicators.
6. Estimate stratified means and population totals.
7. Use cluster-level bootstrap resampling for standard errors and confidence intervals.
8. Translate the sample results into an interactive Tableau dashboard.

## Repository structure

```text
R/
  housing_sampling_analysis.R
data/
  housing_sample.csv
output/
report/
visuals/
README.md
```

## Reproduce the analysis

```r
source("R/housing_sampling_analysis.R")
```

The script reads `data/housing_sample.csv` and writes reproducible result tables to `output/`.

## Limitations

The sampling frame covered Zillow listings at one point in time and excluded off-platform rentals, houses, townhomes, short-term listings, and duplicates. Haverford had a small stratum, and cluster-size variation produced uncertainty in total-unit estimates. Results support decisions about the sampled market period and should not be treated as a permanent census of local housing.

## Tools

**R · dplyr · tidyr · stratified cluster sampling · ratio estimation · bootstrap inference · Tableau**
