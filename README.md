# Powering Education: Evaluating Kenya's Last Mile Connectivity Program Using Distance-Based Instrumental Variables

---

## Overview

This repository contains the replication code for a study estimating the causal effect of household electrification on children's educational outcomes in Kenya. Using data from the 2014 and 2022 Kenya Demographic and Health Surveys (DHS), I instrument household electricity access with distance to a hypothetical pre-1989 transmission network to address the endogeneity of electrification.

**Main findings:**
- Electrification has **no significant effect on school enrollment**, consistent with Kenya's already high baseline enrollment rate of 89%
- Electrification **increases years of schooling by 0.603 years** (24.3% relative to the sample mean), significant at p < 0.01
- Effects are **concentrated among girls**: +1.082 years for girls vs. +0.18 years for boys (p = 0.38), consistent with reduced domestic time burdens
- Results are robust to using least-cost distance as an alternative instrument and to excluding major urban counties

---

## Data

This project uses the following data sources. **DHS microdata cannot be publicly redistributed.** Free registration is required at [dhsprogram.com](https://dhsprogram.com).

| Source | Description | Access |
|--------|-------------|--------|
| Kenya DHS 2014 | Household, Person, and GPS files | [dhsprogram.com](https://dhsprogram.com) |
| Kenya DHS 2022 | Household, Person, and GPS files | [dhsprogram.com](https://dhsprogram.com) |
| NASA SRTM 30m DEM | Elevation and slope | Public |
| OpenStreetMap (Geofabrik Kenya, 2024) | Major roads | Public |
| HydroSHEDS / HydroRIVERS v1.0 | Major rivers (Strahler order ≥4) | Public |
| Kenya Population and Housing Census 1989 | 1989 city populations and locations | Public |

Once downloaded, place DHS files in:
```
Data/Raw/DHS/2014/
Data/Raw/DHS/2022/
Data/Raw/DHS/GPS/
```

---

## Repository Structure

```
kenya-electrification-education/
│
├── README.md
├── Data_Sources.md
│
├── 01_effect_of_electrification_person_level_data_management.R
│       Load and clean DHS person, household, and GPS files.
│       Merge into a person-level panel. Compute distance to
│       transmission lines as first instrument.
│
├── 02_effect_of_electrification_controls_management.R
│       Construct geographic controls: elevation, slope, distance
│       to major roads, rivers, and 1989 urban centers.
│
├── 03_effect_of_electrification_least_cost_distance.R
│       Build the hypothetical pre-1989 transmission network.
│       Compute Euclidean and least-cost distances to the network
│       for each DHS cluster. Merge instruments into analysis file.
│
├── 04_a_analysis_script_descriptives.R
│       Descriptive statistics table, outcome figures by
│       electrification status, electrification-by-distance
│       figure, and balance table (near vs. far from network).
│
├── 04_b_analysis_script_main_results.R
│       OLS and IV (2SLS) estimates for enrollment and years of
│       education. Heterogeneity by gender. Robustness checks:
│       least-cost instrument, urban exclusion.
│
│
├── Figures/                        # Generated figures (not tracked)
└── Tables/                         # Generated tables (not tracked)
```

---

## Replication Instructions

1. Register and download DHS data from [dhsprogram.com](https://dhsprogram.com) (see `Data_Sources.md` for exact file names)
2. Download spatial data sources listed above
3. Open `Final Project.Rproj` in RStudio — this sets the working directory automatically via `here()`
4. Run scripts in order: `01` → `02` → `03` → `04_a` → `04_b`

All outputs are written to `Figures/` and `Tables/`.

---

## Identification Strategy

I construct a hypothetical pre-1989 electricity transmission network following Faber (2014), connecting Kenya's pre-1989 hydroelectric and geothermal generation facilities to major urban centers (1989 population > 50,000) using minimum spanning tree algorithms. Each DHS cluster's Euclidean distance to the nearest network segment serves as the primary instrument. Least-cost distance — accounting for terrain and river crossings — is used as an alternative instrument in robustness checks.

The instrument is valid under the assumption that proximity to this historical, simulated network affects household electrification rates but has no direct effect on children's educational outcomes except through electricity access.

---

## Software

| Tool | Version | Purpose |
|------|---------|---------|
| R | 4.3+ | All analysis |
| RStudio | — | IDE |

Key packages: `tidyverse`, `haven`, `sf`, `terra`, `fixest`, `flextable`, `officer`, `here`, `patchwork`

Install all at once:
```r
install.packages(c("tidyverse", "haven", "sf", "terra", "fixest",
                   "flextable", "officer", "here", "patchwork", "conflicted"))
```

---

## Citation

If you use this code, please cite:

> Alulu. (2026). *Powering Education: Evaluating Kenya's Last Mile Connectivity Program Using Distance-Based Instrumental Variables*.

---

## Contact

Vincent Alulu· [valulu@ucsd.edu]
