# Raw Data Description

This file documents all data sources, file names, and variables used in the project.
DHS microdata cannot be shared publicly. Follow the instructions below to obtain them.
Place all files exactly as described — scripts use `here()` and expect this folder structure.

---

## 1. Kenya DHS 2014

**Source:** Kenya National Bureau of Statistics (KNBS) & ICF  
**Citation:** KNBS & ICF. (2015). *Kenya Demographic and Health Survey 2014*. Rockville, Maryland: KNBS and ICF.  
**Download:** https://dhsprogram.com/data/dataset/Kenya_Standard-DHS_2014.cfm  
**Access:** Free registration required at dhsprogram.com  
**Place files in:** `Data/Raw/DHS/2014/`

### Files Used

| File | Description |
|------|-------------|
| `KEPR72FL.DTA` | Person recode (individual-level data) |
| `KEHR72FL.DTA` | Household recode (household-level data) |
| `KEIR72FL.DTA` | Individual recode — women 15–49 (used for residence years) |

### Key Variables — Person Recode (`KEPR72FL.DTA`)

| DHS Variable | Recoded Name | Description |
|-------------|--------------|-------------|
| `hv001` | `cluster_id` | Cluster number (merge key) |
| `hv002` | `household_id` | Household number within cluster |
| `hvidx` | `person_id` | Person line number |
| `hv112` | `mother_line` | Line number of mother |
| `hv102` | `usual_resident` | Usual resident (1 = yes) |
| `hv105` | `age` | Age in completed years |
| `hv104` | `female` | Sex (recoded: 1 = female, 0 = male) |
| `hv005` | `weight` | Sample weight (divide by 1,000,000) |
| `hv024` | `county_code` | Region/county code |
| `hv025` | `urban` | Urban/rural (recoded: 1 = urban, 0 = rural) |
| `hv108` | `years_education` | Years of education completed |
| `hv106` | `educ_level` | Highest education level attended |
| `hv121` | `currently_enrolled` | Attended school during current year (0/1) |

**Sample restriction:** Children aged 5–14 (`age >= 5 & age <= 14`)

### Key Variables — Household Recode (`KEHR72FL.DTA`)

| DHS Variable | Recoded Name | Description |
|-------------|--------------|-------------|
| `hv001` | `cluster_id` | Cluster number |
| `hv002` | `household_id` | Household number |
| `hv005` | `weight` | Sample weight |
| `hv024` | `county_code` | Region/county code |
| `hv025` | `urban` | Urban/rural |
| `hv206` | `has_electricity` | Has electricity (1 = yes) — **treatment variable** |
| `hv226` | `electric_lighting` | Uses electricity for lighting (1 = yes) |
| `hv207` | `owns_radio` | Owns a radio (1 = yes) |
| `hv208` | `owns_tv` | Owns a television (1 = yes) |
| `hv209` | `owns_fridge` | Owns a refrigerator (1 = yes) |
| `hv243a` | `owns_mobile` | Owns a mobile phone (1 = yes) |
| `hv270` | `wealth_index` | Wealth index quintile (1–5) |

### Key Variables — Individual Recode (`KEIR72FL.DTA`)

| DHS Variable | Recoded Name | Description |
|-------------|--------------|-------------|
| `v001` | `cluster_id` | Cluster number |
| `v002` | `household_id` | Household number |
| `v003` | `person_id` | Respondent line number |
| `v104` | `v104_years` | Years of residence at current address |

---

## 2. Kenya DHS 2022

**Source:** Kenya National Bureau of Statistics (KNBS) & ICF  
**Citation:** KNBS & ICF. (2023). *Kenya Demographic and Health Survey 2022*. Rockville, Maryland: KNBS and ICF.  
**Download:** https://dhsprogram.com/data/dataset/Kenya_Standard-DHS_2022.cfm  
**Access:** Free registration required at dhsprogram.com  
**Place files in:** `Data/Raw/DHS/2022/`

### Files Used

| File | Description |
|------|-------------|
| `KEPR8CFL.DTA` | Person recode |
| `KEHR8CFL.DTA` | Household recode |
| `KEIR8CFL.DTA` | Individual recode — women 15–49 |

*Variable names are consistent with the 2014 recode. See DHS documentation for any differences.*

---

## 3. DHS GPS Cluster Coordinates

**Source:** DHS Program (requested separately from survey data)  
**Download:** Same registration portal — request GPS datasets for Kenya 2014 and 2022  
**Place files in:** `Data/Raw/DHS/GPS/`

| File | Year | Description |
|------|------|-------------|
| `KEGE71FL.shp` | 2014 | GPS coordinates for all DHS clusters |
| `KEGE8AFL.shp` | 2022 | GPS coordinates for all DHS clusters |

**Note:** DHS displaces cluster coordinates randomly up to 5 km (urban) or 10 km (rural) to protect respondent privacy. Distance measures are therefore noisy — classical measurement error in the instrument affects efficiency but not consistency of IV estimates.

| Shapefile Variable | Description |
|-------------------|-------------|
| `DHSCLUST` | Cluster number (merge key → `cluster_id`) |
| `LATNUM` | Latitude (WGS84) |
| `LONGNUM` | Longitude (WGS84) |

---

## 4. Admin Boundaries

**Source:** GADM v4.1  
**Download:** https://gadm.org/download_country.html (select Kenya, level 2)  
**Place file in:** `Data/Raw/Admin/`

| File | Description |
|------|-------------|
| `gadm41_KEN_2.shp` | Kenya subcounty boundaries (level 2) |

| Shapefile Variable | Recoded Name | Description |
|-------------------|--------------|-------------|
| `NAME_1` | `county` | County name |
| `NAME_2` | `subcounty` | Subcounty name |
| `GID_1` | `county_id` | County ID (factored to integer) |
| `GID_2` | `subcounty_id` | Subcounty ID (factored to integer) |

---

## 5. Electricity Transmission Infrastructure

**Source:** Kenya power grid infrastructure shapefile  
**Place file in:** `Data/Raw/Powerlines/`

| File | Description |
|------|-------------|
| `transmission_lines.shp` | Kenya electricity transmission network |

Used in `01_` to compute each cluster's distance to the transmission network (UTM Zone 37S, EPSG:32737). All line features are unioned into a single geometry before distance calculation so that `st_distance()` returns one value per observation.

---

## 6. Hypothetical Pre-1989 Transmission Network (Instrument)

**Constructed in:** `03_effect_of_electrification_least_cost_distance.R`  
**Method:** Follows Faber (2014). Pre-1989 hydroelectric and geothermal power generation facilities are connected to major urban centers with 1989 populations above 50,000 using minimum spanning tree algorithms.

**Power generation nodes (pre-1989):**
- Olkaria Geothermal Plant
- Masinga Dam
- Kiambere Dam
- Gitaru Dam
- Kindaruma Dam

**Urban centers (1989 population > 50,000):** From Kenya Population and Housing Census 1989 (see Section 8 below)

**Instruments constructed:**

| Variable | Description |
|----------|-------------|
| `dist_km_euclidean` | Straight-line distance from cluster to nearest network segment (km) — **primary instrument** |
| `dist_km_leastcost` | Least-cost distance accounting for terrain and river crossings (km) — **robustness instrument** |
| `log_dist_euclidean` | Natural log of Euclidean distance + 1 |

---

## 7. Elevation and Slope (Geographic Controls)

**Source:** NASA Shuttle Radar Topography Mission (SRTM), 30m resolution  
**Access:** Downloaded via `elevatr` R package (AWS terrain tiles) or directly from https://earthdata.nasa.gov  
**Place files in:** `Data/Raw/Spatial/` (if pre-downloaded) or downloaded automatically by script

| Variable | Description |
|----------|-------------|
| `elevation_m` | Mean elevation within cluster buffer (meters) |
| `slope_degrees` | Mean slope within cluster buffer (degrees) |

---

## 8. Roads (Geographic Control)

**Source:** OpenStreetMap via Geofabrik  
**Download:** https://download.geofabrik.de/africa/kenya.html (Kenya extract, 2024)  
**Place file in:** `Data/Raw/Spatial/`

Road types included: motorway, trunk, primary, secondary

| Variable | Description |
|----------|-------------|
| `dist_road_km` | Distance from cluster to nearest major road (km) |

---

## 9. Rivers (Geographic Control)

**Source:** HydroSHEDS / HydroRIVERS v1.0  
**Download:** https://www.hydrosheds.org/products/hydrorivers  
**Filter:** Strahler order ≥ 4 (major rivers only)  
**Place file in:** `Data/Raw/Spatial/`

| Variable | Description |
|----------|-------------|
| `dist_river_km` | Distance from cluster to nearest major river (km) |

---

## 10. Kenya 1989 Population and Housing Census (Geographic Controls)

**Source:** Kenya National Bureau of Statistics  
**Description:** 1989 urban center locations and populations, used to identify the 8 major cities included in the hypothetical network and to construct distance controls

| Variable | Description |
|----------|-------------|
| `dist_1989_city_km` | Distance from cluster to nearest 1989 urban center (km) |
| `nearest_1989_pop` | Population of nearest 1989 urban center |

---

## Reproducibility Checklist

- [ ] DHS 2014 files in `Data/Raw/DHS/2014/`
- [ ] DHS 2022 files in `Data/Raw/DHS/2022/`
- [ ] DHS GPS files in `Data/Raw/DHS/GPS/`
- [ ] GADM Kenya level-2 shapefile in `Data/Raw/Admin/`
- [ ] Transmission lines shapefile in `Data/Raw/Powerlines/`
- [ ] Road and river shapefiles in `Data/Raw/Spatial/`
- [ ] Open `Final Project.Rproj` before running any script
- [ ] Run scripts in order: `01` → `02` → `03` → `04_a` → `04_b`
