# ============================================================
# Load Packages
# ============================================================
# Load conflicted first and set preferences
library(conflicted)
conflicts_prefer(
  terra::compare,
  dplyr::filter,
  dplyr::lag
)
conflicts_prefer(
  huxtable::set_caption,
  huxtable::add_footnote
)
# Then load the rest
library(tidyverse)
library(haven)
library(sf)
library(terra)
library(here)
library(writexl)
library(fixest)
library(modelsummary)
library(huxtable)
library(officer)
library(skimr)
library(ivreg) #To help extract f-stat for interaction
library(stats)
library(car)
# ============================================================
# ALL REGRESSION TABLES - CONSISTENT FORMATTING
# ============================================================

# Load data
person_data <- readRDS(here("Data", "Clean", "person_analysis_both_instruments.rds"))

# Means for tables
mean_enroll <- mean(person_data$enrolled, na.rm = TRUE)
sd_enroll <- sd(person_data$enrolled, na.rm = TRUE)
mean_edu <- mean(person_data$years_education, na.rm = TRUE)
sd_edu <- sd(person_data$years_education, na.rm = TRUE)
mean_elec <- mean(person_data$has_electricity, na.rm = TRUE)
sd_elec <- sd(person_data$has_electricity, na.rm = TRUE)



#test : You ca either use summarize or summarise 

new_stats<- person_data %>% group_by(county) %>%summarise(mean_edu = mean(enrolled, na.rm=TRUE), std_dev = sd(enrolled, na.rm=TRUE))
view(new_stats)

# ============================================================
# FUNCTION: Create Huxtable with Title
# ============================================================

add_table_title <- function(hux_table, title) {
  # Insert title row
  hux_table <- insert_row(hux_table, title, fill = "", after = 0)
  # Merge title
  hux_table <- merge_cells(hux_table, 1, 1:ncol(hux_table))
  # Format title
  hux_table <- set_bold(hux_table, 1, 1, TRUE)
  hux_table <- set_align(hux_table, 1, 1, "center")
  hux_table <- set_font_size(hux_table, 1, 1, 11)
  
  return(hux_table)
}

add_table_borders <- function(hux_table) {
  # Top border (row 2 after title)
  hux_table <- set_top_border(hux_table, 2, 1:ncol(hux_table), 1.5)
  # Header border
  hux_table <- set_bottom_border(hux_table, 2, 1:ncol(hux_table), 0.5)
  # Bottom border
  last_row <- nrow(hux_table)
  hux_table <- set_bottom_border(hux_table, last_row - 1, 1:ncol(hux_table), 1.5)
  # Font
  hux_table <- set_font_size(hux_table, 10)
  
  return(hux_table)
}

# ============================================================
# TABLE 3: REDUCED FORM
# ============================================================
# ============================================================
# TABLE A5: REDUCED FORM
# ============================================================

# Run regressions - panel specification
rf_enrollment <- feols(
  enrolled ~ log_dist_euclidean + 
    log_dist_euclidean:factor(year) +
    age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

rf_education <- feols(
  years_education ~ log_dist_euclidean + 
    log_dist_euclidean:factor(year) +
    age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

summary(rf_enrollment)
summary(rf_education)

# ------------------------------------------------------------
# Create Table
# ------------------------------------------------------------

rf_hux <- modelsummary(
  list("Enrollment" = rf_enrollment, "Years of Education" = rf_education),
  output = "huxtable",
  coef_map = c(
    "log_dist_euclidean" = "Log Distance to Network - Euclidean",
    "log_dist_euclidean:factor(year)2022" = "Log Distance to Network × 2022"
  ),
  add_rows = tibble::tribble(
    ~term, ~`Enrollment`, ~`Years of Education`,
    "Child Controls", "Yes", "Yes",
    "Geography and Infrastructure Controls", "Yes", "Yes",
    "Counties", "47", "47",
    "County Fixed Effects", "Yes", "Yes",
    "Year Fixed Effects", "Yes", "Yes",
    "Mean of Dependent Variable", as.character(round(mean_enroll, 3)), as.character(round(mean_edu, 3)),
    "SD of Dependent Variable", as.character(round(sd_enroll, 3)), as.character(round(sd_edu, 3))
  ),
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

rf_hux <- as_hux(rf_hux)

# ------------------------------------------------------------
# Notes
# ------------------------------------------------------------

rf_notes <- paste(
  "Notes. Reduced form regressions of educational outcomes on log Euclidean distance to the hypothetical pre-1989 electricity transmission network.",
  "Individual-level regressions for children aged 5–14 using Kenya DHS 2014 and 2022.",
  "Dependent variables: school enrollment (binary, Column 1) and years of education (continuous, Column 2).",
  "Log Distance to Network × 2022 is the interaction of log distance with a 2022 year indicator (2014 as base year).",
  "Child controls include age and gender; geographic controls include elevation, slope, agro-ecological zone indicators, and log distances to roads and rivers.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at the DHS cluster level are reported in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

rf_hux <- add_footnote(rf_hux, rf_notes, border = 0, font_size = 9)

# ------------------------------------------------------------
# Journal-Style Borders
# ------------------------------------------------------------

rf_hux <- set_top_border(rf_hux, 1, 1:ncol(rf_hux), 1)
rf_hux <- set_bottom_border(rf_hux, 1, 1:ncol(rf_hux), 0.5)
last_row <- nrow(rf_hux)
rf_hux <- set_bottom_border(rf_hux, last_row - 1, 1:ncol(rf_hux), 1)
rf_hux <- set_font_size(rf_hux, 10)

# ------------------------------------------------------------
# Export to Word
# ------------------------------------------------------------

quick_docx(rf_hux, file = here("Tables", "A5_reduced_form.docx"), open = FALSE)

cat("\n✅ Table A5 saved\n")

#FIRST STAGE - PANEL SPECIFICATION

# Column 1: Baseline - no controls
fs_baseline <- feols(
  has_electricity ~ log_dist_euclidean + 
    log_dist_euclidean:factor(year) | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

# Column 2: Add child controls
fs_baseline_child_ctrls <- feols(
  has_electricity ~ log_dist_euclidean + 
    log_dist_euclidean:factor(year) +
    age + female | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

# Column 3: Full specification
fs_full <- feols(
  has_electricity ~ log_dist_euclidean + 
    log_dist_euclidean:factor(year) +
    age + female + elevation_m + slope_degrees + 
    i(aez) + log_dist_road + log_dist_river | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

# ============================================================
# TABLE 2: FIRST STAGE (Journal Style, DOCX Only)
# ============================================================

n_counties <- 47

# ------------------------------------------------------------
# 1. Numbered Model List
# ------------------------------------------------------------

fs_models <- list(
  "(1)" = fs_baseline,
  "(2)" = fs_baseline_child_ctrls,
  "(3)" = fs_full
)

# ------------------------------------------------------------
# 2. Variable Labels
# ------------------------------------------------------------

coef_map_fs <- c(
  "log_dist_euclidean" = "Log(Distance Euclidean)",
  "log_dist_euclidean:factor(year)2022" = "Log(Distance Euclidean) × 2022"
)

# ------------------------------------------------------------
# 3. Compute First-Stage F-Statistics
# # Minimum of baseline and interaction F-stats
# ------------------------------------------------------------

# Column 1
f1_base <- (coef(fs_baseline)["log_dist_euclidean"] /
              se(fs_baseline)["log_dist_euclidean"])^2
f1_inter <- (coef(fs_baseline)["log_dist_euclidean:factor(year)2022"] /
               se(fs_baseline)["log_dist_euclidean:factor(year)2022"])^2
f1 <- min(f1_base, f1_inter)

# Column 2
f2_base <- (coef(fs_baseline_child_ctrls)["log_dist_euclidean"] /
              se(fs_baseline_child_ctrls)["log_dist_euclidean"])^2
f2_inter <- (coef(fs_baseline_child_ctrls)["log_dist_euclidean:factor(year)2022"] /
               se(fs_baseline_child_ctrls)["log_dist_euclidean:factor(year)2022"])^2
f2 <- min(f2_base, f2_inter)

# Column 3
f3_base <- (coef(fs_full)["log_dist_euclidean"] /
              se(fs_full)["log_dist_euclidean"])^2
f3_inter <- (coef(fs_full)["log_dist_euclidean:factor(year)2022"] /
               se(fs_full)["log_dist_euclidean:factor(year)2022"])^2
f3 <- min(f3_base, f3_inter)

cat("F-stats (minimum):", round(f1,2), round(f2,2), round(f3,2), "\n")

# ------------------------------------------------------------
# 4. Extra Rows
# ------------------------------------------------------------

extra_rows_fs <- tibble::tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Child Controls", "No", "Yes", "Yes",
  "Geography and Infrastructure Controls", "No", "No", "Yes",
  "Counties", as.character(n_counties), as.character(n_counties), as.character(n_counties),
  "County Fixed Effects", "Yes", "Yes", "Yes",
  "Year Fixed Effects", "Yes", "Yes", "Yes",
  "Mean of Dependent Variable", as.character(round(mean_elec, 3)), as.character(round(mean_elec, 3)), as.character(round(mean_elec, 3)),
  "SD of Dependent Variable", as.character(round(sd_elec, 3)), as.character(round(sd_elec, 3)), as.character(round(sd_elec, 3)),
  "First-Stage F-Statistic (minimum)", as.character(round(f1, 2)), as.character(round(f2, 2)), as.character(round(f3, 2))
)

# ------------------------------------------------------------
# 5. Create Huxtable
# ------------------------------------------------------------

fs_hux <- modelsummary(
  fs_models,
  output = "huxtable",
  coef_map = coef_map_fs,
  add_rows = extra_rows_fs,
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# 6. Notes
# ------------------------------------------------------------

fs_notes <- paste(
  "Notes. Individual-level regressions for children aged 5-14 in Kenya, using DHS data from 2014 and 2022.",
  "Dependent variable: Household has electricity (binary indicator).",
  "Log(Distance Euclidean) is the natural logarithm of Euclidean distance to the hypothetical pre-1989 electricity transmission network.",
  "Log(Distance Euclidean) × 2022 is the interaction of log distance with a 2022 year indicator, with 2014 as the base year.",
  "The baseline coefficient captures the cross-sectional relationship in 2014, while the interaction term captures the additional differential effect of network proximity during the LMCP expansion period.",
  "The reported first-stage F-statistic is the minimum across both instruments; the lower value corresponds to the 2022 interaction term (F = 20.72 in the preferred specification), reflecting the more demanding test of differential expansion.",
  "Child controls include age and gender (female indicator).",
  "Geographic controls include elevation (meters), slope (degrees), agro-ecological zone indicators, log distance to major roads, and log distance to major rivers.",
  "Progressive controls: (1) County FE + Year FE; (2) adds child controls; (3) adds geographic and infrastructure controls.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at the DHS cluster level are shown in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

fs_hux <- fs_hux %>%
  set_caption("Table 2: First Stage - Log Distance to Network → Household Electricity") %>%
  add_footnote(fs_notes, border = 0)

# ------------------------------------------------------------
# 7. Journal-Style Borders
# ------------------------------------------------------------

fs_hux <- set_top_border(fs_hux, 1, 1:ncol(fs_hux), 1)
fs_hux <- set_bottom_border(fs_hux, 1, 1:ncol(fs_hux), 0.5)
last_row <- nrow(fs_hux)
fs_hux <- set_bottom_border(fs_hux, last_row - 1, 1:ncol(fs_hux), 1)
fs_hux <- set_font_size(fs_hux, 10)

# ------------------------------------------------------------
# 8. Export to Word
# ------------------------------------------------------------

quick_docx(fs_hux, file = here("Tables", "02_first_stage_march302026.docx"), open = FALSE)

# ------------------------------------------------------------
# SECOND STAGE - PANEL SPECIFICATION
# ------------------------------------------------------------

iv_enrollment_panel <- feols(
  enrolled ~ age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | 
    county_id + year | 
    has_electricity ~ log_dist_euclidean + log_dist_euclidean:factor(year),
  data = person_data,
  vcov = ~cluster_id_unique
)

iv_education_panel <- feols(
  years_education ~ age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | 
    county_id + year | 
    has_electricity ~ log_dist_euclidean + log_dist_euclidean:factor(year),
  data = person_data,
  vcov = ~cluster_id_unique
)

summary(iv_enrollment_panel)
summary(iv_education_panel)

# ------------------------------------------------------------
# Collect model results
# ------------------------------------------------------------

iv_models <- list(
  "Enrollment" = iv_enrollment_panel,
  "Years of Education" = iv_education_panel
)

# ------------------------------------------------------------
# Variable Labels
# ------------------------------------------------------------

coef_map_iv <- c("fit_has_electricity" = "Household has Electricity (IV)")

# ------------------------------------------------------------
# Extra Rows
# ------------------------------------------------------------

extra_rows_iv <- tibble::tribble(
  ~term, ~`Enrollment`, ~`Years of Education`,
  "Child Controls", "Yes", "Yes",
  "Geography and Infrastructure Controls", "Yes", "Yes",
  "Counties", as.character(n_counties), as.character(n_counties),
  "County Fixed Effects", "Yes", "Yes",
  "Year Fixed Effects", "Yes", "Yes",
  "Mean of Dependent Variable", as.character(round(mean_enroll, 3)), as.character(round(mean_edu, 3)),
  "SD of Dependent Variable", as.character(round(sd_enroll, 3)), as.character(round(sd_edu, 3)),
  "First-Stage F-Statistic", as.character(round(f3, 2)), as.character(round(f3, 2))
)

# ------------------------------------------------------------
# Create Huxtable
# ------------------------------------------------------------

iv_hux <- modelsummary(
  iv_models,
  output = "huxtable",
  coef_map = coef_map_iv,
  add_rows = extra_rows_iv,
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# Notes
# ------------------------------------------------------------

iv_notes <- paste(
  "Notes. Individual-level regressions for children aged 5–14 using Kenya DHS 2014 and 2022.",
  "Dependent variables: school enrollment (binary) in Column 1 and years of education (continuous) in Column 2.",
  "Instruments: log Euclidean distance (km) from DHS cluster to nearest segment of the hypothetical pre-1989 electricity transmission network, and its interaction with a 2022 year indicator (2014 as base year).",
  "The panel instrument exploits differential electrification expansion under the LMCP, whereby clusters closer to the historical network experienced disproportionately larger increases in electrification between 2014 and 2022.",
  "The reported first-stage F-statistic (20.72) is the minimum across both instruments.",
  "Child controls include age and gender; geographic and infrastructure controls include elevation, slope, agro-ecological zone indicators, and log distances to roads and rivers.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at the DHS cluster level are reported in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

iv_hux <- iv_hux %>%
  set_caption("Table 3: Second Stage — IV Estimates") %>%
  add_footnote(iv_notes, border = 0)

# ------------------------------------------------------------
# Journal-Style Borders
# ------------------------------------------------------------

iv_hux <- set_top_border(iv_hux, 1, 1:ncol(iv_hux), 1)
iv_hux <- set_bottom_border(iv_hux, 1, 1:ncol(iv_hux), 0.5)
last_row <- nrow(iv_hux)
iv_hux <- set_bottom_border(iv_hux, last_row - 1, 1:ncol(iv_hux), 1)
iv_hux <- set_font_size(iv_hux, 10)

# ------------------------------------------------------------
# Export to Word
# ------------------------------------------------------------

quick_docx(iv_hux, file = here("Tables", "03_second_stage_march302026.docx"), open = FALSE)
# ============================================================
# TABLE A4: OLS vs IV COMPARISON
# ============================================================

# ------------------------------------------------------------
# OLS Models
# ------------------------------------------------------------

ols_enrollment <- feols(
  enrolled ~ has_electricity + age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

ols_education <- feols(
  years_education ~ has_electricity + age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

# ------------------------------------------------------------
# Coefficient Map
# ------------------------------------------------------------

coef_map_comparison <- c(
  "has_electricity" = "Household has Electricity (OLS)",
  "fit_has_electricity" = "Household has Electricity (IV)"
)

# ------------------------------------------------------------
# Extra Rows
# ------------------------------------------------------------

extra_rows_comparison <- tibble::tribble(
  ~term, ~`Enrollment (OLS)`, ~`Enrollment (IV)`, ~`Years Ed (OLS)`, ~`Years Ed (IV)`,
  "Child Controls", "Yes", "Yes", "Yes", "Yes",
  "Geography and Infrastructure Controls", "Yes", "Yes", "Yes", "Yes",
  "Counties", "47", "47", "47", "47",
  "County Fixed Effects", "Yes", "Yes", "Yes", "Yes",
  "Year Fixed Effects", "Yes", "Yes", "Yes", "Yes",
  "Mean of Dependent Variable",
  as.character(round(mean_enroll, 3)),
  as.character(round(mean_enroll, 3)),
  as.character(round(mean_edu, 3)),
  as.character(round(mean_edu, 3)),
  "SD of Dependent Variable",
  as.character(round(sd_enroll, 3)),
  as.character(round(sd_enroll, 3)),
  as.character(round(sd_edu, 3)),
  as.character(round(sd_edu, 3)),
  "First-Stage F-Statistic", "", as.character(round(f3, 1)), "", as.character(round(f3, 1))
)

# ------------------------------------------------------------
# Model List
# ------------------------------------------------------------

comparison_models <- list(
  "Enrollment (OLS)" = ols_enrollment,
  "Enrollment (IV)" = iv_enrollment_panel,
  "Years Ed (OLS)" = ols_education,
  "Years Ed (IV)" = iv_education_panel
)

# ------------------------------------------------------------
# Create Huxtable
# ------------------------------------------------------------

comparison_hux <- modelsummary(
  comparison_models,
  output = "huxtable",
  coef_map = coef_map_comparison,
  add_rows = extra_rows_comparison,
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# Notes
# ------------------------------------------------------------

comparison_notes <- paste(
  "Notes. Individual-level regressions for children aged 5–14 using Kenya DHS 2014 and 2022.",
  "Dependent variables: school enrollment (binary, Columns 1–2) and years of education (continuous, Columns 3–4).",
  "OLS columns show standard regressions of outcomes on household electricity access.",
  "IV columns instrument household electricity with log Euclidean distance and its interaction with a 2022 year indicator (2014 as base year), exploiting differential electrification expansion under the LMCP.",
  "The reported first-stage F-statistic is the minimum across both instruments.",
  "Child controls include age and gender; geographic and infrastructure controls include elevation, slope, agro-ecological zone indicators, and log distances to roads and rivers.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at the DHS cluster level are reported in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

comparison_hux <- comparison_hux %>%
  set_caption("Table A4: Comparing OLS and IV Estimates") %>%
  add_footnote(comparison_notes, border = 0)

# ------------------------------------------------------------
# Journal-Style Borders
# ------------------------------------------------------------

comparison_hux <- set_top_border(comparison_hux, 1, 1:ncol(comparison_hux), 1)
comparison_hux <- set_bottom_border(comparison_hux, 1, 1:ncol(comparison_hux), 0.5)
last_row <- nrow(comparison_hux)
comparison_hux <- set_bottom_border(comparison_hux, last_row - 1, 1:ncol(comparison_hux), 1)
comparison_hux <- set_font_size(comparison_hux, 10)

# ------------------------------------------------------------
# Export to Word
# ------------------------------------------------------------

quick_docx(comparison_hux, file = here("Tables", "A4_ols_iv_comparison.docx"), open = FALSE)

cat("\n✅ Table A4 saved\n")


# ============================================================
# TABLE 4: HETEROGENEITY BY GENDER - PANEL SPECIFICATION
# ============================================================

het_interaction <- feols(
  years_education ~ female + age + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river + i(year) | 
    county_id | 
    has_electricity + has_electricity:female ~ 
    log_dist_euclidean + log_dist_euclidean:female,
  data = person_data,
  vcov = ~cluster_id_unique
)

summary(het_interaction)

# Extract F-statistics
fs_stats <- fitstat(het_interaction, "ivf")
print(fs_stats)

str(fs_stats)
#Extract 
f_elec <- fs_stats$`ivf1::has_electricity`$stat
f_inter <- fs_stats$`ivf1::has_electricity:female`$stat
#Take min
fs_f <- min(f_elec, f_inter)
cat("F (has_electricity):", round(f_elec, 1), "\n")
cat("F (interaction):", round(f_inter, 1), "\n")
cat("First-Stage F (minimum):", round(fs_f, 1), "\n")

cat("First-Stage F (minimum, reported in table):", round(fs_f, 1), "\n")
# Extract key coefficients
# ------------------------------------------------------------

# β₁ = Effect for boys
coef_boys <- coef(het_interaction)["fit_has_electricity"]
se_boys <- se(het_interaction)["fit_has_electricity"]

# β₃ = Interaction (differential effect for girls)
coef_interaction <- coef(het_interaction)["fit_has_electricity:female"]
se_interaction <- se(het_interaction)["fit_has_electricity:female"]

# β₁ + β₃ = Effect for girls
coef_girls <- coef_boys + coef_interaction

# β₂ = Main effect of female
coef_female <- coef(het_interaction)["female"]
se_female <- se(het_interaction)["female"]

# Test significance of interaction
t_stat_interaction <- coef_interaction / se_interaction
p_val_interaction <- 2 * (1 - pnorm(abs(t_stat_interaction)))

p_val_text <- ifelse(p_val_interaction < 0.001, "<0.001", sprintf("%.3f", p_val_interaction))

# ------------------------------------------------------------
# First-stage F-statistic
# ------------------------------------------------------------



cat("Effect for Boys (β₁):", round(coef_boys, 3), "\n")
cat("Effect for Girls (β₁ + β₃):", round(coef_girls, 3), "p =", p_val_text, "\n")
cat("First-Stage F:", round(fs_f, 1), "\n")

# ------------------------------------------------------------
# Test Significance for Boys and Girls Separately
# ------------------------------------------------------------

# Test boys: β₁ = 0 (from t-test)
t_boys <- coef_boys / se_boys
p_boys <- 2 * (1 - pnorm(abs(t_boys)))

# Format p-value for boys
if (p_boys < 0.001) {
  p_boys_text <- "<0.001"
} else {
  p_boys_text <- sprintf("%.3f", p_boys)
}

# Determine stars for boys
if (p_boys < 0.01) {
  stars_boys <- "***"
} else if (p_boys < 0.05) {
  stars_boys <- "**"
} else if (p_boys < 0.10) {
  stars_boys <- "*"
} else {
  stars_boys <- ""
}

# Test girls: β₁ + β₃ = 0 (using linear hypothesis test)
test_girls <- linearHypothesis(
  het_interaction,
  "fit_has_electricity + fit_has_electricity:female = 0"
)

print(test_girls)

# Extract p-value for girls
p_girls <- test_girls$`Pr(>Chisq)`[2]

# Format p-value for girls
if (p_girls < 0.001) {
  p_girls_text <- "<0.001"
} else {
  p_girls_text <- sprintf("%.3f", p_girls)
}

# Determine stars for girls
if (p_girls < 0.01) {
  stars_girls <- "***"
} else if (p_girls < 0.05) {
  stars_girls <- "**"
} else if (p_girls < 0.10) {
  stars_girls <- "*"
} else {
  stars_girls <- ""
}

# Print results
cat("\n=== HETEROGENEITY RESULTS ===\n")
cat("Effect for Boys (β₁):", round(coef_boys, 3), "(SE:", round(se_boys, 3), ") p =", p_boys_text, stars_boys, "\n")
cat("Main Female Effect (β₂):", round(coef_female, 3), "(SE:", round(se_female, 3), ")\n")
cat("Interaction (β₃):", round(coef_interaction, 3), "(SE:", round(se_interaction, 3), ") p =", p_val_text, "\n")
cat("Effect for Girls (β₁ + β₃):", round(coef_girls, 3), "p =", p_girls_text, stars_girls, "\n")
cat("First-Stage F:", round(fs_f, 1), "\n")

# Interpretation
if (p_val_interaction < 0.10) {
  if (coef_interaction > 0) {
    cat("\n✓ Interpretation: Effect is LARGER for girls by", 
        round(coef_interaction, 3), "years (p =", p_val_text, ")\n")
  } else {
    cat("\n✓ Interpretation: Effect is LARGER for boys by", 
        round(abs(coef_interaction), 3), "years (p =", p_val_text, ")\n")
  }
} else {
  cat("\n✓ Interpretation: No significant difference between boys and girls (p =", 
      p_val_text, ")\n")
}

# ------------------------------------------------------------
# Variable Labels
# ------------------------------------------------------------

coef_map_het <- c(
  "fit_has_electricity" = "Household has Electricity (IV)",
  "female" = "Child is Female",
  "fit_has_electricity:female" = "Household has Electricity × Child is Female ")

# ------------------------------------------------------------
# Create Extra Rows with CORRECT Significance
# ------------------------------------------------------------

extra_rows_het <- tibble::tribble(
  ~term, ~`Years of Education`,
  "Child Controls", "Yes",
  "Geography and Infrastructure Controls", "Yes",
  "Counties", as.character(n_counties),
  "County Fixed Effects", "Yes",
  "Year Fixed Effects", "Yes",
  "Mean of Dependent Variable", as.character(round(mean_edu, 3)),
  "SD of Dependent Variable", as.character(round(sd_edu, 3)),
  "First-Stage F-Statistic", as.character(round(fs_f, 1))
)

# ------------------------------------------------------------
# Create Huxtable
# ------------------------------------------------------------

het_hux <- modelsummary(
  list("Years of Education" = het_interaction),
  output = "huxtable",
  coef_map = coef_map_het,
  add_rows = extra_rows_het,
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

# Ensure it's huxtable
het_hux <- as_hux(het_hux)

# ------------------------------------------------------------
# Add Title and Notes
# ------------------------------------------------------------

het_hux <- insert_row(het_hux, 
                      "Table 4: Heterogeneity Analysis:by gender",
                      fill = "", after = 0)

het_hux <- merge_cells(het_hux, 1, 1:ncol(het_hux))
het_hux <- set_bold(het_hux, 1, 1, TRUE)
het_hux <- set_align(het_hux, 1, 1, "center")
het_hux <- set_font_size(het_hux, 1, 1, 11)

het_notes <- paste(
  "Notes. IV regression with interaction term for children aged 5-14 in Kenya, using DHS data from 2014 and 2022.",
  "Dependent variable: Years of education (continuous).",
  "β₁ = Effect for boys (when Female = 0).",
  "β₂ = Baseline gender gap (girls vs boys when not electrified).",
  "β₃ = Interaction term (differential effect of electricity for girls).",
  "Effect for boys = β₁; Effect for girls = β₁ + β₃.",
  "Instrument: Log Euclidean distance to hypothetical 1989 electricity transmission network, interacted with female.",
  "Child controls include age and gender. Geographic and infrastructure controls include elevation, slope, agro-ecological zones, log distance to roads and rivers.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at cluster level in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

het_hux <- add_footnote(het_hux, het_notes, border = 0, font_size = 9)

# ------------------------------------------------------------
# Journal-Style Borders
# ------------------------------------------------------------

het_hux <- set_top_border(het_hux, 2, 1:ncol(het_hux), 1.5)
het_hux <- set_bottom_border(het_hux, 2, 1:ncol(het_hux), 0.5)
last_row <- nrow(het_hux)
het_hux <- set_bottom_border(het_hux, last_row - 1, 1:ncol(het_hux), 1.5)

# ------------------------------------------------------------
# Font Size
# ------------------------------------------------------------
het_hux <- set_font_size(het_hux, 10)

# ------------------------------------------------------------
# Export to Word
# ------------------------------------------------------------
quick_docx(het_hux, file = here("Tables", "05_heterogeneity_gender.docx"),open = FALSE)

#Robustness - exclude major cities, use least cost
# ============================================================
# ROBUSTNESS: EXCLUDE MAJOR CITY COUNTIES (BOARDING SCHOOLS)
# ============================================================

cat("\n=== ROBUSTNESS 1: EXCLUDING MAJOR CITY COUNTIES ===\n")

# Major cities to exclude (boarding school concerns)
#major_cities <- c("Nairobi", "Nakuru", "Mombasa", "Kisumu", 
                 # "Kiambu", "Uasin Gishu", "Machakos", "Nyeri")
major_cities <- c("Nairobi", "Mombasa", "Kisumu", 
                  "Kiambu", "Machakos")
# Create restricted sample
person_data_no_cities <- person_data %>%
  filter(!county %in% major_cities)

cat("Original sample:", nrow(person_data), "\n")
cat("After excluding major cities:", nrow(person_data_no_cities), "\n")
cat("Dropped:", nrow(person_data) - nrow(person_data_no_cities), "observations\n")

# Get summary stats for this sample
mean_elec_nc <- mean(person_data_no_cities$has_electricity, na.rm = TRUE)
mean_enroll_nc <- mean(person_data_no_cities$enrolled, na.rm = TRUE)
mean_edu_nc <- mean(person_data_no_cities$years_education, na.rm = TRUE)
sd_enroll_nc <- sd(person_data_no_cities$enrolled, na.rm = TRUE)
sd_edu_nc <- sd(person_data_no_cities$years_education, na.rm = TRUE)
n_counties_nc <- length(unique(person_data_no_cities$county_id))

# ------------------------------------------------------------
# First Stage - No Major Cities
# ------------------------------------------------------------
fs_no_cities <- feols(
  has_electricity ~ log_dist_euclidean + 
    log_dist_euclidean:factor(year) +
    age + female + 
    elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | county_id + year,
  data = person_data_no_cities,
  vcov = ~cluster_id_unique
)

# F-statistic - minimum across both instruments
f_nc_base <- (coef(fs_no_cities)["log_dist_euclidean"] / 
                se(fs_no_cities)["log_dist_euclidean"])^2
f_nc_inter <- (coef(fs_no_cities)["log_dist_euclidean:factor(year)2022"] / 
                 se(fs_no_cities)["log_dist_euclidean:factor(year)2022"])^2
f_nc <- min(f_nc_base, f_nc_inter)

cat("F-stat (baseline):", round(f_nc_base, 1), "\n")
cat("F-stat (2022 interaction):", round(f_nc_inter, 1), "\n")
cat("Minimum F-statistic:", round(f_nc, 1), "\n")

# ------------------------------------------------------------
# Second Stage - No Major Cities
# ------------------------------------------------------------

iv_enrollment_nc <- feols(
  enrolled ~ age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | 
    county_id + year | 
    has_electricity ~ log_dist_euclidean + log_dist_euclidean:factor(year),
  data = person_data_no_cities,
  vcov = ~cluster_id_unique
)

iv_education_nc <- feols(
  years_education ~ age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | 
    county_id + year | 
    has_electricity ~ log_dist_euclidean + log_dist_euclidean:factor(year),
  data = person_data_no_cities,
  vcov = ~cluster_id_unique
)

# Get F-stats

# ------------------------------------------------------------
# Create Table - No Major Cities
# ------------------------------------------------------------

coef_map_rob <- c(
  "fit_has_electricity" = "Household Has Electricity (IV)"
)

extra_rows_nc <- tibble::tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Child Controls", "Yes", "Yes",
  "Geography and Infrastructure Controls", "Yes", "Yes",
  "Counties", as.character(n_counties_nc), as.character(n_counties_nc),
  "County Fixed Effects", "Yes", "Yes",
  "Year Fixed Effects", "Yes", "Yes",
  "Mean of Dependent Variable", 
  as.character(round(mean_enroll_nc, 3)), 
  as.character(round(mean_edu_nc, 3)),
  "SD of Dependent Variable", 
  as.character(round(sd_enroll_nc, 3)), 
  as.character(round(sd_edu_nc, 3)),
  "First-Stage F-Statistic", 
  as.character(round(f_nc, 1)), 
  as.character(round(f_nc, 1))
)

rob_models_nc <- list(
  "Enrollment" = iv_enrollment_nc,
  "Years of education" = iv_education_nc
)

rob_hux_nc <- modelsummary(
  rob_models_nc,
  output = "huxtable",
  coef_map = coef_map_rob,
  add_rows = extra_rows_nc,
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# Add Caption AND Notes FIRST
# ------------------------------------------------------------

rob_notes_nc <- paste(
  "Notes. Robustness check excluding five major city counties (Nairobi, Mombasa, Kisumu, Kiambu, and Machakos), leaving 42 counties.",
  "Individual-level regressions for children aged 5–14 using Kenya DHS 2014 and 2022.",
  "Dependent variables: school enrollment (binary, Column 1) and years of education (continuous, Column 2).",
  "Instruments: log Euclidean distance to the hypothetical pre-1989 electricity transmission network and its interaction with a 2022 year indicator (2014 as base year).",
  "The reported first-stage F-statistic is the minimum across both instruments.",
  "Child controls include age and gender; geographic controls include elevation, slope, agro-ecological zone indicators, and log distances to roads and rivers.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at the DHS cluster level are reported in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

rob_hux_nc <- rob_hux_nc %>%
  set_caption("Table A2: Robustness - Excluding Major City Counties") %>%
  add_footnote(rob_notes_nc, border = 0)

# ------------------------------------------------------------
# THEN Set Borders (AFTER notes added)
# ------------------------------------------------------------

# Thick top rule
rob_hux_nc <- set_top_border(rob_hux_nc, 1, 1:ncol(rob_hux_nc), 1)

# Thin rule under header
rob_hux_nc <- set_bottom_border(rob_hux_nc, 1, 1:ncol(rob_hux_nc), 0.5)

# Thick bottom rule BEFORE notes (use last_row - 1)
last_row <- nrow(rob_hux_nc)
rob_hux_nc <- set_bottom_border(rob_hux_nc, last_row - 1, 1:ncol(rob_hux_nc), 1)

# Font size
rob_hux_nc <- set_font_size(rob_hux_nc, 10)

# Save
quick_docx(rob_hux_nc, 
           file = here("Tables", "A2_robustness_no_cities.docx"), 
           open = FALSE)

cat("\n✅ Table A2 saved\n")

# ============================================================
# ROBUSTNESS 2: LEAST-COST DISTANCE AS INSTRUMENT
# ============================================================
# ============================================================
# ROBUSTNESS 2: LEAST-COST DISTANCE AS INSTRUMENT
# ============================================================

cat("\n=== ROBUSTNESS 2: LEAST-COST DISTANCE AS INSTRUMENT ===\n")

# ------------------------------------------------------------
# First Stage - Least-Cost
# ------------------------------------------------------------

fs_leastcost <- feols(
  has_electricity ~ log_dist_leastcost + age + female + 
    elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | county_id + year,
  data = person_data,
  vcov = ~cluster_id_unique
)

# F-statistic
f_lc <- (coef(fs_leastcost)["log_dist_leastcost"] / 
           se(fs_leastcost)["log_dist_leastcost"])^2

cat("\nFirst-stage (least-cost):\n")
cat("Coefficient:", round(coef(fs_leastcost)["log_dist_leastcost"], 3), "\n")
cat("F-statistic:", round(f_lc, 1), "\n")

# ------------------------------------------------------------
# Second Stage - Least-Cost
# ------------------------------------------------------------

# Enrollment
iv_enrollment_lc <- feols(
  enrolled ~ age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | 
    county_id + year | 
    has_electricity ~ log_dist_leastcost,
  data = person_data,
  vcov = ~cluster_id_unique
)

# Years of education
iv_education_lc <- feols(
  years_education ~ age + female + elevation_m + slope_degrees + i(aez) + 
    log_dist_road + log_dist_river | 
    county_id + year | 
    has_electricity ~ log_dist_leastcost,
  data = person_data,
  vcov = ~cluster_id_unique
)

# Get F-stats
f_enroll_lc <- fitstat(iv_enrollment_lc, "ivf")$ivf$stat
f_edu_lc <- fitstat(iv_education_lc, "ivf")$ivf$stat

cat("\nSecond-stage (least-cost):\n")
cat("Enrollment effect:", round(coef(iv_enrollment_lc)["fit_has_electricity"], 3), "\n")
cat("Education effect:", round(coef(iv_education_lc)["fit_has_electricity"], 3), "\n")
cat("F-stat:", round(f_edu_lc, 1), "\n")

# ------------------------------------------------------------
# Create Table - Least-Cost
# ------------------------------------------------------------

coef_map_rob <- c(
  "fit_has_electricity" = "Household Has Electricity (IV)"
)

extra_rows_lc <- tibble::tribble(
  ~term, ~`(1)`, ~`(2)`,
  "Child Controls", "Yes", "Yes",
  "Geography and Infrastructure Controls", "Yes", "Yes",
  "Counties", as.character(n_counties), as.character(n_counties),
  "County Fixed Effects", "Yes", "Yes",
  "Year Fixed Effects", "Yes", "Yes",
  "Mean of Dependent Variable", 
  as.character(round(mean_enroll, 3)), 
  as.character(round(mean_edu, 3)),
  "SD of Dependent Variable", 
  as.character(round(sd_enroll, 3)), 
  as.character(round(sd_edu, 3)),
  "First-Stage F-Statistic", 
  as.character(round(f_lc, 1)), 
  as.character(round(f_lc, 1))
)

rob_models_lc <- list(
  "Enrollment" = iv_enrollment_lc,
  "Years of education" = iv_education_lc
)

rob_hux_lc <- modelsummary(
  rob_models_lc,
  output = "huxtable",
  coef_map = coef_map_rob,
  add_rows = extra_rows_lc,
  gof_map = tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})"
)

# ------------------------------------------------------------
# Add Caption and Notes
# ------------------------------------------------------------

rob_notes_lc <- paste(
  "Notes. Robustness check using least-cost distance as alternative instrument.",
  "Individual-level regressions for children aged 5–14 using Kenya DHS 2014 and 2022.",
  "Dependent variables: school enrollment (binary, Column 1) and years of education (continuous, Column 2).",
  "Instrument: log least-cost distance (km) accounting for terrain and river crossings to the hypothetical pre-1989 electricity transmission network.",
  "The year interaction is not applied as the baseline least-cost instrument falls below conventional strength thresholds when interacted with time (minimum F = 9.3); the Euclidean panel specification remains preferred.",
  "Child controls include age and gender; geographic controls include elevation, slope, agro-ecological zone indicators, and log distances to roads and rivers.",
  "All specifications include county and year fixed effects.",
  "Standard errors clustered at the DHS cluster level are reported in parentheses.",
  "*** p<0.01, ** p<0.05, * p<0.10",
  sep = " "
)

rob_hux_lc <- rob_hux_lc %>%
  set_caption("Table A3: Robustness - Least-Cost Distance as Instrument") %>%
  add_footnote(rob_notes_lc, border = 0)

# ------------------------------------------------------------
# Journal-Style Borders
# ------------------------------------------------------------

rob_hux_lc <- set_top_border(rob_hux_lc, 1, 1:ncol(rob_hux_lc), 1)
rob_hux_lc <- set_bottom_border(rob_hux_lc, 1, 1:ncol(rob_hux_lc), 0.5)
last_row <- nrow(rob_hux_lc)
rob_hux_lc <- set_bottom_border(rob_hux_lc, last_row - 1, 1:ncol(rob_hux_lc), 1)
rob_hux_lc <- set_font_size(rob_hux_lc, 10)

# ------------------------------------------------------------
# Export to Word
# ------------------------------------------------------------

quick_docx(rob_hux_lc, 
           file = here("Tables", "A3_robustness_leastcost.docx"), 
           open = FALSE)

cat("\n✅ Table A3 saved\n")