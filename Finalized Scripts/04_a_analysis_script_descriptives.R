
# ============================================================
# DESCRIPTIVE STATISTICS & FIGURES
# ============================================================

library(tidyverse)
library(officer)
library(flextable)
library(here)
library(ggplot2)
library(patchwork)
##install.packages("conflicted")
#detach("package:huxtable", unload = TRUE)
#detach("package:terra", unload = TRUE)
# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------

person_data <- readRDS(here("Data", "Clean", 
                            "person_analysis_both_instruments.rds"))

# ============================================================
# TABLE 1: DESCRIPTIVE STATISTICS (Keep your existing code)
# ============================================================

calc_mean_sd <- function(var) {
  sprintf("%.2f (%.2f)", 
          mean(var, na.rm = TRUE), 
          sd(var, na.rm = TRUE))
}

desc_table <- tibble(
  Variable = c(
    "Panel A: Educational Outcomes",
    "Enrolled in school (1=Yes, 0=No)",
    "Years of education (Years)",
    
    "Panel B: Treatment Variable",
    "Household has electricity (1=Yes, 0=No)",
    
    "Panel C: Instrumental Variables",
    "Distance to hypothetical network - least-cost (km)",
    "Distance to hypothetical network - Euclidean (km)",
    
    "Panel D: Geographic Controls",
    "Elevation (meters)",
    "Slope (degrees)",
    "Distance to major road (km)",
    "Distance to major river (km)",
    "Distance to nearest 1989 city (km)",
    "Population of nearest 1989 city",
    
    "Panel E: Individual Characteristics",
    "Age (years)",
    "Female (1=Yes, 0=No)",
    "Urban residence (1=Yes, 0=No)"
  ),
  
  `Mean (SD)` = c(
    "",
    calc_mean_sd(person_data$enrolled),
    calc_mean_sd(person_data$years_education),
    
    "",
    calc_mean_sd(person_data$has_electricity),
    
    "",
    calc_mean_sd(person_data$dist_km_leastcost),
    calc_mean_sd(person_data$dist_km_euclidean),
    
    "",
    calc_mean_sd(person_data$elevation_m),
    calc_mean_sd(person_data$slope_degrees),
    calc_mean_sd(person_data$dist_road_km),
    calc_mean_sd(person_data$dist_river_km),
    calc_mean_sd(person_data$dist_1989_city_km),
    calc_mean_sd(person_data$nearest_1989_pop),
    
    "",
    calc_mean_sd(person_data$age),
    calc_mean_sd(person_data$female),
    calc_mean_sd(person_data$urban)
  )
)

desc_flex <- desc_table %>%
  flextable() %>%
  set_header_labels(
    Variable = "Variable",
    `Mean (SD)` = "Mean (SD)"
  ) %>%
  bold(part = "header") %>%
  bold(i = c(1, 4, 6, 9, 16), j = 1) %>%
  padding(i = c(2:3, 5, 7:8, 10:15, 17:19), j = 1, padding.left = 15) %>%
  padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
  align(align = "left", j = 1, part = "all") %>%
  align(align = "right", j = 2, part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1), part = "header") %>%
  hline_bottom(border = fp_border(width = 0.5), part = "header") %>%
  hline_bottom(border = fp_border(width = 1), part = "body") %>%
  width(j = 1, width = 4.5) %>%
  width(j = 2, width = 1.8) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  line_spacing(space = 1, part = "all") %>%
  set_table_properties(opts_word = list(repeat_headers = FALSE)) %>%
  add_footer_lines(
    paste(
      "Notes: The sample includes children aged 5–14 from Kenya DHS 2014 and 2022 (N = 90,080). Standard deviations in parentheses. All distance variables measured in kilometers. Population refers to 1989 urban center population from official Kenya census records. Binary variables take values 0 or 1. Instrumental Variables: Distance to hypothetical 1990 electricity network constructed by connecting pre-existing power generation facilities (Olkaria Geothermal, Masinga Dam, Kiambere Dam, Gitaru Dam, Kindaruma Dam) to major urban centers from the 1989 Population Census using minimum spanning tree algorithms. Least-cost distance accounts for terrain and river crossings; Euclidean distance is straight-line. Geographic Controls: Elevation and slope from NASA SRTM (~90m resolution via AWS). Distances to major roads are sourced from OpenStreetMap (Geofabrik Kenya, 2024). Distances to major rivers are sourced from HydroRIVERS v1.0 (Strahler order ≥4). Distances to 1989 cities and populations from Kenya Population and Housing Census 1989. Outcome Variables: School enrollment and years of education from DHS. Treatment variable: Household electricity access from DHS. Individual characteristics: Age is complete years, gender is binary where 1=Female and 0=Male and urban is a binary where 1=urban and 0 otherwise."
    )
  ) %>%
  fontsize(size = 9, part = "footer") %>% 
  align(align = "left", part = "footer") %>%
  padding(padding.top = 3, part = "footer")

save_as_docx(
  desc_flex,
  path = here("Tables", "01_descriptive_statistics.docx")
)


# ============================================================
# FIGURE 1: OUTCOMES BY ELECTRIFICATION STATUS (FOR PRESENTATION)
# ============================================================

# Calculate summary statistics
ed_summary <- person_data %>%
  mutate(
    elec_status = ifelse(has_electricity == 1, "Electrified", "Not Electrified"),
    year_label = as.factor(year)
  ) %>%
  group_by(year_label, elec_status) %>%
  summarise(
    mean_years = mean(years_education, na.rm = TRUE),
    se_years = sd(years_education, na.rm = TRUE) / sqrt(n()),
    mean_enroll = mean(enrolled, na.rm = TRUE),
    se_enroll = sd(enrolled, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# View the data
cat("\n=== Summary Statistics ===\n")
print(ed_summary)

sum_all_yr_obs<- ed_summary %>% summarise(all=sum(n))

#View
view(sum_all_yr_obs) #90080

#Drop NA is bar graph table
# Calculate summary statistics
ed_summary<-ed_summary %>% na.omit()

view(ed_summary)

#Check total
sum_all_yr_obs_no_na<- ed_summary %>% summarise(all=sum(n))

#View
view(sum_all_yr_obs_no_na) #90059

# Panel A: Years of Education
panel_a <- ggplot(ed_summary, 
                  aes(x = year_label, y = mean_years, fill = elec_status)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.45) +
  scale_fill_manual(
    values = c("Electrified" = "#00BFC4", "Not Electrified" = "#D95F02"),
    name = "Household Status"
  ) +
  labs(
    title = "Panel A: Years of Education",
    x = "Survey Year",
    y = "Mean Years of Education"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "plain", size = 12),
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, 4)

# ------------------------------------------------------------
# Panel B: School Enrollment Rate
# ------------------------------------------------------------

panel_b <- ggplot(ed_summary, 
                  aes(x = year_label, y = mean_enroll, fill = elec_status)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.45) +
  scale_fill_manual(
    values = c("Electrified" = "#00BFC4", "Not Electrified" = "#D95F02"),
    name = "Household Status"
  ) +
  labs(
    title = "Panel B: School Enrollment Rate",
    x = "Survey Year",
    y = "Mean Enrollment Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12),
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, 1.0)

# ------------------------------------------------------------
# Combine and save
# ------------------------------------------------------------

combined_plot <- panel_a + panel_b + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  filename = here("Figures", "02_outcomes_by_electrification.png"),
  plot = combined_plot,
  width = 8,
  height = 4,
  dpi = 300
)

# ============================================================
# FIGURE 2: ELECTRIFICATION BY DISTANCE (OPTIONAL - BAR VERSION)
# ============================================================

# CREATE DISTANCE BINS FIRST
person_data_with_bins <- person_data %>%
  mutate(
    # Create 10km bins for distance
    dist_bin = cut(
      dist_km_leastcost,
      breaks = c(0, 5, 10,15, 20,25, 30,35, 40,45, 50,55, 60,65, 70,75, 80,85, 90,95, 100,105, 110,115, 120,125, 130,135, 140, Inf),
      labels = c("0-5","5-10","10-15","15-20","20-25","25-30",
                 "30-35","35-40","40-45","45-50","50-55","55-60",
                 "60-65","65-70","70-75","75-80","80-85","85-90",
                 "90-95","95-100","100-105","105-110","110-115",
                 "115-120","120-125","125-130","130-135","135-140","140+"),
      right = FALSE
    )
  )

# Calculate summary by distance bin
elec_summary <- person_data_with_bins %>%
  filter(!is.na(dist_bin)) %>%
  group_by(dist_bin, year) %>%
  summarise(
    mean_elec = mean(has_electricity, na.rm = TRUE),
    se_elec = sd(has_electricity, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Create bar plot
elec_plot <- ggplot(elec_summary, 
                    aes(x = dist_bin, y = mean_elec, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_elec - 1.96*se_elec, 
        ymax = mean_elec + 1.96*se_elec),
    width = 0.2, 
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(
    values = c("2014" = "#D95F02", "2022" = "#00BFC4"),
    name = "Survey Year"
  ) +
  labs(
    title = "",
    x = "Distance to pre-1989 Network (km)",
    y = "Mean Household Electrification Rate") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 10))
  )

ggsave(filename = here("Figures", "03_electrification_by_distance.png"),plot = elec_plot,width = 10,height = 6,dpi = 300)

#Electrification by year

person_data %>%
  group_by(year) %>%
  summarise(elec_rate = mean(has_electricity, na.rm = TRUE) * 100)


#Balance table for Exclusion

# ============================================================
# TABLE A1: BALANCE TABLE - NEAR VS FAR FROM PRE-1989 NETWORK
# ============================================================


# ------------------------------------------------------------
# Define near/far using median Euclidean distance
# (cluster-level, so collapse to unique clusters first)
# ------------------------------------------------------------
# ------------------------------------------------------------
# Define near/far using median Euclidean distance (cluster-level)
# ------------------------------------------------------------
cluster_data <- cluster_data %>%
  mutate(
    elevation_km = elevation_m / 1000,             # Elevation in km
    nearest_1989_pop_thousands = nearest_1989_pop / 1000  # Population in thousands
  )

# Update balance_vars and labels for table
balance_vars <- c(
  "elevation_km",
  "slope_degrees",
  "dist_road_km",
  "dist_river_km",
  "dist_1989_city_km",
  "nearest_1989_pop_thousands"
)

balance_labels <- c(
  "Elevation (km)",
  "Slope (degrees)",
  "Distance to major road (km)",
  "Distance to major river (km)",
  "Distance to nearest 1989 city (km)",
  "Population of nearest 1989 city (thousands)"
)

# ------------------------------------------------------------
# Compute means, SDs, differences, and t-statistics
# ------------------------------------------------------------
balance_results <- map2_dfr(balance_vars, balance_labels, function(var, label) {
  
  near <- cluster_data %>% filter(near_network == 1) %>% pull(!!sym(var))
  far  <- cluster_data %>% filter(near_network == 0) %>% pull(!!sym(var))
  
  mean_near <- mean(near, na.rm = TRUE)
  sd_near   <- sd(near, na.rm = TRUE)
  
  mean_far  <- mean(far, na.rm = TRUE)
  sd_far    <- sd(far, na.rm = TRUE)
  
  diff      <- mean_near - mean_far
  t_test    <- t.test(near, far)
  t_stat    <- t_test$statistic
  p_val     <- t_test$p.value
  
  stars <- case_when(
    p_val < 0.01 ~ "***",
    p_val < 0.05 ~ "**",
    p_val < 0.10 ~ "*",
    TRUE         ~ ""
  )
  
  tibble(
    Variable           = label,
    `Near Network\nMean (SD)` = sprintf("%.2f (%.2f)", mean_near, sd_near),
    `Far Network\nMean (SD)`  = sprintf("%.2f (%.2f)", mean_far, sd_far),
    `Difference`        = sprintf("%.2f%s", diff, stars),
    `t-Statistic`       = sprintf("%.2f", t_stat)
  )
})

# ------------------------------------------------------------
# Add sample size row
# ------------------------------------------------------------
n_near <- cluster_data %>% filter(near_network == 1) %>% nrow()
n_far  <- cluster_data %>% filter(near_network == 0) %>% nrow()

n_row <- tibble(
  Variable               = "Observations",
  `Near Network\nMean (SD)` = as.character(n_near),
  `Far Network\nMean (SD)`  = as.character(n_far),
  `Difference`           = "",
  `t-Statistic`          = ""
)

balance_results <- bind_rows(balance_results, n_row)

# View results
balance_results

# ------------------------------------------------------------
# Build flextable
# ------------------------------------------------------------

balance_flex <- balance_results %>%
  flextable() %>%
  set_header_labels(
    Variable             = "Variable",
    `Near Network\nMean` = "Near Network\nMean",
    `Far Network\nMean`  = "Far Network\nMean",
    `Difference`         = "Difference",
    `t-Statistic`        = "t-Statistic"
  ) %>%
  bold(part = "header") %>%
  align(align = "left",   j = 1, part = "all") %>%
  align(align = "center", j = 2:5, part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1), part = "header") %>%
  hline_bottom(border = fp_border(width = 0.5), part = "header") %>%
  hline(i = nrow(balance_results) - 1,
        border = fp_border(width = 0.5, style = "solid"), part = "body") %>%
  hline_bottom(border = fp_border(width = 1), part = "body") %>%
  width(j = 1, width = 3.2) %>%
  width(j = 2:5, width = 1.3) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  line_spacing(space = 1, part = "all") %>%
  padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
  add_footer_lines(
    paste(
      "Notes: Balance test comparing cluster-level predetermined characteristics",
      "for clusters near (at or below median Euclidean distance) versus far (above median)",
      "from the hypothetical pre-1989 electricity transmission network.",
      "Near and far are defined relative to the sample median Euclidean distance",
      "of", round(median(cluster_data$dist_km_euclidean, na.rm = TRUE), 1), "km.",
      "Each observation is a unique cluster-year. Geographic controls are time-invariant",
      "and assigned at the cluster level. Difference = Near minus Far.",
      "t-statistics from two-sample t-tests with unequal variances.",
      "*** p<0.01, ** p<0.05, * p<0.10."
    )
  ) %>%
  fontsize(size = 9, part = "footer") %>%
  align(align = "left", part = "footer") %>%
  padding(padding.top = 3, part = "footer")

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

save_as_docx(
  balance_flex,
  path = here("Tables", "A1_balance_table.docx")
)

# ------------------------------------------------------------
# Quick console check
# ------------------------------------------------------------

cat("\n=== Balance Table ===\n")
print(balance_results)
cat("\nMedian Euclidean distance cutoff:",
    round(median(cluster_data$dist_km_euclidean, na.rm = TRUE), 1), "km\n")
cat("Near clusters:", n_near, "| Far clusters:", n_far, "\n")
