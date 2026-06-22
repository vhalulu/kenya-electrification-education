library(tidyverse)
library(readxl)
library(broom)
library(patchwork)

file <- "raw_data.xlsx"  # ← update to your filename

# ── 1. Load all sheets (Total rows only) ────────────────────────────────────
load_sheet <- function(sheet) {
  df <- read_excel(file, sheet = sheet)
  df$industry_code <- as.character(df$industry_code)
  df %>%
    filter(industry_code == "T") %>%
    pivot_longer(
      cols      = -c(prefecture_code, prefecture, industry_code, industry),
      names_to  = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.integer(year))
}

rv <- load_sheet("Real_value_added_(RV)")
kt <- load_sheet("Net_capital_stock_(KT)")
mh <- load_sheet("Man-hours_(MH)")
wl <- load_sheet("Labor_costs_(WL)")
nv <- load_sheet("Nominal_value_added_(NV)")

# ── 2. Create Japan totals by summing all prefectures ────────────────────────
make_japan <- function(df) {
  df %>%
    group_by(year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(prefecture = "Japan")
}

rv_jp <- make_japan(rv)
kt_jp <- make_japan(kt)
mh_jp <- make_japan(mh)
wl_jp <- make_japan(wl)
nv_jp <- make_japan(nv)

# ── 3. Stack Tokyo, Fukuoka, Japan — dplyr::select to avoid namespace conflict
stack_var <- function(df, jp_df, varname) {
  df %>%
    filter(prefecture %in% c("Tokyo", "Fukuoka")) %>%
    dplyr::select(prefecture, year, value) %>%
    bind_rows(jp_df %>% dplyr::select(prefecture, year, value)) %>%
    rename(!!varname := value)
}

rv_all <- stack_var(rv, rv_jp, "RV")
kt_all <- stack_var(kt, kt_jp, "KT")
mh_all <- stack_var(mh, mh_jp, "MH")
wl_all <- stack_var(wl, wl_jp, "WL")
nv_all <- stack_var(nv, nv_jp, "NV")

# ── 4. Compute alpha (fixed at 2010) ─────────────────────────────────────────
alpha_df <- wl_all %>%
  inner_join(nv_all, by = c("prefecture", "year")) %>%
  filter(year == 2010) %>%
  mutate(alpha = 1 - (WL / NV)) %>%
  dplyr::select(prefecture, alpha)

print(alpha_df)  # sanity check — Tokyo ~0.37, Fukuoka ~0.34

# ── 5. Merge variables and compute annual growth rates ───────────────────────
levels_all <- rv_all %>%
  inner_join(kt_all, by = c("prefecture", "year")) %>%
  inner_join(mh_all, by = c("prefecture", "year"))

growth_all <- levels_all %>%
  arrange(prefecture, year) %>%
  group_by(prefecture) %>%
  mutate(
    gY = (RV / lag(RV)) - 1,
    gK = (KT / lag(KT)) - 1,
    gL = (MH / lag(MH)) - 1
  ) %>%
  ungroup() %>%
  filter(year >= 1971)

# ── 6. Back out annual gA ────────────────────────────────────────────────────
growth_all <- growth_all %>%
  inner_join(alpha_df, by = "prefecture") %>%
  mutate(gA = gY - alpha * gK - (1 - alpha) * gL)

# ── 7. Cumulate to log TFP levels (base = 0 in 1970) ────────────────────────
base_1970 <- tibble(
  prefecture = c("Tokyo", "Fukuoka", "Japan"),
  year       = 1970,
  gA         = 0,
  log_tfp    = 0
)

tfp_levels <- growth_all %>%
  dplyr::select(prefecture, year, gA) %>%
  arrange(prefecture, year) %>%
  group_by(prefecture) %>%
  mutate(log_tfp = cumsum(gA)) %>%
  ungroup() %>%
  bind_rows(base_1970 %>% dplyr::select(prefecture, year, gA, log_tfp)) %>%
  arrange(prefecture, year) %>%
  mutate(prefecture = factor(prefecture,
                             levels = c("Fukuoka", "Tokyo", "Japan")))

print(tfp_levels %>% filter(year %in% c(1970, 1992, 2012)))

# ── 8. Plot Figure 1 — Log TFP ───────────────────────────────────────────────
ggplot(tfp_levels, aes(x = year, y = log_tfp,
                       color = prefecture, linetype = prefecture)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(
    "Tokyo"   = "#185FA5",
    "Fukuoka" = "#BA7517",
    "Japan"   = "#1D9E75"
  )) +
  scale_linetype_manual(values = c(
    "Tokyo"   = "solid",
    "Fukuoka" = "dashed",
    "Japan"   = "dotted"
  )) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) +
  geom_vline(xintercept = 1992, linetype = "longdash",
             color = "grey60", linewidth = 0.4) +
  annotate("text", x = 1993, y = min(tfp_levels$log_tfp, na.rm = TRUE),
           label = "1992", color = "grey50", size = 3, hjust = 0) +
  labs(
    title   = "Log TFP over time — Fukuoka, Tokyo and Japan",
    x       = "Year",
    y       = "Log TFP (1970 = 0)",
    color   = NULL,
    linetype = NULL,
    caption = "Source: RIETI R-JIP 2017. TFP backed out as residual: gA = gY − α·gK − (1−α)·gL.\nAlpha computed from 2010 labor cost / nominal value added. Total industries."
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.caption       = element_text(color = "grey50", size = 8, hjust = 0),
    plot.title         = element_text(face = "bold", size = 13)
  )

ggsave("Figures/figure1_log_tfp.png",
       width = 7, height = 4.5, dpi = 300, bg = "white")
message("Done — saved as figure1_log_tfp.png")

# ════════════════════════════════════════════════════════════════════════════
# DECOMPOSITION SETUP — Load RV and MH with all industry rows
# ════════════════════════════════════════════════════════════════════════════
years <- as.character(1970:2012)

rv_full <- read_excel(file, sheet = "Real_value_added_(RV)")
rv_full$industry_code <- as.character(rv_full$industry_code)
rv_full <- rv_full %>%
  mutate(sector = case_when(
    industry_code == "1"                   ~ "Agriculture",
    industry_code %in% as.character(2:16)  ~ "Industry",
    industry_code %in% as.character(17:23) ~ "Services",
    industry_code == "T"                   ~ "Total",
    TRUE                                   ~ NA_character_
  )) %>%
  filter(!is.na(sector))

mh_full <- read_excel(file, sheet = "Man-hours_(MH)")
mh_full$industry_code <- as.character(mh_full$industry_code)
mh_full <- mh_full %>%
  mutate(sector = case_when(
    industry_code == "1"                   ~ "Agriculture",
    industry_code %in% as.character(2:16)  ~ "Industry",
    industry_code %in% as.character(17:23) ~ "Services",
    industry_code == "T"                   ~ "Total",
    TRUE                                   ~ NA_character_
  )) %>%
  filter(!is.na(sector))

# ── Japan aggregates for decomposition ───────────────────────────────────────
rv_japan_decomp <- rv_full %>%
  group_by(industry_code, industry, sector) %>%
  summarise(across(all_of(years), ~ sum(as.numeric(.), na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(prefecture_label = "Japan")

mh_japan_decomp <- mh_full %>%
  group_by(industry_code, industry, sector) %>%
  summarise(across(all_of(years), ~ sum(as.numeric(.), na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(prefecture_label = "Japan")

# ════════════════════════════════════════════════════════════════════════════
# DECOMPOSITION — Period 2 (AAGR, 1992–2012)
# ════════════════════════════════════════════════════════════════════════════
n <- 20

rv_decomp <- rv_full %>%
  filter(prefecture_code %in% c(13, 40)) %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, sector, all_of(years)) %>%
  bind_rows(rv_japan_decomp %>% dplyr::select(prefecture_label, sector, all_of(years))) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "RV") %>%
  mutate(year = as.integer(year), RV = as.numeric(RV)) %>%
  filter(year %in% c(1992, 2012)) %>%
  group_by(prefecture_label, sector, year) %>%
  summarise(RV = sum(RV, na.rm = TRUE), .groups = "drop")

mh_decomp <- mh_full %>%
  filter(prefecture_code %in% c(13, 40)) %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, sector, all_of(years)) %>%
  bind_rows(mh_japan_decomp %>% dplyr::select(prefecture_label, sector, all_of(years))) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "MH") %>%
  mutate(year = as.integer(year), MH = as.numeric(MH)) %>%
  filter(year %in% c(1992, 2012), sector != "Total") %>%
  group_by(prefecture_label, sector, year) %>%
  summarise(MH = sum(MH, na.rm = TRUE), .groups = "drop")

mh_total <- mh_decomp %>%
  group_by(prefecture_label, year) %>%
  summarise(MH_total = sum(MH, na.rm = TRUE), .groups = "drop")

decomp_base <- rv_decomp %>%
  filter(sector != "Total") %>%
  inner_join(mh_decomp, by = c("prefecture_label", "sector", "year")) %>%
  inner_join(mh_total, by = c("prefecture_label", "year")) %>%
  mutate(x = RV / MH, s = MH / MH_total) %>%
  dplyr::select(prefecture_label, sector, year, x, s)

decomp_wide <- decomp_base %>%
  pivot_wider(names_from = year, values_from = c(x, s)) %>%
  mutate(delta_x = x_2012 - x_1992, delta_s = s_2012 - s_1992)

decomp_results <- decomp_wide %>%
  group_by(prefecture_label) %>%
  summarise(
    Within      = sum(s_1992 * delta_x),
    Across      = sum(x_1992 * delta_s),
    Interaction = sum(delta_s * delta_x),
    Total_X     = sum(s_1992 * delta_x) +
      sum(x_1992 * delta_s) +
      sum(delta_s * delta_x),
    .groups = "drop"
  ) %>%
  left_join(
    decomp_base %>%
      filter(year == 1992) %>%
      group_by(prefecture_label) %>%
      summarise(X_1992 = sum(s * x), .groups = "drop"),
    by = "prefecture_label"
  ) %>%
  mutate(
    Within_aagr      = 100 * Within      / (X_1992 * n),
    Across_aagr      = 100 * Across      / (X_1992 * n),
    Interaction_aagr = 100 * Interaction / (X_1992 * n),
    Total_aagr       = 100 * Total_X     / (X_1992 * n)
  )

print(decomp_results %>%
        dplyr::select(prefecture_label, Within_aagr, Across_aagr,
                      Interaction_aagr, Total_aagr))

decomp_plot <- decomp_results %>%
  dplyr::select(prefecture_label, Within_aagr, Across_aagr,
                Interaction_aagr, Total_aagr) %>%
  pivot_longer(cols = -prefecture_label,
               names_to = "component", values_to = "value") %>%
  mutate(
    component = recode(component,
                       "Within_aagr"      = "Within",
                       "Across_aagr"      = "Across",
                       "Interaction_aagr" = "Interaction",
                       "Total_aagr"       = "Total"),
    component = factor(component,
                       levels = c("Total", "Within", "Across", "Interaction")),
    prefecture_label = factor(prefecture_label,
                              levels = c("Fukuoka", "Tokyo", "Japan"))
  )

# ════════════════════════════════════════════════════════════════════════════
# DECOMPOSITION — Period 1 (AAGR, 1970–1992)
# ════════════════════════════════════════════════════════════════════════════
n_p1 <- 22

rv_decomp_p1 <- rv_full %>%
  filter(prefecture_code %in% c(13, 40)) %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, sector, all_of(years)) %>%
  bind_rows(rv_japan_decomp %>% dplyr::select(prefecture_label, sector, all_of(years))) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "RV") %>%
  mutate(year = as.integer(year), RV = as.numeric(RV)) %>%
  filter(year %in% c(1970, 1992)) %>%
  group_by(prefecture_label, sector, year) %>%
  summarise(RV = sum(RV, na.rm = TRUE), .groups = "drop")

mh_decomp_p1 <- mh_full %>%
  filter(prefecture_code %in% c(13, 40)) %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, sector, all_of(years)) %>%
  bind_rows(mh_japan_decomp %>% dplyr::select(prefecture_label, sector, all_of(years))) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "MH") %>%
  mutate(year = as.integer(year), MH = as.numeric(MH)) %>%
  filter(year %in% c(1970, 1992), sector != "Total") %>%
  group_by(prefecture_label, sector, year) %>%
  summarise(MH = sum(MH, na.rm = TRUE), .groups = "drop")

mh_total_p1 <- mh_decomp_p1 %>%
  group_by(prefecture_label, year) %>%
  summarise(MH_total = sum(MH, na.rm = TRUE), .groups = "drop")

decomp_base_p1 <- rv_decomp_p1 %>%
  filter(sector != "Total") %>%
  inner_join(mh_decomp_p1, by = c("prefecture_label", "sector", "year")) %>%
  inner_join(mh_total_p1, by = c("prefecture_label", "year")) %>%
  mutate(x = RV / MH, s = MH / MH_total) %>%
  dplyr::select(prefecture_label, sector, year, x, s)

decomp_wide_p1 <- decomp_base_p1 %>%
  pivot_wider(names_from = year, values_from = c(x, s)) %>%
  mutate(delta_x = x_1992 - x_1970, delta_s = s_1992 - s_1970)

decomp_results_p1 <- decomp_wide_p1 %>%
  group_by(prefecture_label) %>%
  summarise(
    Within      = sum(s_1970 * delta_x),
    Across      = sum(x_1970 * delta_s),
    Interaction = sum(delta_s * delta_x),
    Total_X     = sum(s_1970 * delta_x) +
      sum(x_1970 * delta_s) +
      sum(delta_s * delta_x),
    .groups = "drop"
  ) %>%
  left_join(
    decomp_base_p1 %>%
      filter(year == 1970) %>%
      group_by(prefecture_label) %>%
      summarise(X_1970 = sum(s * x), .groups = "drop"),
    by = "prefecture_label"
  ) %>%
  mutate(
    Within_aagr      = 100 * Within      / (X_1970 * n_p1),
    Across_aagr      = 100 * Across      / (X_1970 * n_p1),
    Interaction_aagr = 100 * Interaction / (X_1970 * n_p1),
    Total_aagr       = 100 * Total_X     / (X_1970 * n_p1)
  )

print(decomp_results_p1 %>%
        dplyr::select(prefecture_label, Within_aagr, Across_aagr,
                      Interaction_aagr, Total_aagr))

decomp_plot_p1 <- decomp_results_p1 %>%
  dplyr::select(prefecture_label, Within_aagr, Across_aagr,
                Interaction_aagr, Total_aagr) %>%
  pivot_longer(cols = -prefecture_label,
               names_to = "component", values_to = "value") %>%
  mutate(
    component = recode(component,
                       "Within_aagr"      = "Within",
                       "Across_aagr"      = "Across",
                       "Interaction_aagr" = "Interaction",
                       "Total_aagr"       = "Total"),
    component = factor(component,
                       levels = c("Total", "Within", "Across", "Interaction")),
    prefecture_label = factor(prefecture_label,
                              levels = c("Fukuoka", "Tokyo", "Japan"))
  )

# ════════════════════════════════════════════════════════════════════════════
# FIGURE 2 — Combined decomposition (Period 1 left, Period 2 right)
# ════════════════════════════════════════════════════════════════════════════
plot_p1 <- ggplot(decomp_plot_p1,
                  aes(x = component, y = value, fill = prefecture_label)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_text(aes(label = round(value, 2),
                vjust = ifelse(value >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.6),
            size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Fukuoka" = "#BA7517", "Tokyo" = "#185FA5", "Japan" = "#1D9E75")) +
  labs(title = "Period 1: 1970–1992",
       x = "Component",
       y = "Contribution to AAGR (% per year)",
       fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.title         = element_text(face = "bold", size = 12)
  )

plot_p2 <- ggplot(decomp_plot,
                  aes(x = component, y = value, fill = prefecture_label)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_text(aes(label = round(value, 2),
                vjust = ifelse(value >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.6),
            size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Fukuoka" = "#BA7517", "Tokyo" = "#185FA5", "Japan" = "#1D9E75")) +
  labs(title = "Period 2: 1992–2012",
       x = "Component",
       y = NULL,
       fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.title         = element_text(face = "bold", size = 12)
  )

combined_plot <- (plot_p1 + plot_p2) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "top")

combined_plot <- combined_plot +
  plot_annotation(
    title   = "GDP per worker decomposition — Fukuoka, Tokyo and Japan",
    caption = "Source: RIETI R-JIP 2017. AAGR decomposition of real GDP per man-hour.\nWithin = within-sector productivity growth. Across = labor reallocation. Interaction = cross term.",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 13),
      plot.caption = element_text(color = "grey50", size = 8, hjust = 0)
    )
  )

ggsave("Figures/figure2_decomposition_combined.png",
       combined_plot,
       width = 14, height = 5.5, dpi = 300, bg = "white")
message("Done — saved as figure2_decomposition_combined.png")

# ════════════════════════════════════════════════════════════════════════════
# FIGURE 3 — Industry-level LP gap, Fukuoka vs Tokyo, 1970 and 1992
# ════════════════════════════════════════════════════════════════════════════
rv_ind <- rv_full %>%
  filter(prefecture_code %in% c(13, 40),
         industry_code != "T") %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, industry_code, industry, all_of(years)) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "RV") %>%
  mutate(year = as.integer(year), RV = as.numeric(RV)) %>%
  filter(year %in% c(1970, 1992))

mh_ind <- mh_full %>%
  filter(prefecture_code %in% c(13, 40),
         industry_code != "T") %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, industry_code, industry, all_of(years)) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "MH") %>%
  mutate(year = as.integer(year), MH = as.numeric(MH)) %>%
  filter(year %in% c(1970, 1992))

lp_ind <- rv_ind %>%
  inner_join(mh_ind, by = c("prefecture_label", "industry_code",
                            "industry", "year")) %>%
  mutate(LP = RV / MH)

lp_wide <- lp_ind %>%
  dplyr::select(prefecture_label, industry_code, industry, year, LP) %>%
  pivot_wider(names_from = prefecture_label, values_from = LP) %>%
  mutate(
    LP_gap = Tokyo - Fukuoka,
    sector = case_when(
      industry_code == "1"                   ~ "Agriculture",
      industry_code %in% as.character(2:16)  ~ "Industry",
      industry_code %in% as.character(17:23) ~ "Services"
    )
  )

lp_gap <- lp_wide %>%
  filter(year %in% c(1970, 1992)) %>%
  arrange(industry_code, year) %>%
  mutate(industry_short = case_when(
    industry == "Agriculture, forestry, and fisheries" ~ "Agriculture",
    industry == "Mining"                               ~ "Mining",
    industry == "Food and beverages"                   ~ "Food & beverages",
    industry == "Textile mill products"                ~ "Textiles",
    industry == "Pulp and paper"                       ~ "Pulp & paper",
    industry == "Chemicals"                            ~ "Chemicals",
    industry == "Petroleum and coal products"          ~ "Petroleum & coal",
    industry == "Ceramics, stone and clay"             ~ "Ceramics",
    industry == "Basic metal"                          ~ "Basic metals",
    industry == "Processed metals"                     ~ "Processed metals",
    industry == "General machinery"                    ~ "General machinery",
    industry == "Electrical machinery"                 ~ "Electrical machinery",
    industry == "Transport equipment"                  ~ "Transport equipment",
    industry == "Precision instruments"                ~ "Precision instruments",
    industry == "Other manufacturing"                  ~ "Other manufacturing",
    industry == "Construction"                         ~ "Construction",
    industry == "Electricity, gas and water utilities" ~ "Utilities",
    industry == "Wholesale and retail trade"           ~ "Wholesale & retail",
    industry == "Finance and insurance"                ~ "Finance & insurance",
    industry == "Real estate"                          ~ "Real estate",
    industry == "Transport and communications"         ~ "Transport & comms",
    industry == "Non-government other services"        ~ "Non-govt services",
    industry == "Government other services"            ~ "Govt services",
    TRUE                                               ~ industry
  ))

industry_order <- lp_gap %>%
  filter(year == 1992) %>%
  arrange(desc(LP_gap)) %>%
  pull(industry_short)

lp_gap <- lp_gap %>%
  mutate(
    industry_short = factor(industry_short, levels = rev(industry_order)),
    year_label     = paste0(year),
    sector         = factor(sector, levels = c("Agriculture", "Industry", "Services"))
  )

ggplot(lp_gap, aes(x = LP_gap, y = industry_short, fill = sector)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
  facet_wrap(~ year_label, ncol = 2) +
  scale_fill_manual(values = c(
    "Agriculture" = "#1D9E75",
    "Industry"    = "#185FA5",
    "Services"    = "#BA7517"
  )) +
  labs(
    title    = "Labor productivity gap: Tokyo minus Fukuoka, by industry",
    subtitle = "Positive = Tokyo more productive. 1970 and 1992.",
    x        = "LP gap (million yen per man-hour)",
    y        = NULL,
    fill     = "Sector",
    caption  = "Source: RIETI R-JIP 2017. LP = real value added (2000 prices) / man-hours.\nIndustries ordered by 1992 gap size."
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position    = "top",
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    plot.caption       = element_text(color = "grey50", size = 8, hjust = 0),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    axis.text.y        = element_text(size = 9)
  )

ggsave("Figures/figure3_industry_LP_gap.png",
       width = 11, height = 7, dpi = 300, bg = "white")
message("Done — saved as figure3_industry_LP_gap.png")

# ════════════════════════════════════════════════════════════════════════════
# FIGURE 4 — OLS regression coefficient plot
# ════════════════════════════════════════════════════════════════════════════
rv_ind_all <- rv_full %>%
  filter(prefecture_code %in% c(13, 40),
         industry_code != "T") %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, industry_code, industry, all_of(years)) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "RV") %>%
  mutate(year = as.integer(year), RV = as.numeric(RV)) %>%
  filter(year >= 1970, year <= 1992)

mh_ind_all <- mh_full %>%
  filter(prefecture_code %in% c(13, 40),
         industry_code != "T") %>%
  mutate(prefecture_label = ifelse(prefecture_code == 13, "Tokyo", "Fukuoka")) %>%
  dplyr::select(prefecture_label, industry_code, industry, all_of(years)) %>%
  pivot_longer(cols = all_of(years), names_to = "year", values_to = "MH") %>%
  mutate(year = as.integer(year), MH = as.numeric(MH)) %>%
  filter(year >= 1970, year <= 1992)

lp_annual <- rv_ind_all %>%
  inner_join(mh_ind_all,
             by = c("prefecture_label", "industry_code", "industry", "year")) %>%
  mutate(LP = RV / MH)

lp_gap_annual <- lp_annual %>%
  dplyr::select(prefecture_label, industry_code, industry, year, LP) %>%
  pivot_wider(names_from = prefecture_label, values_from = LP) %>%
  mutate(
    LP_gap = Tokyo - Fukuoka,
    sector = case_when(
      industry_code == "1"                   ~ "Agriculture",
      industry_code %in% as.character(2:16)  ~ "Industry",
      industry_code %in% as.character(17:23) ~ "Services"
    ),
    industry_short = case_when(
      industry == "Agriculture, forestry, and fisheries" ~ "Agriculture",
      industry == "Mining"                               ~ "Mining",
      industry == "Food and beverages"                   ~ "Food & beverages",
      industry == "Textile mill products"                ~ "Textiles",
      industry == "Pulp and paper"                       ~ "Pulp & paper",
      industry == "Chemicals"                            ~ "Chemicals",
      industry == "Petroleum and coal products"          ~ "Petroleum & coal",
      industry == "Ceramics, stone and clay"             ~ "Ceramics",
      industry == "Basic metal"                          ~ "Basic metals",
      industry == "Processed metals"                     ~ "Processed metals",
      industry == "General machinery"                    ~ "General machinery",
      industry == "Electrical machinery"                 ~ "Electrical machinery",
      industry == "Transport equipment"                  ~ "Transport equipment",
      industry == "Precision instruments"                ~ "Precision instruments",
      industry == "Other manufacturing"                  ~ "Other manufacturing",
      industry == "Construction"                         ~ "Construction",
      industry == "Electricity, gas and water utilities" ~ "Utilities",
      industry == "Wholesale and retail trade"           ~ "Wholesale & retail",
      industry == "Finance and insurance"                ~ "Finance & insurance",
      industry == "Real estate"                          ~ "Real estate",
      industry == "Transport and communications"         ~ "Transport & comms",
      industry == "Non-government other services"        ~ "Non-govt services",
      industry == "Government other services"            ~ "Govt services",
      TRUE                                               ~ industry
    )
  )

lp_gap_annual <- lp_gap_annual %>%
  mutate(industry_short = relevel(factor(industry_short), ref = "Agriculture"))

reg_model <- lm(LP_gap ~ industry_short + year, data = lp_gap_annual)
summary(reg_model)

coef_df <- tidy(reg_model, conf.int = TRUE) %>%
  filter(str_detect(term, "industry_short")) %>%
  mutate(
    industry_short = str_remove(term, "industry_short"),
    sector = case_when(
      industry_short %in% c("Mining", "Food & beverages", "Textiles",
                            "Pulp & paper", "Chemicals", "Petroleum & coal",
                            "Ceramics", "Basic metals", "Processed metals",
                            "General machinery", "Electrical machinery",
                            "Transport equipment", "Precision instruments",
                            "Other manufacturing", "Construction")       ~ "Industry",
      industry_short %in% c("Utilities", "Wholesale & retail",
                            "Finance & insurance", "Real estate",
                            "Transport & comms", "Non-govt services",
                            "Govt services")                             ~ "Services",
      TRUE                                                               ~ "Agriculture"
    ),
    significant = ifelse(p.value < 0.05, "Significant", "Not significant")
  ) %>%
  arrange(desc(estimate))

coef_df <- coef_df %>%
  mutate(industry_short = factor(industry_short,
                                 levels = industry_short[order(estimate)]))

ggplot(coef_df, aes(x = estimate, y = industry_short,
                    color = sector, shape = significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  scale_color_manual(values = c(
    "Agriculture" = "#1D9E75",
    "Industry"    = "#185FA5",
    "Services"    = "#BA7517"
  )) +
  scale_shape_manual(values = c(
    "Significant"     = 16,
    "Not significant" = 1
  )) +
  labs(
    title    = "Industry LP gap relative to Agriculture: Fukuoka vs Tokyo, 1970–1992",
    subtitle = "OLS coefficients with 95% confidence intervals. Reference = Agriculture.",
    x        = "LP gap coefficient (million yen per man-hour)",
    y        = NULL,
    color    = "Sector",
    shape    = NULL,
    caption  = "Source: RIETI R-JIP 2017. LP gap = Tokyo minus Fukuoka real value added per man-hour.\nReference category (Agriculture) has coefficient = 0 by construction.\nRegression controls for year trend. Filled = significant at 5%, open = not significant."
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position    = "top",
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    plot.caption       = element_text(color = "grey50", size = 8, hjust = 0),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    axis.text.y        = element_text(size = 10.5)
  )

ggsave("Figures/figure4_regression_coef.png",
       width = 10, height = 7, dpi = 300, bg = "white")
message("Done — saved as figure4_regression_coef.png")