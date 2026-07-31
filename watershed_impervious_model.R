# =============================================================================
# watershed_impervious_model.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Watershed drivers
# Used in     : Ch. 4 Methods (Watershed Drivers)
# Status      : FINAL — see caveat in reference doc
#
# PURPOSE
#   Joins monitoring stations to HUC12 watersheds, calculates mean fractional
#   impervious surface per watershed from NLCD 2021, and tests whether
#   impervious surface predicts SSI and FC Mann-Kendall trend direction.
#
# INPUTS      : stations_with_HUC12.csv; HUC12_impervious_stats.csv; MK_results_all_stations.csv; FC_MK_results_all.csv
# OUTPUTS     : watershed_analysis_full.csv; watershed_area_summary.csv; scatter and table figures
# RUN AFTER   : ssi_annual_mannkendall.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# watershed_model.R
# Kate Chatman | MPA/EVSS Thesis | College of Charleston / SC Sea Grant
# June 2026
#
# PURPOSE:
#   SSI Component 3: Watershed Impervious Surface Model
#   Joins SCDES monitoring stations to HUC12 watershed boundaries,
#   calculates mean impervious surface per watershed from NLCD 2021,
#   and tests whether impervious surface predicts SSI and FC MK trend
#   direction across SC's coastal shellfish management areas.
#
# WORKFLOW:
#   ArcGIS Pro (completed before this script):
#   1. Merge WBDHU12 from four NHD HU4 tiles (0304, 0305, 0306, 0307)
#      → SC_HUC12_merged (347 watersheds)
#   2. Spatial Join: des_stations_kc → SC_HUC12_merged (INTERSECT)
#      → stations_with_HUC12.csv (582 stations, all matched)
#   3. Zonal Statistics as Table: SC_HUC12_merged → NLCD 2021 impervious
#      Zone field: HUC12 | Statistics: MEAN
#      → HUC12_impervious_stats.csv (347 watersheds, MEAN 0-46.1%)
#
# KEY FINDINGS:
#   - Impervious surface explains <4% of SSI trend variance (R²=0.035)
#   - FC trend shows NO relationship with impervious surface (R²≈0)
#   - Worsening SSI concentrated in LOW-impervious watersheds (Areas 19, 17, 06A)
#   - Suggests tidal/hydrological drivers rather than urbanization
#   - High impervious areas (Areas 3, 2, 1) show worsening FC but stable SSI
#
# INPUTS:
#   - stations_with_HUC12.csv      : from ArcGIS Spatial Join
#   - HUC12_impervious_stats.csv   : from ArcGIS Zonal Statistics
#   - MK_results_all_stations.csv  : SSI MK results
#   - FC_MK_results_all.csv        : FC MK results
#
# OUTPUTS:
#   - watershed_analysis_full.csv          : per-station joined data
#   - watershed_area_summary.csv           : area-level summary
#   - watershed_impervious_ssi_scatter.png : scatter plot Figure 5.X
#   - watershed_impervious_table.png       : area-level table Table 5.X
# =============================================================================

library(tidyverse)
library(ggplot2)
library(gt)

setwd("C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts")

# --- 1. LOAD DATA ------------------------------------------------------------

stations_huc <- read_csv("stations_with_HUC12.csv") %>%
  select(Station, HUC12) %>%
  filter(!is.na(HUC12))

huc_impervious <- read_csv("HUC12_impervious_stats.csv") %>%
  select(HUC12, mean_impervious = MEAN) %>%
  filter(!is.na(mean_impervious))

ssi_mk <- read_csv("MK_results_all_stations.csv") %>%
  select(Station, Area, mk_tau, mk_pvalue, mean_ssi, trend_simple,
         LATITUDE, LONGITUDE)

fc_mk <- read_csv("FC_MK_results_all.csv") %>%
  select(Station, fc_tau = mk_tau, fc_trend = trend_simple)

cat("Stations with HUC12 assignment:", nrow(stations_huc), "\n")
cat("HUC12 watersheds with impervious data:", nrow(huc_impervious), "\n")
cat("Impervious surface range:", round(min(huc_impervious$mean_impervious), 1),
    "to", round(max(huc_impervious$mean_impervious), 1), "%\n")

# --- 2. JOIN ALL DATA --------------------------------------------------------

watershed_analysis <- stations_huc %>%
  left_join(huc_impervious, by = "HUC12") %>%
  left_join(ssi_mk, by = "Station") %>%
  filter(!is.na(mean_impervious), !is.na(mk_tau))

watershed_full <- watershed_analysis %>%
  left_join(fc_mk, by = "Station")

cat("Stations with complete watershed data:", nrow(watershed_full), "\n")

# --- 3. REGRESSION ANALYSES -------------------------------------------------

model_ssi <- lm(mk_tau ~ mean_impervious, data = watershed_full)
model_fc  <- lm(fc_tau ~ mean_impervious, data = watershed_full)

cat("\n=== SSI MK ~ Impervious Surface ===\n")
cat("Coefficient:", round(coef(model_ssi)["mean_impervious"], 5), "\n")
cat("R-squared:", round(summary(model_ssi)$r.squared, 4), "\n")
cat("p-value:", format(summary(model_ssi)$coefficients[2,4], scientific=TRUE), "\n")

cat("\n=== FC MK ~ Impervious Surface ===\n")
cat("Coefficient:", round(coef(model_fc)["mean_impervious"], 5), "\n")
cat("R-squared:", round(summary(model_fc)$r.squared, 4), "\n")
cat("p-value:", format(summary(model_fc)$coefficients[2,4], scientific=TRUE), "\n")

# --- 4. AREA-LEVEL SUMMARY --------------------------------------------------

area_watershed <- watershed_full %>%
  group_by(Area) %>%
  summarise(
    n_stations        = n(),
    mean_impervious   = round(mean(mean_impervious, na.rm=TRUE), 1),
    mean_ssi_tau      = round(mean(mk_tau, na.rm=TRUE), 3),
    mean_fc_tau       = round(mean(fc_tau, na.rm=TRUE), 3),
    pct_ssi_worsening = round(sum(trend_simple == "Worsening") / n() * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_impervious))

cat("\n=== AREA-LEVEL: IMPERVIOUS SURFACE vs SSI AND FC TRENDS ===\n")
print(area_watershed, n = 25)

# --- 5. WRITE OUTPUTS -------------------------------------------------------

write_csv(watershed_full,    "watershed_analysis_full.csv")
write_csv(area_watershed,    "watershed_area_summary.csv")

cat("\nCSV outputs written.\n")

# --- 6. FIGURES -------------------------------------------------------------

# Scatter: impervious vs SSI tau colored by FC trend
p1 <- ggplot(watershed_full,
             aes(x = mean_impervious, y = mk_tau, color = fc_trend)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", linewidth = 0.8) +
  scale_color_manual(values = c(
    "Improving" = "#27AE60",
    "Stable"    = "#95A5A6",
    "Worsening" = "#C0392B"
  ), name = "FC Trend") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title    = "Watershed Impervious Surface vs. SSI Mann-Kendall Trend",
    subtitle = "R\u00b2 = 0.035, p < 0.001 \u2014 weak association; worsening SSI in low-impervious watersheds",
    x        = "Mean Watershed Impervious Surface (%)",
    y        = "SSI Mann-Kendall Tau (negative = worsening stability)",
    caption  = "Source: NLCD 2021 + SCDES 1997\u20132023 | Analysis: Chatman 2026"
  ) +
  theme_minimal(base_size = 12)

ggsave("watershed_impervious_ssi_scatter.png", p1,
       width = 9, height = 6, dpi = 300)

# Area-level formatted table
area_watershed %>%
  arrange(desc(mean_impervious)) %>%
  gt() %>%
  tab_header(
    title    = "Watershed Impervious Surface by Shellfish Management Area",
    subtitle = "NLCD 2021 Fractional Impervious Surface | HUC12 Watershed Mean"
  ) %>%
  cols_label(
    Area              = "Management Area",
    n_stations        = "N Stations",
    mean_impervious   = "Mean Impervious (%)",
    mean_ssi_tau      = "Mean SSI Tau",
    mean_fc_tau       = "Mean FC Tau",
    pct_ssi_worsening = "% SSI Worsening"
  ) %>%
  tab_style(
    style     = cell_fill(color = "#FFEBEE"),
    locations = cells_body(rows = mean_impervious >= 20)
  ) %>%
  tab_style(
    style     = cell_fill(color = "#E8F5E9"),
    locations = cells_body(rows = mean_impervious <= 2)
  ) %>%
  tab_style(
    style     = list(cell_text(weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_footnote(
    footnote  = "SSI worsening concentrated in low-impervious watersheds — tidal/hydrological drivers",
    locations = cells_body(rows = Area %in% c("19", "06A", "17"))
  ) %>%
  tab_source_note(
    "Sources: NLCD 2021 (MRLC); NHD HUC12 Watersheds (USGS); SCDES WQ Monitoring | Analysis: Chatman 2026"
  ) %>%
  gtsave("watershed_impervious_table.png", expand = 20)

cat("Figures saved.\n")

# =============================================================================
# THESIS INTERPRETATION — CHAPTER 5.3:
#
# The watershed impervious surface model tests whether land cover change
# (urbanization) explains SSI and FC trend direction across SC's coastal
# shellfish management areas.
#
# FINDING: Impervious surface is NOT a meaningful predictor of either
# SSI or FC trend direction at the HUC12 watershed scale.
#
# SSI: R² = 0.035 — 3.5% of variance explained. Statistically significant
# (p < 0.001) but ecologically trivial. Counterintuitive positive direction
# likely reflects confounding: most urbanized areas (Charleston, Hilton Head)
# are in high-salinity, ocean-influenced zones with engineered stormwater.
#
# FC: R² ≈ 0 — no meaningful relationship. Fecal coliform trends are driven
# by point source management, seasonal patterns, and event-based loading,
# not by static land cover composition.
#
# IMPLICATION: SC's worsening SSI stations (Areas 19, 17, 06A) are in
# LOW-impervious, rural watersheds. Their salinity instability reflects
# tidal dynamics, sea level rise, or agricultural drainage — not urban
# stormwater. This finding reinforces that salinity stability is a
# fundamentally different ecological dimension from contamination burden,
# and cannot be predicted from land cover alone.
#
# For the thesis: this null finding is methodologically honest and
# defensible. It strengthens the case for the SSI as a distinct
# contribution — the index captures something that standard land cover
# proxies do not.
# =============================================================================
