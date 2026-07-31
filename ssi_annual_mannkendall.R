# =============================================================================
# ssi_annual_mannkendall.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : SSI — temporal trend
# Used in     : Ch. 4 Methods (Temporal Trend Analysis); ESSI Component 2
# Status      : FINAL
#
# PURPOSE
#   Extends the static SSI by computing annual SSI scores per station
#   1997-2023, then applying a Mann-Kendall trend test to each station time
#   series. Produces the SSI Trend layer.
#
# INPUTS      : all_sc_wq.xlsx; ssi_results.csv
# OUTPUTS     : ssi_annual_scores.csv; MK_results_all_stations.csv; MK_results_significant.csv; MK_area_summary.csv
# RUN AFTER   : ssi_calculate.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# SSI Annual Time Series + Mann-Kendall Trend Analysis
# Kate Chatman | MPA/EVSS Thesis | College of Charleston / SC Sea Grant
# Last updated: May 2026
#
# PURPOSE:
#   This script extends the static SSI (ssicode_april26.R) by computing
#   annual SSI scores per station across 1997-2023, then running a
#   Mann-Kendall trend test on each station's time series.
#   Output feeds the SSI Trend Layer (Component 1) of the enhanced
#   Shellfish Suitability Index and the ArcGIS Pro suitability surface.
#
# INPUTS:
#   - all_sc_wq.xlsx (61,681 observations, 593 stations, May-Sept 1997-2023)
#   - SSI_Results_with_coords.csv (static SSI with lat/lon for coord join)
#
# OUTPUTS:
#   - ssi_annual_scores.csv     : annual SSI per station (wide + long formats)
#   - MK_results_all_stations.csv  : MK tau, p-value, trend direction, all stations
#   - MK_results_significant.csv   : filtered to p < 0.05 only
#   - MK_area_summary.csv          : trend counts aggregated by management area
#
# MINIMUM DATA THRESHOLD:
#   Stations must have >= 10 years of data to be included in MK analysis.
#   This is the standard minimum for reliable trend detection.
# =============================================================================

# --- 1. LIBRARIES ------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(kendall)      # install.packages("kendall") if needed
# library(Kendall)    # capital-K version also works; function is MannKendall()

# --- 2. LOAD AND CLEAN DATA --------------------------------------------------
df <- read_excel("all_sc_wq.xlsx")

# Strip "Station_" prefix to match SSI_Results_with_coords.csv format
df$Station <- gsub("^Station_", "", df$Station)
names(df) <- make.names(names(df))

# Remove zero and NA salinity (same filter as static SSI code)
df_clean <- df %>%
  filter(!is.na(Salinity), Salinity != 0)

# Confirm we're working with summer months only (May=5 through Sept=9)
# The SCDES dataset is summer-only by design; this is a sanity check
stopifnot(all(df_clean$Month %in% 5:9))

# Select only what we need
df_sal <- df_clean %>%
  select(Station, Area, Salinity, Year, Month)

# --- 3. ANNUAL SSI CALCULATION -----------------------------------------------
# Duration score requires a custom function (same logic as static SSI)
calculate_duration_score <- function(sal_vector) {
  suboptimal <- sal_vector < 10 | sal_vector > 35
  suboptimal <- suboptimal[!is.na(suboptimal)]
  if (length(suboptimal) == 0) return(100)
  rle_result <- rle(suboptimal)
  max_consecutive <- if (any(rle_result$values)) {
    max(rle_result$lengths[rle_result$values])
  } else {
    0
  }
  duration_score <- pmax(0, 100 - (max_consecutive ^ 1.2))
  return(duration_score)
}

# Compute all four SSI components annually, grouped by Station + Year
ssi_annual <- df_sal %>%
  group_by(Station, Area, Year) %>%
  summarise(
    n_obs            = n(),
    mean_sal         = mean(Salinity, na.rm = TRUE),
    sd_sal           = sd(Salinity, na.rm = TRUE),

    # Component 1: Optimal Range Score (w = 0.30)
    or_score         = sum(Salinity >= 10 & Salinity <= 35) / n() * 100,

    # Component 2: CV Stability Score (w = 0.30)
    cv               = (sd(Salinity, na.rm = TRUE) / mean(Salinity, na.rm = TRUE)) * 100,
    cv_stability     = pmax(0, 100 - cv),

    # Component 3: Extreme Event Score (w = 0.25)
    # Below 8 ppt weighted x2 (higher mortality risk); above 35 ppt x1.25
    extreme_event    = pmax(0, 100 -
                         (sum(Salinity < 8) / n() * 100 * 2.0) -
                         (sum(Salinity > 35) / n() * 100 * 1.25)),

    # Component 4: Duration Score (w = 0.15) — computed via custom function
    duration         = calculate_duration_score(Salinity),

    .groups = "drop"
  ) %>%
  mutate(
    # Weighted composite — same weights as static SSI
    ssi_annual = round(
      (0.30 * or_score) +
      (0.30 * cv_stability) +
      (0.25 * extreme_event) +
      (0.15 * duration),
      2
    ),
    # Classify each annual score
    stability_class = case_when(
      ssi_annual >= 90 ~ "Excellent (90-100)",
      ssi_annual >= 75 ~ "Good (75-89)",
      ssi_annual >= 60 ~ "Moderate (60-74)",
      ssi_annual >= 45 ~ "Poor (45-59)",
      TRUE             ~ "Unsuitable (<45)"
    )
  ) %>%
  arrange(Station, Year)

# Save annual scores (long format — one row per station-year)
write_csv(ssi_annual, "ssi_annual_scores.csv")
cat("Annual SSI scores written:", nrow(ssi_annual), "station-year records\n")

# --- 4. MANN-KENDALL TREND TEST ----------------------------------------------
# Applied to annual SSI scores at each station across the 27-year record.
# MK is nonparametric and rank-based — appropriate for environmental time
# series that may not meet normality assumptions (Mann 1945; Hipel & McLeod 2005).
#
# THRESHOLD: >= 10 years of annual data required per station.
# This is the accepted minimum for reliable MK trend detection.

# Identify qualifying stations
years_per_station <- ssi_annual %>%
  group_by(Station) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop")

qualifying_stations <- years_per_station %>%
  filter(n_years >= 10) %>%
  pull(Station)

cat("Stations qualifying for MK (>= 10 years):", length(qualifying_stations), "\n")

# Run Mann-Kendall for each qualifying station
mk_results <- ssi_annual %>%
  filter(Station %in% qualifying_stations) %>%
  group_by(Station) %>%
  arrange(Year, .by_group = TRUE) %>%
  summarise(
    n_years     = n(),
    mean_ssi    = round(mean(ssi_annual, na.rm = TRUE), 2),
    sd_ssi      = round(sd(ssi_annual, na.rm = TRUE), 2),
    first_year  = min(Year),
    last_year   = max(Year),

    # Mann-Kendall test
    mk_tau      = MannKendall(ssi_annual)$tau,
    mk_pvalue   = MannKendall(ssi_annual)$sl,   # two-sided p-value

    .groups = "drop"
  ) %>%
  mutate(
    # Trend direction classification
    # Using p < 0.10 for "marginal" and p < 0.05 for "significant"
    trend_direction = case_when(
      mk_pvalue < 0.05 & mk_tau > 0  ~ "Improving (significant)",
      mk_pvalue < 0.05 & mk_tau < 0  ~ "Worsening (significant)",
      mk_pvalue < 0.10 & mk_tau > 0  ~ "Improving (marginal)",
      mk_pvalue < 0.10 & mk_tau < 0  ~ "Worsening (marginal)",
      TRUE                            ~ "No trend"
    ),
    # Simplified 3-category version for mapping
    trend_simple = case_when(
      mk_pvalue < 0.05 & mk_tau > 0  ~ "Improving",
      mk_pvalue < 0.05 & mk_tau < 0  ~ "Worsening",
      TRUE                            ~ "Stable"
    )
  ) %>%
  arrange(mk_pvalue)

# --- 5. JOIN COORDINATES FROM STATIC SSI FILE --------------------------------
coords <- read_csv("SSI_Results_with_coords.csv") %>%
  select(Station, LATITUDE, LONGITUDE) %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE))

mk_results <- mk_results %>%
  left_join(coords, by = "Station")

# Join management area from annual data
area_lookup <- ssi_annual %>%
  select(Station, Area) %>%
  distinct()

mk_results <- mk_results %>%
  left_join(area_lookup, by = "Station")

# --- 6. WRITE OUTPUTS --------------------------------------------------------
# All qualifying stations
write_csv(mk_results, "MK_results_all_stations.csv")
cat("MK results written for", nrow(mk_results), "stations\n")

# Significant trends only (p < 0.05)
mk_significant <- mk_results %>%
  filter(mk_pvalue < 0.05)
write_csv(mk_significant, "MK_results_significant.csv")
cat("Significant trends (p < 0.05):", nrow(mk_significant), "stations\n")

# --- 7. AREA-LEVEL SUMMARY ---------------------------------------------------
# Aggregate trend counts by management area for thesis Table X and ArcGIS labeling
mk_area_summary <- mk_results %>%
  group_by(Area) %>%
  summarise(
    n_stations       = n(),
    n_improving_sig  = sum(trend_simple == "Improving"),
    n_worsening_sig  = sum(trend_simple == "Worsening"),
    n_stable         = sum(trend_simple == "Stable"),
    pct_worsening    = round(n_worsening_sig / n_stations * 100, 1),
    pct_improving    = round(n_improving_sig / n_stations * 100, 1),
    mean_tau         = round(mean(mk_tau, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_worsening))

write_csv(mk_area_summary, "MK_area_summary.csv")
cat("Area summary written for", nrow(mk_area_summary), "management areas\n")

# --- 8. CONSOLE SUMMARY ------------------------------------------------------
cat("\n===== MANN-KENDALL SUMMARY =====\n")
cat("Total qualifying stations:", nrow(mk_results), "\n")
print(table(mk_results$trend_simple))
cat("\nTop worsening stations (p < 0.05, most negative tau):\n")
print(
  mk_results %>%
    filter(trend_simple == "Worsening") %>%
    select(Station, Area, n_years, mk_tau, mk_pvalue, LATITUDE, LONGITUDE) %>%
    head(15)
)
cat("\nTop improving stations (p < 0.05, most positive tau):\n")
print(
  mk_results %>%
    filter(trend_simple == "Improving") %>%
    select(Station, Area, n_years, mk_tau, mk_pvalue, LATITUDE, LONGITUDE) %>%
    head(15)
)
cat("\nArea summary (sorted by % worsening):\n")
print(mk_area_summary)

# =============================================================================
# NEXT STEPS (for thesis Chapter 5):
#
# 1. Load MK_results_all_stations.csv into ArcGIS Pro
#    - Join to station point layer on Station field
#    - Symbolize by trend_simple (Improving / Stable / Worsening)
#    - This becomes your SSI Trend Layer — Figure X in Chapter 5
#
# 2. Cross-check worsening stations against SCDES CUR_CLASS layer
#    - Areas 17-20 should appear prominently in both
#    - This convergence is your cross-dataset validation argument
#
# 3. Feed mk_tau values as input weight into ArcGIS weighted overlay
#    - More negative tau = higher weight on vulnerability in predictive layer
#
# 4. Storm vulnerability analysis uses separate WQP dataset
#    - Files in: thesis/scdeswqpdata97_23
#    - Script: to be developed (storm_vulnerability.R)
# =============================================================================
