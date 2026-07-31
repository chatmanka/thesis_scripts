# =============================================================================
# station_tier_classification.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Regulatory gap analysis
# Used in     : Ch. 6 Governance
# Status      : FINAL
#
# PURPOSE
#   Cross-joins SSI and fecal coliform Mann-Kendall results to classify each
#   station into a three-tier mariculture suitability framework based on dual
#   biological and water quality conditions.
#
# INPUTS      : MK_results_all_stations.csv; FC_MK_results_all.csv; CUR_CLASS spatial join
# OUTPUTS     : SSI_MK_stations_with_classification.csv; tier summary
# RUN AFTER   : ssi_annual_mannkendall.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# station_tier_classification.R
# Kate Chatman | MPA/EVSS Thesis | College of Charleston / SC Sea Grant
# May 30, 2026
#
# PURPOSE:
#   Cross-joins SSI MK and FC MK trend results to classify each monitoring
#   station into a three-tier mariculture suitability framework based on
#   dual biological and water quality conditions.
#
# TIER LOGIC:
#   Tier 1 — Reclassification Candidate:
#     Both FC trend (Stable/Improving) AND SSI trend (Stable/Improving)
#     Both conditions met — suitable for reclassification consideration
#
#   Tier 2 — Salinity Risk — Monitor Closely:
#     FC Stable/Improving BUT SSI Worsening
#     Water quality improving but salinity instability poses mortality risk
#     NOT recommended for reclassification regardless of FC trend
#
#   Tier 3a — Watershed Intervention Required:
#     FC Worsening, SSI Stable/Improving
#     Water quality deteriorating — source control needed upstream
#
#   Tier 3b — Doubly Constrained:
#     Both FC Worsening AND SSI Worsening
#     Strongest justification for maintained or strengthened restriction
#
# KEY FINDING:
#   129 stations in Restricted areas meet Tier 1 dual conditions
#   (both FC and SSI stable or improving, currently Restricted classification)
#
# INPUTS:
#   - FC_MK_results_all.csv
#   - MK_results_all_stations.csv
#   - FC_MK_stations_with_classification.csv (spatial join output)
#
# OUTPUTS:
#   - station_tier_classification.csv : tier assignment for all 503 stations
# =============================================================================

library(tidyverse)

# --- 1. LOAD DATA ------------------------------------------------------------
fc_mk <- read_csv("FC_MK_results_all.csv") %>%
  select(Station, trend_simple) %>%
  rename(fc_trend = trend_simple)

ssi_mk <- read_csv("MK_results_all_stations.csv") %>%
  select(Station, Area, trend_simple, LATITUDE, LONGITUDE) %>%
  rename(ssi_trend = trend_simple)

fc_class <- read_csv("FC_MK_stations_with_classification.csv")

# --- 2. BUILD TIER CLASSIFICATIONS ------------------------------------------
combined <- inner_join(ssi_mk, fc_mk, by = "Station") %>%
  mutate(
    tier = case_when(
      fc_trend %in% c("Improving","Stable") &
        ssi_trend %in% c("Improving","Stable") ~ "Tier 1 — Reclassification Candidate",
      fc_trend == "Worsening" &
        ssi_trend %in% c("Improving","Stable") ~ "Tier 3 — Watershed Intervention Required",
      fc_trend %in% c("Improving","Stable") &
        ssi_trend == "Worsening"               ~ "Tier 2 — Salinity Risk — Monitor Closely",
      fc_trend == "Worsening" &
        ssi_trend == "Worsening"               ~ "Tier 3 — Doubly Constrained",
      TRUE ~ "Stable — No Action Indicated"
    )
  )

# --- 3. JOIN TO CLASSIFICATION LAYER ----------------------------------------
fc_tiers <- fc_class %>%
  left_join(combined %>% select(Station, tier, ssi_trend, fc_trend),
            by = "Station") %>%
  filter(!is.na(cur_class), !is.na(tier))

# --- 4. SUMMARY OUTPUTS -----------------------------------------------------
cat("===== TIER DISTRIBUTION =====\n")
print(table(combined$tier))

cat("\n===== TIER BY MANAGEMENT AREA =====\n")
print(table(combined$tier, combined$Area))

cat("\n===== TIER 1 RESTRICTED STATIONS =====\n")
tier1_restricted <- fc_tiers %>%
  filter(tier == "Tier 1 — Reclassification Candidate", cur_class == "R")
cat("Count:", nrow(tier1_restricted), "\n")

cat("\n===== TIER 1 RESTRICTED — RECLASSIFICATION ACTIVITY 2017-2023 =====\n")
tier1_reclass <- tier1_restricted %>%
  filter(!is.na(class_changes)) %>%
  mutate(
    activity = case_when(
      class_changes == 0 ~ "No change 2017-2023",
      class_changes >= 1 ~ "At least one change 2017-2023"
    )
  )
print(table(tier1_reclass$activity))

# --- 5. WRITE OUTPUT --------------------------------------------------------
write_csv(combined, "station_tier_classification.csv")
cat("\nstation_tier_classification.csv written:", nrow(combined), "stations\n")
