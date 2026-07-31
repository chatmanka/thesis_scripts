# =============================================================================
# reclassification_frequency.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Regulatory gap analysis
# Used in     : Ch. 6 Governance
# Status      : FINAL
#
# PURPOSE
#   Analyses frequency and direction of SCDES shellfish harvest area
#   reclassification 2017-2023 against the dual-condition tier framework,
#   testing whether improving areas are actually reclassified.
#
# INPUTS      : SCDES classification history; tier classification output
# OUTPUTS     : Reclassification frequency tables
# RUN AFTER   : station_tier_classification.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# reclassification_frequency.R
# Kate Chatman | MPA/EVSS Thesis | College of Charleston / SC Sea Grant
# May 30, 2026
#
# PURPOSE:
#   Analyzes the frequency and direction of SCDES shellfish harvest area
#   reclassification events between 2017 and 2023, cross-referenced against
#   the dual-condition tier framework (SSI MK + FC MK).
#
# CORE QUESTION:
#   Are areas with improving water quality (Tier 1) being reclassified
#   upward to Approved? Or is regulatory inertia preventing classification
#   changes that the water quality data would support?
#
# KEY FINDINGS:
#   - 129 Tier 1 Restricted stations (dual-condition met)
#   - 73 of 129 (56.6%) showed NO classification change 2017-2023
#   - Of 17 stations with improving FC in Restricted areas:
#     only 7 were ever reclassified to Approved 2017-2023
#     ALL 7 ended 2023 back at Restricted (oscillation pattern)
#   - Governance inertia confirmed: reclassification mechanism exists
#     but is not producing durable upward reclassification
#
# INPUTS:
#   - FC_MK_stations_with_classification.csv
#   - MK_results_all_stations.csv
#   - FC_MK_results_all.csv
#   - station_tier_classification.csv (from station_tier_classification.R)
#
# OUTPUTS:
#   - tier1_restricted_classification_history.csv
#   - reclassification_frequency_summary.csv
# =============================================================================

library(tidyverse)
library(gt)

# --- 1. REBUILD fc_tiers (requires station_tier_classification.R first) ------
fc_mk <- read_csv("FC_MK_results_all.csv") %>%
  select(Station, trend_simple) %>%
  rename(fc_trend = trend_simple)

ssi_mk <- read_csv("MK_results_all_stations.csv") %>%
  select(Station, Area, trend_simple, LATITUDE, LONGITUDE) %>%
  rename(ssi_trend = trend_simple)

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

fc_class <- read_csv("FC_MK_stations_with_classification.csv")

fc_tiers <- fc_class %>%
  left_join(combined %>% select(Station, tier, ssi_trend, fc_trend),
            by = "Station") %>%
  filter(!is.na(cur_class), !is.na(tier))

# --- 2. RECLASSIFICATION FREQUENCY BY CLASS ---------------------------------
cat("=== RECLASSIFICATION FREQUENCY BY CURRENT CLASS (2017-2023) ===\n")
reclassification_freq <- fc_tiers %>%
  filter(!is.na(class_changes)) %>%
  mutate(
    cur_class_label = case_when(
      cur_class == "A" ~ "Approved",
      cur_class == "R" ~ "Restricted",
      cur_class == "P" ~ "Prohibited",
      TRUE ~ cur_class
    ),
    reclass_rate = case_when(
      class_changes == 0 ~ "Never changed (2017-2023)",
      class_changes == 1 ~ "Changed once",
      class_changes == 2 ~ "Changed twice",
      class_changes >= 3 ~ "Changed 3+ times"
    )
  ) %>%
  group_by(cur_class_label, reclass_rate) %>%
  summarise(n_stations = n(), .groups = "drop") %>%
  pivot_wider(names_from = reclass_rate, values_from = n_stations, values_fill = 0)

print(reclassification_freq)

# --- 3. TIER 1 RESTRICTED — RECLASSIFICATION ACTIVITY ----------------------
cat("\n=== TIER 1 RESTRICTED STATIONS — RECLASSIFICATION ACTIVITY ===\n")
tier1_reclass <- fc_tiers %>%
  filter(tier == "Tier 1 — Reclassification Candidate",
         cur_class == "R",
         !is.na(class_changes)) %>%
  mutate(
    activity = case_when(
      class_changes == 0 ~ "No change 2017-2023",
      class_changes >= 1 ~ "At least one change 2017-2023"
    )
  )

cat("Total Tier 1 Restricted stations:", nrow(tier1_reclass), "\n")
print(table(tier1_reclass$activity))

# --- 4. YEAR-BY-YEAR CLASSIFICATION HISTORY ---------------------------------
tier1_temporal <- fc_tiers %>%
  filter(tier == "Tier 1 — Reclassification Candidate",
         cur_class == "R") %>%
  select(Station, Area, fc_trend, ssi_trend,
         C2017, C2018, C2019, C2020, C2021, C2022, C2023, class_changes) %>%
  filter(!is.na(C2017)) %>%
  arrange(desc(class_changes), Area)

cat("\n=== TIER 1 RESTRICTED — YEAR BY YEAR CLASSIFICATION (top 30) ===\n")
print(tier1_temporal, n = 30)

# --- 5. OSCILLATION ANALYSIS: EVER RECLASSIFIED TO APPROVED? ---------------
# KEY FINDING: stations with improving FC in Restricted areas —
# were any ever reclassified to Approved AND stayed there?
cat("\n=== TIER 1 IMPROVING FC RESTRICTED: EVER APPROVED 2017-2023? ===\n")
tier1_reclassified_up <- fc_tiers %>%
  filter(
    tier == "Tier 1 — Reclassification Candidate",
    fc_trend == "Improving",
    cur_class == "R"
  ) %>%
  select(Station, Area, C2017:C2023, class_changes, fc_trend, ssi_trend) %>%
  filter(!is.na(C2017)) %>%
  mutate(
    ever_approved = case_when(
      C2017 == "A" | C2018 == "A" | C2019 == "A" |
      C2020 == "A" | C2021 == "A" | C2022 == "A" | C2023 == "A" ~ "Yes",
      TRUE ~ "No"
    ),
    # Was 2023 classification Approved? (durable reclassification)
    approved_2023 = ifelse(C2023 == "A", "Approved in 2023", "Restricted in 2023")
  )

print(table(tier1_reclassified_up$ever_approved))

cat("\nStations ever reclassified to Approved — 2023 status:\n")
print(
  tier1_reclassified_up %>%
    filter(ever_approved == "Yes") %>%
    select(Station, Area, C2017:C2023, approved_2023)
)

cat("\nOSCILLATION FINDING: stations that were ever Approved but ended 2023 Restricted:\n")
oscillating <- tier1_reclassified_up %>%
  filter(ever_approved == "Yes", approved_2023 == "Restricted in 2023")
cat(nrow(oscillating), "of", sum(tier1_reclassified_up$ever_approved == "Yes"),
    "ever-approved stations ended 2023 back at Restricted\n")

# --- 6. SUMMARY TABLE BY CLASS AND TIER ------------------------------------
cat("\n=== RECLASSIFICATION FREQUENCY BY CLASS AND TIER ===\n")
reclass_summary <- fc_tiers %>%
  filter(!is.na(class_changes), !is.na(cur_class)) %>%
  mutate(
    cur_class_label = case_when(
      cur_class == "A" ~ "Approved",
      cur_class == "R" ~ "Restricted",
      cur_class == "P" ~ "Prohibited",
      TRUE ~ cur_class
    )
  ) %>%
  group_by(cur_class_label, tier) %>%
  summarise(
    n_stations    = n(),
    mean_changes  = round(mean(class_changes, na.rm = TRUE), 2),
    pct_no_change = round(sum(class_changes == 0) / n() * 100, 1),
    pct_changed   = round(sum(class_changes >= 1) / n() * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(cur_class_label, tier)

print(reclass_summary, n = 30)

# --- 7. WRITE OUTPUTS -------------------------------------------------------
write_csv(tier1_temporal,  "tier1_restricted_classification_history.csv")
write_csv(reclass_summary, "reclassification_frequency_summary.csv")
cat("\nFiles written.\n")

# =============================================================================
# THESIS INTERPRETATION:
#
# The oscillation pattern — areas briefly reclassified to Approved then
# returned to Restricted — demonstrates that SCDES possesses the regulatory
# mechanism to reclassify improving areas but is not applying it durably.
# This constitutes administrative burden in the form of regulatory inertia:
# growers cannot build economically viable operations on classifications that
# flip annually. The constraint is not ecological — it is governance.
#
# This finding supports Herd & Moynihan's (2019) administrative burden
# framework: compliance and learning costs are imposed on potential applicants
# through classification instability that rational economic actors cannot plan
# around, regardless of underlying water quality conditions.
# =============================================================================
