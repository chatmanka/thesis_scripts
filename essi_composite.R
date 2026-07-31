# =============================================================================
# essi_composite.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : ESSI — composite
# Used in     : Ch. 4 Methods; Ch. 5 Results
# Status      : FINAL — predates the July 2026 ESSI restructure; see reference doc
#
# PURPOSE
#   Builds the Enhanced Salinity Suitability Index composite score per station
#   from the SSI baseline and its additional dimensions.
#
# INPUTS      : SSI results; MK trend results; component layers
# OUTPUTS     : ESSI composite scores per station
# RUN AFTER   : ssi_calculate.R, ssi_annual_mannkendall.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# ENHANCED SALINITY SUITABILITY INDEX (ESSI) — COMPOSITE SCORE BUILDER
# =============================================================================
# Author:  Kate Chatman
# Date:    2026-06-07
# Project: "Closing the Governance Gap" — SC Shellfish Mariculture Thesis
#          MPA/EVSS Dual Degree, College of Charleston
#          SC Sea Grant Consortium Graduate Research Assistant
#
# PURPOSE:
#   Combine four analytical components into a single Enhanced Salinity
#   Suitability Index (ESSI) score per monitoring station. This is the
#   interpolation input for the UVVR cost-raster IPDW surface.
#
# ESSI COMPONENT WEIGHTS (provisional — sensitivity-tested below):
#   1. Base SSI score          50%  — 26-yr pooled salinity stability baseline
#   2. Mann-Kendall trend      20%  — trajectory (improving/stable/worsening)
#   3. Storm vulnerability     20%  — acute crash risk post-precipitation
#   4. Watershed impervious    10%  — land use pressure (R²=0.035 in SC system)
#
# INPUTS (all in ../data/ relative to script directory):
#   SSI_Results_with_coords.csv       — 582 stations; base score col = 'ssi'
#   MK_results_all_stations.csv       — 503 stations; trend col = 'mk_tau'
#   storm_vulnerability_map_data.csv  — 100 WQP rows; join key = 'scdes_station';
#                                       score col = 'storm_vulnerability_score'
#   watershed_analysis_full.csv       — 503 stations; col = 'mean_impervious'
#
# OUTPUTS:
#   essi_composite_scores.csv         — 503 stations, full ESSI + components
#   essi_sensitivity_summary.csv      — weight scenario correlations
#   essi_score_distribution.png       — histogram by ESSI class
#   essi_component_correlation.png    — pairplot of all components
#   essi_sensitivity_plot.png         — bar chart of sensitivity results
#
# STATION COUNT:
#   582 total SCDES → 503 ESSI-eligible (MK ≥10yr filter is binding)
#   79 excluded stations have no trend data; omitted from interpolation.
#
# JOIN NOTES:
#   - Storm file has 100 WQP rows → 79 unique SCDES matches → 65 overlap
#     with 503 MK stations. Remaining 438 stations receive neutral imputation
#     (score = 50) for storm component, documented as limitation.
#   - Area column sourced from MK results (SSI file has no Area column).
#   - 4 SSI null stations (rows 578-581) excluded via inner join on MK.
# =============================================================================

library(tidyverse)
library(GGally)

setwd("C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts")

# =============================================================================
# STEP 1: LOAD ALL FOUR COMPONENT FILES
# =============================================================================

cat("Loading component files...\n")

ssi_base <- read_csv("SSI_Results_with_coords.csv") %>%
  select(Station, LATITUDE, LONGITUDE,
         ssi, or_score, cv, extreme_event_score, stability_class)

mk_results <- read_csv("MK_results_all_stations.csv") %>%
  select(Station, Area, n_years,
         mean_ssi, mk_tau, mk_pvalue,
         trend_direction, trend_simple,
         LATITUDE, LONGITUDE)

storm_vuln <- read_csv("storm_vulnerability_map_data.csv") %>%
  select(scdes_station, storm_vulnerability_score, vulnerability_class,
         crash_rate_per_year, mean_crash_severity, pct_autumn_crashes) %>%
  group_by(scdes_station) %>%
  summarise(
    storm_vulnerability_score = mean(storm_vulnerability_score, na.rm = TRUE),
    vulnerability_class = first(vulnerability_class),
    crash_rate_per_year = mean(crash_rate_per_year, na.rm = TRUE),
    mean_crash_severity = mean(mean_crash_severity, na.rm = TRUE),
    pct_autumn_crashes  = mean(pct_autumn_crashes, na.rm = TRUE),
    .groups = "drop"
  )

watershed <- read_csv("watershed_analysis_full.csv") %>%
  select(Station, HUC12, mean_impervious)

cat("  SSI base:         ", nrow(ssi_base), "stations\n")
cat("  MK results:       ", nrow(mk_results), "stations\n")
cat("  Storm vuln:       ", nrow(storm_vuln), "unique SCDES stations\n")
cat("  Watershed:        ", nrow(watershed), "stations\n")

# =============================================================================
# STEP 2: JOIN — MK results are the spine (503 stations, binding filter)
# =============================================================================

cat("\nJoining components...\n")

essi_joined <- mk_results %>%
  # Base SSI — get the raw 'ssi' score and coords
  left_join(ssi_base %>% select(Station, ssi, or_score, cv, extreme_event_score),
            by = "Station") %>%
  # Storm vulnerability — join on Station = scdes_station
  left_join(storm_vuln,
            by = c("Station" = "scdes_station")) %>%
  # Watershed impervious
  left_join(watershed %>% select(Station, HUC12, mean_impervious),
            by = "Station")

cat("  Joined rows:                    ", nrow(essi_joined), "\n")
cat("  Missing storm_vulnerability_score:", sum(is.na(essi_joined$storm_vulnerability_score)), "\n")
cat("  Missing mean_impervious:         ", sum(is.na(essi_joined$mean_impervious)), "\n")
cat("  Missing ssi (base):              ", sum(is.na(essi_joined$ssi)), "\n")

# =============================================================================
# STEP 3: NORMALIZE EACH COMPONENT TO 0-100
# =============================================================================

cat("\nNormalizing components...\n")

normalize_0100 <- function(x, low_is_good = FALSE) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(50, length(x)))  # degenerate case
  normed <- (x - rng[1]) / (rng[2] - rng[1]) * 100
  if (low_is_good) normed <- 100 - normed
  return(normed)
}

essi_norm <- essi_joined %>%
  mutate(
    # Component 1: Base SSI — higher = better salinity stability
    comp1_base_ssi = normalize_0100(ssi, low_is_good = FALSE),

    # Component 2: MK trend — positive tau = improving = good
    # tau ranges -1 to +1 in theory; your data: -0.474 to +0.564
    comp2_mk_trend = normalize_0100(mk_tau, low_is_good = FALSE),

    # Component 3: Storm vulnerability — higher score = MORE vulnerable = BAD
    # Invert so high vulnerability → low ESSI contribution
    # Missing stations get neutral imputation = 50 (documented limitation)
    comp3_storm = case_when(
      is.na(storm_vulnerability_score) ~ 50,
      TRUE ~ 100 - normalize_0100(storm_vulnerability_score, low_is_good = FALSE)
    ),

    # Component 4: Watershed impervious — higher % = more pressure = BAD
    # Invert so high impervious → low ESSI contribution
    comp4_watershed = normalize_0100(mean_impervious, low_is_good = TRUE)
  )

cat("  comp1_base_ssi range:  ",
    round(min(essi_norm$comp1_base_ssi, na.rm=TRUE),1), "–",
    round(max(essi_norm$comp1_base_ssi, na.rm=TRUE),1), "\n")
cat("  comp2_mk_trend range:  ",
    round(min(essi_norm$comp2_mk_trend, na.rm=TRUE),1), "–",
    round(max(essi_norm$comp2_mk_trend, na.rm=TRUE),1), "\n")
cat("  comp3_storm range:     ",
    round(min(essi_norm$comp3_storm, na.rm=TRUE),1), "–",
    round(max(essi_norm$comp3_storm, na.rm=TRUE),1), "\n")
cat("  comp4_watershed range: ",
    round(min(essi_norm$comp4_watershed, na.rm=TRUE),1), "–",
    round(max(essi_norm$comp4_watershed, na.rm=TRUE),1), "\n")

# =============================================================================
# STEP 4: CALCULATE ESSI COMPOSITE SCORE
# =============================================================================

W1 <- 0.50  # Base SSI
W2 <- 0.20  # MK trend
W3 <- 0.20  # Storm vulnerability (inverted)
W4 <- 0.10  # Watershed impervious (inverted)

essi_final <- essi_norm %>%
  mutate(
    essi_score = (W1 * comp1_base_ssi) +
                 (W2 * comp2_mk_trend) +
                 (W3 * comp3_storm) +
                 (W4 * comp4_watershed),

    # Classify into 5 ordinal classes (provisional equal-interval breaks)
    # Will refine with Jenks natural breaks if distribution is skewed
    essi_class = case_when(
      essi_score >= 80 ~ "Excellent",
      essi_score >= 60 ~ "Good",
      essi_score >= 40 ~ "Moderate",
      essi_score >= 20 ~ "Poor",
      TRUE             ~ "Very Poor"
    ),
    essi_class = factor(essi_class,
                        levels = c("Very Poor","Poor","Moderate","Good","Excellent"))
  )

cat("\nESSI score summary:\n")
print(summary(essi_final$essi_score))
cat("\nESSI class distribution:\n")
print(table(essi_final$essi_class))

# =============================================================================
# STEP 5: SENSITIVITY ANALYSIS
# =============================================================================

cat("\nRunning sensitivity analysis...\n")

# Baseline score vector for correlation reference
baseline_scores <- with(essi_norm,
  0.50 * comp1_base_ssi +
  0.20 * comp2_mk_trend +
  0.20 * comp3_storm +
  0.10 * comp4_watershed
)

scenarios <- list(
  baseline     = c(0.50, 0.20, 0.20, 0.10),
  ssi_high     = c(0.60, 0.15, 0.15, 0.10),
  ssi_low      = c(0.40, 0.25, 0.25, 0.10),
  mk_high      = c(0.45, 0.30, 0.15, 0.10),
  mk_low       = c(0.55, 0.10, 0.25, 0.10),
  storm_high   = c(0.45, 0.15, 0.30, 0.10),
  storm_low    = c(0.55, 0.25, 0.10, 0.10),
  watershed_min= c(0.54, 0.22, 0.22, 0.02)
)

sensitivity_results <- map_dfr(names(scenarios), function(name) {
  w <- scenarios[[name]]
  scores <- with(essi_norm,
    w[1]*comp1_base_ssi + w[2]*comp2_mk_trend +
    w[3]*comp3_storm    + w[4]*comp4_watershed)
  tibble(
    scenario      = name,
    w1_ssi        = w[1], w2_mk = w[2],
    w3_storm      = w[3], w4_watershed = w[4],
    mean_score    = round(mean(scores, na.rm=TRUE), 2),
    sd_score      = round(sd(scores, na.rm=TRUE), 2),
    cor_baseline  = round(cor(scores, baseline_scores, use="complete.obs"), 4)
  )
})

cat("\nSensitivity results:\n")
print(sensitivity_results %>% select(scenario, w1_ssi, w2_mk, w3_storm,
                                      w4_watershed, mean_score, cor_baseline))

min_cor <- min(sensitivity_results$cor_baseline[sensitivity_results$scenario != "baseline"])
cat("\nMinimum correlation with baseline:", round(min_cor, 4), "\n")
if (min_cor >= 0.95) {
  cat("RESULT: Spatial pattern STABLE across weight perturbations (r ≥ 0.95)\n")
  cat("INTERPRETATION: Weighting scheme is defensible — map does not change\n")
  cat("meaningfully when weights shift ±10%. Suitable for committee defense.\n")
} else {
  cat("WARNING: Some scenarios show r < 0.95. Review which component drives\n")
  cat("instability and consider narrowing weight range or justifying further.\n")
}

# =============================================================================
# STEP 6: WRITE OUTPUTS
# =============================================================================

essi_output <- essi_final %>%
  select(
    Station, Area, LATITUDE, LONGITUDE,
    essi_score, essi_class,
    comp1_base_ssi, comp2_mk_trend, comp3_storm, comp4_watershed,
    ssi, mk_tau, mk_pvalue, trend_simple,
    storm_vulnerability_score, vulnerability_class,
    crash_rate_per_year,
    mean_impervious, HUC12,
    n_years
  ) %>%
  arrange(desc(essi_score))

write_csv(essi_output, "../data/essi_composite_scores.csv")
write_csv(sensitivity_results, "../data/essi_sensitivity_summary.csv")
cat("\nOutputs written.\n")

# =============================================================================
# STEP 7: FIGURES
# =============================================================================

# Figure 1: ESSI score distribution
p1 <- ggplot(essi_final, aes(x = essi_score, fill = essi_class)) +
  geom_histogram(bins = 40, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Excellent" = "#1a6b3c", "Good"     = "#4dac26",
    "Moderate"  = "#f7d03c", "Poor"     = "#d95f02",
    "Very Poor" = "#7b0000")) +
  labs(
    title    = "Enhanced Salinity Suitability Index (ESSI) — Score Distribution",
    subtitle = paste0("n=503 stations | Weights: Base SSI 50% / MK Trend 20% / Storm Vuln 20% / Watershed 10%"),
    x = "ESSI Score (0–100)", y = "Number of Stations", fill = "ESSI Class",
    caption = "Higher scores = more suitable for shellfish mariculture siting"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("../data/essi_score_distribution.png", p1, width=10, height=6, dpi=300)

# Figure 2: Component pairplot
comp_data <- essi_final %>%
  select(`Base SSI`   = comp1_base_ssi,
         `MK Trend`   = comp2_mk_trend,
         `Storm Vuln` = comp3_storm,
         `Watershed`  = comp4_watershed,
         `ESSI Score` = essi_score)

p2 <- ggpairs(comp_data,
  title = "ESSI Component Correlations (n=503 stations)",
  upper = list(continuous = wrap("cor", size = 3.5)),
  lower = list(continuous = wrap("points", alpha = 0.25, size = 0.7)),
  diag  = list(continuous = wrap("densityDiag"))) +
  theme_minimal(base_size = 10)

ggsave("../data/essi_component_correlation.png", p2, width=10, height=10, dpi=300)

# Figure 3: Sensitivity plot
p3 <- sensitivity_results %>%
  filter(scenario != "baseline") %>%
  mutate(scenario = fct_reorder(scenario, cor_baseline)) %>%
  ggplot(aes(x = scenario, y = cor_baseline,
             fill = cor_baseline >= 0.95)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0.95, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d73027"),
                    guide = "none") +
  scale_y_continuous(limits = c(0.88, 1.0),
                     breaks = seq(0.88, 1.0, 0.02)) +
  annotate("text", x = 0.7, y = 0.953, label = "r = 0.95 threshold",
           hjust = 0, size = 3.5, color = "red") +
  labs(
    title    = "ESSI Sensitivity Analysis — Correlation with Baseline Weighting",
    subtitle = "Blue bars (r ≥ 0.95) = spatial pattern stable; red bars = potentially sensitive",
    x = "Weight Scenario", y = "Pearson r with Baseline ESSI",
    caption = "Scenarios test ±10% perturbation on each component weight"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title  = element_text(face = "bold"))

ggsave("../data/essi_sensitivity_plot.png", p3, width=10, height=6, dpi=300)

cat("Figures written.\n")

# =============================================================================
# STEP 8: CONSOLE SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("  ESSI COMPOSITE — SESSION SUMMARY\n")
cat("================================================================\n")
cat("Stations with complete ESSI scores:", nrow(essi_output), "\n")
cat("Excluded (insufficient MK data):   ", 582 - nrow(essi_output), "\n")
cat("Storm component — imputed neutral: ",
    sum(is.na(essi_joined$storm_vulnerability_score)), "stations\n")
cat("\nESSI Score Range:",
    round(min(essi_output$essi_score),1), "–",
    round(max(essi_output$essi_score),1), "\n")
cat("Mean ESSI Score:", round(mean(essi_output$essi_score),1), "\n")
cat("\nClass distribution:\n")
print(table(essi_output$essi_class))
cat("\nTop 10 stations (ESSI):\n")
print(essi_output %>%
  select(Station, Area, essi_score, essi_class, trend_simple,
         vulnerability_class) %>% head(10), n=10)
cat("\nBottom 10 stations (ESSI):\n")
print(essi_output %>%
  select(Station, Area, essi_score, essi_class, trend_simple,
         vulnerability_class) %>% tail(10), n=10)
cat("\nSensitivity min r with baseline:",
    round(min_cor, 4), "\n")
cat("\nFiles written to ../data/:\n")
cat("  essi_composite_scores.csv\n")
cat("  essi_sensitivity_summary.csv\n")
cat("  essi_score_distribution.png\n")
cat("  essi_component_correlation.png\n")
cat("  essi_sensitivity_plot.png\n")
cat("================================================================\n")

