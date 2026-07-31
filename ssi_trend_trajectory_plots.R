# =============================================================================
# ssi_trend_trajectory_plots.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : SSI — temporal trend
# Used in     : Ch. 5 figures
# Status      : FINAL
#
# PURPOSE
#   Corrected trajectory plots for worsening and improving stations. Resolves
#   an Area column name collision that occurred when joining MK results to
#   annual scores.
#
# INPUTS      : MK_results_all_stations.csv; ssi_annual_scores.csv
# OUTPUTS     : Trajectory figures (PNG)
# RUN AFTER   : ssi_annual_mannkendall.R, ssi_trend_visualisations.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# FIXED: SSI Trajectory Plots for Worsening and Improving Stations
# Run after ssi_annual_mannkendall.R and mk_visualization.R
# Fix: Area column conflict resolved by renaming before join
# =============================================================================

library(tidyverse)
library(ggplot2)

ssi_annual <- read_csv("ssi_annual_scores.csv")
mk_results <- read_csv("MK_results_all_stations.csv")

# --- FIX: rename Area in mk_results before joining to avoid conflict ---------
mk_join <- mk_results %>%
  select(Station, mk_tau, mk_area = Area)   # rename Area to mk_area

# --- PLOT 2: Worsening station trajectories ----------------------------------
worsening_stations <- mk_results %>%
  filter(trend_simple == "Worsening") %>%
  arrange(mk_tau) %>%
  pull(Station)

worsening_data <- ssi_annual %>%
  filter(Station %in% worsening_stations) %>%
  left_join(mk_join, by = "Station") %>%
  mutate(label = paste0(Station, " (Area ", mk_area, ", τ=", round(mk_tau, 2), ")"))

p2 <- ggplot(worsening_data, aes(x = Year, y = ssi_annual, group = Station, color = label)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(aes(group = Station), method = "lm", se = FALSE,
              linetype = "dashed", linewidth = 0.5, alpha = 0.4) +
  scale_color_brewer(palette = "Set1", name = "Station") +
  labs(
    title = "Annual SSI Trajectories — Worsening Stations (p < 0.05)",
    subtitle = "9 stations with statistically significant declining salinity stability",
    x = "Year", y = "Annual SSI Score",
    caption = "Dashed lines = OLS trend for reference | Source: SCDES 1997–2023"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

ggsave("MK_worsening_trajectories.png", p2, width = 11, height = 6, dpi = 300)
cat("Plot 2 saved: MK_worsening_trajectories.png\n")

# --- PLOT 3: Improving station trajectories ----------------------------------
improving_stations <- mk_results %>%
  filter(trend_simple == "Improving") %>%
  arrange(desc(mk_tau)) %>%
  pull(Station)

improving_data <- ssi_annual %>%
  filter(Station %in% improving_stations) %>%
  left_join(mk_join, by = "Station") %>%
  mutate(label = paste0(Station, " (Area ", mk_area, ", τ=", round(mk_tau, 2), ")"))

p3 <- ggplot(improving_data, aes(x = Year, y = ssi_annual, group = Station, color = label)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(aes(group = Station), method = "lm", se = FALSE,
              linetype = "dashed", linewidth = 0.5, alpha = 0.4) +
  scale_color_brewer(palette = "Set2", name = "Station") +
  labs(
    title = "Annual SSI Trajectories — Improving Stations (p < 0.05)",
    subtitle = "10 stations with statistically significant improving salinity stability",
    x = "Year", y = "Annual SSI Score",
    caption = "Dashed lines = OLS trend for reference | Source: SCDES 1997–2023"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

ggsave("MK_improving_trajectories.png", p3, width = 11, height = 6, dpi = 300)
cat("Plot 3 saved: MK_improving_trajectories.png\n")

