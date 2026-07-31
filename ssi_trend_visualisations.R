# =============================================================================
# ssi_trend_visualisations.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : SSI — temporal trend
# Used in     : Ch. 5 figures
# Status      : FINAL
#
# PURPOSE
#   Plots annual SSI trajectories for worsening and improving stations to
#   confirm Mann-Kendall results are real rather than data artifacts; produces
#   the tau distribution figure; flags the Areas 17-19 convergence for cross-
#   dataset validation.
#
# INPUTS      : ssi_annual_scores.csv; MK_results_all_stations.csv
# OUTPUTS     : MK trajectory and tau distribution figures (PNG)
# RUN AFTER   : ssi_annual_mannkendall.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# MK Results Visualization + Validation Check
# Run AFTER ssi_annual_mannkendall.R
# Kate Chatman | MPA/EVSS Thesis | May 2026
#
# PURPOSE:
#   1. Plot annual SSI trajectories for worsening/improving stations
#      to visually confirm MK results are real (not data artifacts)
#   2. Produce a tau distribution plot for thesis Figure X
#   3. Flag the Areas 17-19 convergence for cross-dataset validation
# =============================================================================

library(tidyverse)
library(ggplot2)

# Load outputs from previous script
ssi_annual  <- read_csv("ssi_annual_scores.csv")
mk_results  <- read_csv("MK_results_all_stations.csv")
mk_area     <- read_csv("MK_area_summary.csv")

# --- PLOT 1: Tau distribution across all 503 stations -----------------------
# Shows the overall pattern: most near zero (stable), tails = trends
p1 <- ggplot(mk_results, aes(x = mk_tau)) +
  geom_histogram(binwidth = 0.05, fill = "#2E86AB", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = -0.55, y = 55, label = "Worsening →\n(negative tau)", 
           hjust = 0, size = 3.5, color = "#C0392B") +
  annotate("text", x = 0.55, y = 55, label = "← Improving\n(positive tau)", 
           hjust = 1, size = 3.5, color = "#27AE60") +
  labs(
    title = "Distribution of Mann-Kendall Tau Values Across 503 SCDES Stations",
    subtitle = "Negative tau = declining salinity stability | Positive tau = improving stability",
    x = "Mann-Kendall Tau (τ)",
    y = "Number of Stations",
    caption = "Source: SCDES Site Selection Tool 1997–2023 | Analysis: Chatman 2026"
  ) +
  theme_minimal(base_size = 12)

ggsave("MK_tau_distribution.png", p1, width = 9, height = 5, dpi = 300)
cat("Plot 1 saved: MK_tau_distribution.png\n")

# --- PLOT 2: Annual SSI trajectories for WORSENING stations -----------------
# Visual confirmation that the trend is real, not a single bad year
worsening_stations <- mk_results %>%
  filter(trend_simple == "Worsening") %>%
  arrange(mk_tau) %>%          # most negative first
  pull(Station)

worsening_data <- ssi_annual %>%
  filter(Station %in% worsening_stations) %>%
  left_join(mk_results %>% select(Station, mk_tau, Area), by = "Station") %>%
  mutate(label = paste0(Station, " (Area ", Area, ", τ=", round(mk_tau,2), ")"))

p2 <- ggplot(worsening_data, aes(x = Year, y = ssi_annual, group = Station, color = label)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(aes(group = Station), method = "lm", se = FALSE, 
              linetype = "dashed", linewidth = 0.5, alpha = 0.4) +
  scale_color_brewer(palette = "Set1", name = "Station") +
  labs(
    title = "Annual SSI Trajectories — Worsening Stations (p < 0.05)",
    subtitle = "9 stations with statistically significant declining salinity stability",
    x = "Year",
    y = "Annual SSI Score",
    caption = "Dashed lines = OLS trend for reference | Source: SCDES 1997–2023"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

ggsave("MK_worsening_trajectories.png", p2, width = 11, height = 6, dpi = 300)
cat("Plot 2 saved: MK_worsening_trajectories.png\n")

# --- PLOT 3: Annual SSI trajectories for IMPROVING stations -----------------
improving_stations <- mk_results %>%
  filter(trend_simple == "Improving") %>%
  arrange(desc(mk_tau)) %>%
  pull(Station)

improving_data <- ssi_annual %>%
  filter(Station %in% improving_stations) %>%
  left_join(mk_results %>% select(Station, mk_tau, Area), by = "Station") %>%
  mutate(label = paste0(Station, " (Area ", Area, ", τ=", round(mk_tau,2), ")"))

p3 <- ggplot(improving_data, aes(x = Year, y = ssi_annual, group = Station, color = label)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(aes(group = Station), method = "lm", se = FALSE,
              linetype = "dashed", linewidth = 0.5, alpha = 0.4) +
  scale_color_brewer(palette = "Set2", name = "Station") +
  labs(
    title = "Annual SSI Trajectories — Improving Stations (p < 0.05)",
    subtitle = "10 stations with statistically significant improving salinity stability",
    x = "Year",
    y = "Annual SSI Score",
    caption = "Dashed lines = OLS trend for reference | Source: SCDES 1997–2023"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

ggsave("MK_improving_trajectories.png", p3, width = 11, height = 6, dpi = 300)
cat("Plot 3 saved: MK_improving_trajectories.png\n")

# --- PLOT 4: Area-level summary bar chart -----------------------------------
# Shows which management areas have the highest % worsening — thesis Figure
# Filter to areas with at least 5 stations for meaningful percentages
mk_area_plot <- mk_area %>%
  filter(n_stations >= 5) %>%
  mutate(Area = fct_reorder(as.character(Area), pct_worsening))

p4 <- ggplot(mk_area_plot, aes(x = Area)) +
  geom_col(aes(y = pct_worsening), fill = "#C0392B", alpha = 0.8, 
           position = "identity", width = 0.6) +
  geom_col(aes(y = -pct_improving), fill = "#27AE60", alpha = 0.8,
           position = "identity", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),
    breaks = seq(-15, 20, 5)
  ) +
  labs(
    title = "Significant MK Trends by SCDES Shellfish Management Area",
    subtitle = "Red = % stations worsening (p<0.05) | Green = % stations improving (p<0.05)",
    x = "Management Area",
    y = "Percentage of Stations with Significant Trend",
    caption = "Areas with <5 qualifying stations excluded | Source: SCDES 1997–2023"
  ) +
  annotate("text", x = Inf, y = 12, label = "Worsening →", 
           hjust = 1, vjust = 1.5, color = "#C0392B", size = 3.5) +
  annotate("text", x = Inf, y = -8, label = "← Improving",
           hjust = 0, vjust = 1.5, color = "#27AE60", size = 3.5) +
  theme_minimal(base_size = 12)

ggsave("MK_area_summary_chart.png", p4, width = 9, height = 7, dpi = 300)
cat("Plot 4 saved: MK_area_summary_chart.png\n")

# --- CONSOLE: Cross-dataset validation check --------------------------------
cat("\n===== CROSS-DATASET VALIDATION CHECK =====\n")
cat("Worsening stations by area (for comparison with SCDES CUR_CLASS high-change polygons):\n")
mk_results %>%
  filter(trend_simple == "Worsening") %>%
  count(Area, name = "n_worsening") %>%
  arrange(desc(n_worsening)) %>%
  print()

cat("\nExpected: Areas 17, 19 should dominate — this matches the CUR_CLASS\n")
cat("high class_changes polygons identified in your SCDES spatial layer.\n")
cat("If confirmed in ArcGIS, this is your cross-dataset convergence validation.\n")

cat("\n===== KEY THESIS NUMBERS (Chapter 5) =====\n")
cat("Total stations analyzed:", nrow(mk_results), "\n")
cat("Stable (no significant trend, p >= 0.05):", sum(mk_results$trend_simple == "Stable"), 
    "(", round(mean(mk_results$trend_simple == "Stable")*100, 1), "%)\n")
cat("Worsening (significant, p < 0.05):", sum(mk_results$trend_simple == "Worsening"),
    "(", round(mean(mk_results$trend_simple == "Worsening")*100, 1), "%)\n")
cat("Improving (significant, p < 0.05):", sum(mk_results$trend_simple == "Improving"),
    "(", round(mean(mk_results$trend_simple == "Improving")*100, 1), "%)\n")
cat("\nPrimary worsening area: Area 19 (4 of 9 worsening stations)\n")
cat("Primary improving area: Area 8/9A cluster\n")

