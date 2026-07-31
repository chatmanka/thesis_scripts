# =============================================================================
# ssi_annual_means_regression.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 Validation
# Status      : FINAL
#
# PURPOSE
#   Builds the site-year panel dataset linking annual mortality to annual SSI
#   scores via the site-to-station spatial join, then fits the regression used
#   in biological validation.
#
# INPUTS      : Site-to-station mapping; annual mortality; ssi_annual_scores.csv
# OUTPUTS     : Panel dataset and regression output
# RUN AFTER   : ssi_annual_mannkendall.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

library(tidyverse)

# ── LOAD FILES ────────────────────────────────────────────────────────────────
# Site-to-station mapping from previous spatial join
val_map <- read_csv(
  "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data/ssi_mortality_validation_results.csv",
  show_col_types = FALSE
) %>%
  select(Site_code, nearest_station, distance_km, close_match)

# Annual mortality (raw, one row per site-year)
mortality_annual <- readxl::read_excel(
  "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data/databases/excel/MortalityData_SeaGrant_KC.xlsx",
  sheet = "Sheet2"
) %>%
  filter(EventSeason <= 2023)   # restrict to SSI temporal window

# Annual SSI scores
ssi_annual <- read_csv(
  "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts/ssi_annual_scores.csv",
  show_col_types = FALSE
)

# ── BUILD PANEL DATASET ───────────────────────────────────────────────────────
panel <- mortality_annual %>%
  # Attach the nearest SSI station to each sentinel site
  left_join(val_map, by = "Site_code") %>%
  # Join annual SSI by station + year
  left_join(
    ssi_annual %>% select(Station, Year, ssi_annual, n_obs,
                          or_score, cv_stability, extreme_event, duration),
    by = c("nearest_station" = "Station", "EventSeason" = "Year")
  ) %>%
  # Flag close spatial matches only
  filter(!is.na(ssi_annual))   # drops site-years with no SSI coverage

cat(sprintf("Panel observations: %d\n", nrow(panel)))
cat(sprintf("Sites: %d  |  Years: %d-%d\n",
            n_distinct(panel$Site_code),
            min(panel$EventSeason), max(panel$EventSeason)))
cat(sprintf("Mean n_obs per station-year: %.1f\n", mean(panel$n_obs, na.rm = TRUE)))

# ── REGRESSION 1: Pooled OLS ──────────────────────────────────────────────────
reg_pooled <- lm(Mortality ~ ssi_annual, data = panel)
s1 <- summary(reg_pooled)
cat("\n=== POOLED OLS: Mortality ~ annual SSI (n =", nrow(panel), ") ===\n")
cat(sprintf("Slope:   %.4f\n", coef(reg_pooled)[2]))
cat(sprintf("R\u00b2:      %.4f  (%.1f%% variance explained)\n",
            s1$r.squared, s1$r.squared * 100))
cat(sprintf("p-value: %.4f  %s\n", s1$coefficients[2,4],
            if (s1$coefficients[2,4] < 0.05) "*** SIGNIFICANT" else "ns"))

# ── REGRESSION 2: Site fixed effects ─────────────────────────────────────────
# Controls for time-invariant differences between sites (geography, disease
# pressure, predation). Asks: within a given site, do higher-SSI years
# have lower mortality?
reg_site_fe <- lm(Mortality ~ ssi_annual + Site_code, data = panel)
s2 <- summary(reg_site_fe)
cat("\n=== SITE FIXED EFFECTS: Mortality ~ annual SSI + site (n =", nrow(panel), ") ===\n")
cat(sprintf("Slope:   %.4f\n", coef(reg_site_fe)["ssi_annual"]))
cat(sprintf("R\u00b2:      %.4f  (%.1f%%)\n", s2$r.squared, s2$r.squared * 100))
cat(sprintf("p-value: %.4f  %s\n", s2$coefficients["ssi_annual", 4],
            if (s2$coefficients["ssi_annual", 4] < 0.05) "*** SIGNIFICANT" else "ns"))

# ── REGRESSION 3: Year fixed effects ─────────────────────────────────────────
# Controls for coast-wide annual events (e.g. 2015 flood affects all sites).
# Most conservative test of SSI-mortality relationship.
reg_year_fe <- lm(Mortality ~ ssi_annual + factor(EventSeason), data = panel)
s3 <- summary(reg_year_fe)
cat("\n=== YEAR FIXED EFFECTS: Mortality ~ annual SSI + year (n =", nrow(panel), ") ===\n")
cat(sprintf("Slope:   %.4f\n", coef(reg_year_fe)["ssi_annual"]))
cat(sprintf("R\u00b2:      %.4f  (%.1f%%)\n", s3$r.squared, s3$r.squared * 100))
cat(sprintf("p-value: %.4f  %s\n", s3$coefficients["ssi_annual", 4],
            if (s3$coefficients["ssi_annual", 4] < 0.05) "*** SIGNIFICANT" else "ns"))

# ── SCATTER PLOT ──────────────────────────────────────────────────────────────
FIG_DIR <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/figures"

ggplot(panel, aes(x = ssi_annual, y = Mortality)) +
  geom_point(aes(color = factor(EventSeason)), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray20", fill = "gray80", linewidth = 0.9) +
  annotate("text", x = min(panel$ssi_annual, na.rm=TRUE) + 1, 
           y = max(panel$Mortality, na.rm=TRUE) * 0.95,
           label = sprintf("R\u00b2 = %.2f  |  p = %.4f  |  n = %d site-years",
                           s1$r.squared, s1$coefficients[2,4], nrow(panel)),
           hjust = 0, size = 3.8, color = "gray20") +
  scale_color_viridis_d(name = "Year", option = "plasma") +
  labs(
    title    = "Annual SSI Score vs. Annual Oyster Mortality",
    subtitle = "Panel analysis: 29 sites \u00d7 9 years (2015\u20132023) | 261 site-year observations",
    x        = "Annual SSI Score (summer observations, May\u2013Sept)",
    y        = "Annual Mortality (%)",
    caption  = paste0("Each point = one site in one year. Annual SSI calculated from ~5 summer\n",
                      "salinity readings per station-year. Pooled OLS shown.")
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(size = 8, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_ssi_mortality_panel_scatter.png"),
       dpi = 300, width = 10, height = 6.5)

cat("\nFigure saved.\n")