# =============================================================================
# mortality_ssi_precipitation_interaction.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 Validation
# Status      : FINAL
#
# PURPOSE
#   Interaction model testing whether rainfall affects low-SSI sites more
#   severely than high-SSI sites. Compares additive and interaction models by
#   ANOVA using the static 26-year composite SSI.
#
# INPUTS      : panel_rain (mortality + rainfall panel); static SSI
# OUTPUTS     : Interaction model output and figure
# RUN AFTER   : ssi_annual_means_regression.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# ── INTERACTION MODEL: Does rainfall hit low-SSI sites harder? ───────────────

# Use static SSI (26-year composite) — more interpretable than noisy annual
# panel_rain already has 'ssi' (static) from the val_map join

# Model 1: additive (no interaction)
M_add  <- lm(Mortality ~ ssi + mean_annual_precip, data = panel_rain)

# Model 2: with interaction
M_int  <- lm(Mortality ~ ssi * mean_annual_precip, data = panel_rain)

s_add  <- summary(M_add)
s_int  <- summary(M_int)

cat("=== ADDITIVE MODEL: Mortality ~ SSI + Precipitation ===\n")
cat(sprintf("R\u00b2 = %.1f%%\n", s_add$r.squared * 100))
print(round(s_add$coefficients, 5))

cat("\n=== INTERACTION MODEL: Mortality ~ SSI * Precipitation ===\n")
cat(sprintf("R\u00b2 = %.1f%%\n", s_int$r.squared * 100))
print(round(s_int$coefficients, 5))

# ANOVA test: does adding the interaction significantly improve the model?
cat("\n=== DOES THE INTERACTION TERM IMPROVE FIT? ===\n")
print(anova(M_add, M_int))

# ── VISUALISE THE INTERACTION ─────────────────────────────────────────────────
# Assign SSI class to panel_rain for grouping
panel_rain <- panel_rain %>%
  mutate(ssi_class_static = case_when(
    ssi >= 90 ~ "Excellent (SSI 90-100)",
    ssi >= 75 ~ "Good (SSI 75-89)",
    TRUE      ~ "Moderate (SSI 60-74)"
  ),
  ssi_class_static = factor(ssi_class_static,
                            levels = c("Moderate (SSI 60-74)",
                                       "Good (SSI 75-89)",
                                       "Excellent (SSI 90-100)")))

# Scatter: precipitation vs mortality, one line per SSI class
fig_int <- ggplot(panel_rain,
                  aes(x = mean_annual_precip, y = Mortality,
                      color = ssi_class_static)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  scale_color_manual(
    values = c("Moderate (SSI 60-74)"   = "#d73027",
               "Good (SSI 75-89)"       = "#f4a582",
               "Excellent (SSI 90-100)" = "#1a9850"),
    name = "Site SSI Class"
  ) +
  labs(
    title    = "Rainfall\u2013Mortality Relationship by SSI Class",
    subtitle = "If the interaction is real, the red line should be steepest",
    x        = "Mean Annual Coastal Precipitation (inches)",
    y        = "Annual Mortality (%)",
    caption  = "Each point = one site in one year.\nSteeper slope for Moderate (red) = low-SSI sites more sensitive to rainfall."
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(size = 8.5, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_ssi_precip_interaction.png"),
       fig_int, dpi = 300, width = 9, height = 6.5)

# ── PREDICTED VALUES SURFACE ──────────────────────────────────────────────────
# Show predicted mortality across a grid of SSI × precipitation values
grid <- expand.grid(
  ssi                = seq(65, 100, by = 1),
  mean_annual_precip = seq(38, 56,  by = 1)
)
grid$predicted_mortality <- predict(M_int, newdata = grid)

fig_surface <- ggplot(grid, aes(x = mean_annual_precip, y = ssi,
                                fill = predicted_mortality)) +
  geom_tile() +
  scale_fill_gradient2(low = "#1a9850", mid = "#fee08b", high = "#d73027",
                       midpoint = 7, name = "Predicted\nmortality (%)") +
  geom_vline(xintercept = 46.5, linetype = "dashed",
             color = "white", linewidth = 0.8) +
  annotate("text", x = 46.8, y = 98, label = "2020\n(Wet)",
           hjust = 0, color = "white", size = 3) +
  geom_vline(xintercept = 53.6, linetype = "dashed",
             color = "white", linewidth = 0.8) +
  annotate("text", x = 53.9, y = 98, label = "2015\n(Flood)",
           hjust = 0, color = "white", size = 3) +
  labs(
    title    = "Predicted Oyster Mortality: SSI \u00d7 Annual Precipitation",
    subtitle = "Dark red = highest predicted mortality risk",
    x        = "Mean Annual Coastal Precipitation (inches)",
    y        = "Static SSI Score",
    caption  = "Predictions from interaction model: Mortality ~ SSI \u00d7 Precipitation\nHorizontal bands show SSI site classes (bottom = Moderate, top = Excellent)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(size = 8.5, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_ssi_precip_interaction_surface.png"),
       fig_surface, dpi = 300, width = 9, height = 6.5)

cat("\nFigures saved.\n")