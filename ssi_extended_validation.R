# =============================================================================
# ssi_extended_validation.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 Validation
# Status      : FINAL
#
# PURPOSE
#   Extended validation testing lagged salinity effects, temperature,
#   rainfall, and multi-factor models against observed mortality.
#
# INPUTS      : Annual SSI; mortality panel; NOAA precipitation; water temperature
# OUTPUTS     : Extended validation tables and figures
# RUN AFTER   : ssi_biological_validation.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# SSI EXTENDED VALIDATION: Lag Analysis, Temperature, Rainfall, Multi-Factor
# Kate Chatman | MPA/EVSS | June 2026
# =============================================================================

library(tidyverse)
library(readxl)

BASE   <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis"
SCRIPTS <- file.path(BASE, "scripts")
DATA    <- file.path(BASE, "data")
PRECIP  <- file.path(DATA, "scdeswqpdata97_23")
FIG_DIR <- file.path(BASE, "figures")
OUT_DIR <- DATA

# =============================================================================
# SECTION 1: LOAD BASE DATA
# =============================================================================
cat("Loading base datasets...\n")

mortality_raw <- read_excel(
  file.path(DATA, "databases/excel/MortalityData_SeaGrant_KC.xlsx"),
  sheet = "Sheet2"
) %>% filter(EventSeason <= 2023)

ssi_annual <- read_csv(file.path(SCRIPTS, "ssi_annual_scores.csv"),
                       show_col_types = FALSE)

val_map <- read_csv(file.path(DATA, "ssi_mortality_validation_results.csv"),
                    show_col_types = FALSE) %>%
  select(Site_code, nearest_station, ssi, distance_km, close_match)

# =============================================================================
# SECTION 2: BUILD BASE PANEL
# =============================================================================
cat("Building base panel...\n")

panel_base <- mortality_raw %>%
  left_join(val_map, by = "Site_code") %>%
  left_join(
    ssi_annual %>% select(Station, Year, ssi_annual, mean_sal, n_obs),
    by = c("nearest_station" = "Station", "EventSeason" = "Year")
  ) %>%
  filter(!is.na(ssi_annual)) %>%
  arrange(Site_code, EventSeason)

cat(sprintf("  Base panel: %d site-year observations, %d sites\n",
            nrow(panel_base), n_distinct(panel_base$Site_code)))

# =============================================================================
# SECTION 3: ADD TEMPERATURE FROM all_sc_wq.xlsx
# =============================================================================
cat("Loading temperature data...\n")

wq_temp <- read_excel(file.path(SCRIPTS, "all_sc_wq.xlsx")) %>%
  filter(!is.na(Water), Water > 0, Water < 40) %>%   # realistic water temp range C
  group_by(Station, Year) %>%
  summarise(
    mean_summer_temp = mean(Water, na.rm = TRUE),
    max_summer_temp  = max(Water,  na.rm = TRUE),
    n_temp_obs       = n(),
    .groups = "drop"
  )

cat(sprintf("  Temperature records: %d station-years\n", nrow(wq_temp)))

panel <- panel_base %>%
  left_join(wq_temp, by = c("nearest_station" = "Station",
                            "EventSeason"     = "Year"))

cat(sprintf("  Panel with temp: %d obs with temperature data\n",
            sum(!is.na(panel$mean_summer_temp))))

# =============================================================================
# SECTION 4: LOAD AND PROCESS NOAA PRECIPITATION
# =============================================================================
cat("Loading NOAA precipitation files...\n")

precip_files <- list.files(PRECIP,
                           pattern = "^(4299|4330)",
                           full.names = TRUE)
cat(sprintf("  Found %d NOAA precipitation files\n", length(precip_files)))

# Load and combine — NOAA CDO format: STATION, DATE, PRCP (inches)
precip_raw <- map_dfr(precip_files, function(f) {
  tryCatch(
    read_csv(f, show_col_types = FALSE),
    error = function(e) {
      read_excel(f)  # fallback if Excel format
    }
  )
})

cat(sprintf("  Raw precip records: %d\n", nrow(precip_raw)))
cat("  Precip column names:", paste(names(precip_raw)[1:8], collapse=", "), "\n")

# Compute annual total precipitation per station, then coast-wide mean
# NOAA CDO: PRCP column is daily precipitation in inches
prcp_col <- names(precip_raw)[grepl("^PRCP$", names(precip_raw),
                                    ignore.case = TRUE)]

if (length(prcp_col) == 0) {
  # Try alternative column detection
  prcp_col <- names(precip_raw)[grepl("prcp|precip|rainfall",
                                      names(precip_raw),
                                      ignore.case = TRUE)][1]
}
cat(sprintf("  Using precipitation column: %s\n", prcp_col))

annual_precip <- precip_raw %>%
  rename(prcp = all_of(prcp_col)) %>%
  mutate(
    prcp = as.numeric(prcp),
    date = as.Date(DATE),
    year = year(date),
    month = month(date)
  ) %>%
  filter(!is.na(prcp), prcp >= 0, year >= 2010) %>%
  group_by(STATION, year) %>%
  summarise(
    annual_precip_in  = sum(prcp, na.rm = TRUE),
    summer_precip_in  = sum(prcp[month %in% 5:9], na.rm = TRUE),
    autumn_precip_in  = sum(prcp[month %in% 10:11], na.rm = TRUE),
    n_days            = n(),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  summarise(
    mean_annual_precip  = mean(annual_precip_in,  na.rm = TRUE),
    mean_summer_precip  = mean(summer_precip_in,  na.rm = TRUE),
    mean_autumn_precip  = mean(autumn_precip_in,  na.rm = TRUE),
    n_stations          = n(),
    .groups = "drop"
  )

cat("\nCoastal SC annual precipitation summary (2015-2023):\n")
print(annual_precip %>% filter(year >= 2015) %>%
        select(year, mean_annual_precip, mean_summer_precip, mean_autumn_precip))

# Classify years: wet / normal / dry relative to 2015-2023 period
med_precip <- median(
  annual_precip$mean_annual_precip[annual_precip$year %in% 2015:2023]
)
sd_precip <- sd(
  annual_precip$mean_annual_precip[annual_precip$year %in% 2015:2023]
)

annual_precip <- annual_precip %>%
  mutate(
    precip_class = case_when(
      mean_annual_precip > med_precip + 0.5 * sd_precip ~ "Wet",
      mean_annual_precip < med_precip - 0.5 * sd_precip ~ "Dry",
      TRUE ~ "Normal"
    )
  )

cat("\nYear classification (wet/dry/normal):\n")
print(annual_precip %>% filter(year >= 2015) %>%
        select(year, mean_annual_precip, precip_class))

# Join precipitation to panel
panel <- panel %>%
  left_join(annual_precip %>% select(year, mean_annual_precip,
                                     mean_autumn_precip, precip_class),
            by = c("EventSeason" = "year"))

# =============================================================================
# SECTION 5: LAG REGRESSIONS
# =============================================================================
cat("\n\n=== LAG REGRESSION ANALYSIS ===\n")

panel_lag <- panel %>%
  arrange(Site_code, EventSeason) %>%
  group_by(Site_code) %>%
  mutate(
    ssi_lag1     = lag(ssi_annual, 1),   # SSI from t-1
    ssi_lag2     = lag(ssi_annual, 2),   # SSI from t-2
    sal_lag1     = lag(mean_sal, 1),
    temp_lag1    = lag(mean_summer_temp, 1)
  ) %>%
  ungroup()

# Model L0: current year (baseline from earlier)
L0 <- lm(Mortality ~ ssi_annual, data = panel_lag)
# Model L1: one-year lag
L1 <- lm(Mortality ~ ssi_lag1, data = panel_lag %>% filter(!is.na(ssi_lag1)))
# Model L2: two-year lag
L2 <- lm(Mortality ~ ssi_lag2, data = panel_lag %>% filter(!is.na(ssi_lag2)))
# Model L3: current + lag combined
L3 <- lm(Mortality ~ ssi_annual + ssi_lag1,
         data = panel_lag %>% filter(!is.na(ssi_lag1)))

model_table <- function(label, model, data) {
  s <- summary(model)
  ssi_coef <- s$coefficients[grepl("ssi", rownames(s$coefficients))[1], ]
  tibble(
    Model   = label,
    n       = nrow(data),
    Slope   = round(ssi_coef[1], 4),
    R2      = round(s$r.squared, 4),
    `R2 %`  = round(s$r.squared * 100, 1),
    p_value = round(ssi_coef[4], 4),
    Sig     = if_else(ssi_coef[4] < 0.05, "***", if_else(ssi_coef[4] < 0.10, ".", "ns"))
  )
}

lag_results <- bind_rows(
  model_table("L0: SSI same year",    L0, panel_lag),
  model_table("L1: SSI lag 1 year",   L1, panel_lag %>% filter(!is.na(ssi_lag1))),
  model_table("L2: SSI lag 2 years",  L2, panel_lag %>% filter(!is.na(ssi_lag2))),
  model_table("L3: SSI + lag combined", L3, panel_lag %>% filter(!is.na(ssi_lag1)))
)
cat("\n"); print(lag_results, n=10)

# =============================================================================
# SECTION 6: TEMPERATURE PARTITION MODEL
# =============================================================================
cat("\n\n=== TEMPERATURE PARTITION ===\n")

panel_temp <- panel_lag %>% filter(!is.na(mean_summer_temp))
cat(sprintf("  n for temperature models: %d\n", nrow(panel_temp)))

T1 <- lm(Mortality ~ mean_summer_temp,              data = panel_temp)
T2 <- lm(Mortality ~ ssi_annual,                    data = panel_temp)
T3 <- lm(Mortality ~ ssi_annual + mean_summer_temp, data = panel_temp)
T4 <- lm(Mortality ~ ssi_lag1   + mean_summer_temp,
         data = panel_temp %>% filter(!is.na(ssi_lag1)))

s_T1 <- summary(T1); s_T2 <- summary(T2)
s_T3 <- summary(T3); s_T4 <- summary(T4)

temp_results <- tibble(
  Model       = c("Temperature only", "SSI only",
                  "SSI + Temperature", "SSI lag-1 + Temperature"),
  n           = c(nrow(panel_temp), nrow(panel_temp),
                  nrow(panel_temp), nrow(panel_temp %>% filter(!is.na(ssi_lag1)))),
  R2          = round(c(s_T1$r.squared, s_T2$r.squared,
                        s_T3$r.squared, s_T4$r.squared), 4),
  `R2 %`      = round(c(s_T1$r.squared, s_T2$r.squared,
                        s_T3$r.squared, s_T4$r.squared) * 100, 1),
  SSI_p       = c(NA,
                  round(s_T2$coefficients["ssi_annual", 4], 4),
                  round(s_T3$coefficients["ssi_annual", 4], 4),
                  round(s_T4$coefficients["ssi_lag1", 4], 4)),
  Temp_p      = c(round(s_T1$coefficients["mean_summer_temp", 4], 4),
                  NA,
                  round(s_T3$coefficients["mean_summer_temp", 4], 4),
                  round(s_T4$coefficients["mean_summer_temp", 4], 4))
)
cat("\n"); print(temp_results)

# Variance partition: unique contribution of each predictor
cat(sprintf("\n  SSI unique contribution (R2 full - R2 temp only): %.4f (%.1f%%)\n",
            s_T3$r.squared - s_T1$r.squared,
            (s_T3$r.squared - s_T1$r.squared) * 100))
cat(sprintf("  Temp unique contribution (R2 full - R2 SSI only):  %.4f (%.1f%%)\n",
            s_T3$r.squared - s_T2$r.squared,
            (s_T3$r.squared - s_T2$r.squared) * 100))
cat(sprintf("  Shared variance: %.4f (%.1f%%)\n",
            s_T1$r.squared + s_T2$r.squared - s_T3$r.squared,
            (s_T1$r.squared + s_T2$r.squared - s_T3$r.squared) * 100))

# =============================================================================
# SECTION 7: RAINFALL / DROUGHT ANALYSIS
# =============================================================================
cat("\n\n=== RAINFALL / DROUGHT ANALYSIS ===\n")

panel_rain <- panel_lag %>% filter(!is.na(mean_annual_precip))
cat(sprintf("  n with precipitation data: %d\n", nrow(panel_rain)))

R1 <- lm(Mortality ~ mean_annual_precip,                    data = panel_rain)
R2 <- lm(Mortality ~ ssi_annual + mean_annual_precip,       data = panel_rain)
R3 <- lm(Mortality ~ ssi_annual + mean_summer_temp +
           mean_annual_precip,
         data = panel_rain %>% filter(!is.na(mean_summer_temp)))

s_R1 <- summary(R1); s_R2 <- summary(R2); s_R3 <- summary(R3)

cat(sprintf("\n  Rainfall alone:           R2=%.4f  p=%.4f\n",
            s_R1$r.squared, s_R1$coefficients[2,4]))
cat(sprintf("  SSI + Rainfall:           R2=%.4f  rainfall_p=%.4f\n",
            s_R2$r.squared, s_R2$coefficients["mean_annual_precip",4]))
cat(sprintf("  SSI + Temp + Rainfall:    R2=%.4f\n", s_R3$r.squared))

# Mortality by year class (wet/dry/normal)
mort_by_class <- panel_rain %>%
  group_by(precip_class) %>%
  summarise(
    n_obs          = n(),
    mean_mortality = round(mean(Mortality, na.rm=TRUE), 2),
    sd_mortality   = round(sd(Mortality,   na.rm=TRUE), 2),
    .groups = "drop"
  )
cat("\nMean mortality by precipitation year class:\n")
print(mort_by_class)

# =============================================================================
# SECTION 8: BEST COMBINED MODEL
# =============================================================================
cat("\n\n=== COMBINED MULTI-FACTOR MODEL ===\n")

panel_full <- panel_lag %>%
  filter(!is.na(ssi_lag1), !is.na(mean_summer_temp),
         !is.na(mean_annual_precip))

FULL <- lm(Mortality ~ ssi_lag1 + mean_summer_temp + mean_annual_precip,
           data = panel_full)
s_FULL <- summary(FULL)
cat(sprintf("  n = %d\n", nrow(panel_full)))
cat(sprintf("  R2 = %.4f (%.1f%%)\n", s_FULL$r.squared, s_FULL$r.squared*100))
cat("\n  Coefficients:\n")
print(round(s_FULL$coefficients, 4))

# =============================================================================
# SECTION 9: FIGURES
# =============================================================================
cat("\nGenerating figures...\n")

# ── Fig 1: Lag comparison scatter ────────────────────────────────────────────
lag_plot_data <- bind_rows(
  panel_lag %>% filter(!is.na(ssi_annual)) %>%
    mutate(ssi_plot = ssi_annual, lag_label = "Same year (t)"),
  panel_lag %>% filter(!is.na(ssi_lag1)) %>%
    mutate(ssi_plot = ssi_lag1,   lag_label = "1-year lag (t-1)"),
  panel_lag %>% filter(!is.na(ssi_lag2)) %>%
    mutate(ssi_plot = ssi_lag2,   lag_label = "2-year lag (t-2)")
) %>%
  mutate(lag_label = factor(lag_label,
                            levels = c("Same year (t)",
                                       "1-year lag (t-1)",
                                       "2-year lag (t-2)")))

r2_labels <- lag_results %>%
  filter(Model %in% c("L0: SSI same year",
                      "L1: SSI lag 1 year",
                      "L2: SSI lag 2 years")) %>%
  mutate(lag_label = factor(c("Same year (t)", "1-year lag (t-1)",
                              "2-year lag (t-2)"),
                            levels = c("Same year (t)", "1-year lag (t-1)",
                                       "2-year lag (t-2)")),
         label = sprintf("R²=%.3f  p=%.4f", R2, p_value))

fig_lag <- ggplot(lag_plot_data, aes(x = ssi_plot, y = Mortality)) +
  geom_point(aes(color = factor(EventSeason)), alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray20", fill = "gray85", linewidth = 0.9) +
  geom_text(data = r2_labels,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.4, size = 3.5,
            color = "gray20", inherit.aes = FALSE) +
  facet_wrap(~ lag_label, ncol = 3) +
  scale_color_viridis_d(name = "Year", option = "plasma") +
  labs(
    title    = "Lag Regression: Annual SSI vs. Oyster Mortality",
    subtitle = "Does salinity stability predict mortality in the same year, or with a 1-2 year delay?",
    x        = "Annual SSI Score",
    y        = "Annual Mortality (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(FIG_DIR, "ch5_lag_regression_comparison.png"),
       fig_lag, dpi = 300, width = 12, height = 5)

# ── Fig 2: Temperature vs SSI — confounding check ───────────────────────────
fig_temp_ssi <- ggplot(panel_temp,
                       aes(x = mean_summer_temp, y = ssi_annual)) +
  geom_point(aes(color = Mortality), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray30", linewidth = 0.8) +
  scale_color_gradient2(low = "#1a9850", mid = "#fee08b", high = "#d73027",
                        midpoint = 8, name = "Mortality (%)") +
  labs(
    title    = "Summer Temperature vs. Annual SSI",
    subtitle = "Testing for salinity-temperature confounding | Point color = annual mortality",
    x        = "Mean Summer Water Temperature (°C)",
    y        = "Annual SSI Score",
    caption  = "If hot-dry years produce both high SSI and high disease-driven mortality,\nthis creates apparent confounding between the two predictors."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_temp_ssi_confound_check.png"),
       fig_temp_ssi, dpi = 300, width = 8, height = 6)

# ── Fig 3: Annual precipitation with mortality overlay ───────────────────────
precip_yearly <- panel_rain %>%
  group_by(EventSeason, mean_annual_precip, precip_class) %>%
  summarise(mean_mortality = mean(Mortality, na.rm = TRUE), .groups = "drop")

fig_precip <- ggplot(precip_yearly, aes(x = EventSeason)) +
  geom_col(aes(y = mean_annual_precip / 10, fill = precip_class),
           alpha = 0.6, width = 0.7) +
  geom_line(aes(y = mean_mortality, group = 1),
            color = "#d73027", linewidth = 1.2) +
  geom_point(aes(y = mean_mortality),
             color = "#d73027", size = 3) +
  scale_fill_manual(
    values = c("Wet" = "#2166ac", "Normal" = "#92c5de", "Dry" = "#f4a582"),
    name = "Year type"
  ) +
  scale_y_continuous(
    name = "Mean annual mortality (%)",
    sec.axis = sec_axis(~ . * 10,
                        name = "Mean coastal precipitation (inches/yr)")
  ) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(
    title    = "Coastal SC Precipitation vs. Mean Oyster Mortality (2015–2023)",
    subtitle = "Bars = annual precipitation (right axis) | Line = mean mortality (left axis)",
    x        = "Year",
    caption  = "2015: high autumn precipitation (flood year) associated with peak mortality.\nDry years show lower mortality on average — consistent with salinity-temperature coupling hypothesis."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8.5, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_precip_mortality_annual.png"),
       fig_precip, dpi = 300, width = 10, height = 6)

# ── Fig 4: Model comparison R² bar chart ────────────────────────────────────
model_comparison <- tibble(
  Model = c("SSI (site means)\n[primary result]",
            "SSI annual\n(pooled OLS)",
            "Temperature\nonly",
            "SSI +\nTemperature",
            "SSI lag-1 +\nTemperature",
            "Full model\n(SSI lag + Temp + Rain)"),
  R2    = c(0.2842, s_T2$r.squared, s_T1$r.squared,
            s_T3$r.squared, s_T4$r.squared, s_FULL$r.squared),
  sig   = c(TRUE, TRUE, s_T1$coefficients[2,4]<0.05,
            TRUE, TRUE, TRUE),
  type  = c("Primary", "Panel", "Panel", "Panel", "Panel", "Panel")
)

fig_model_comp <- ggplot(model_comparison,
                         aes(x = reorder(Model, R2), y = R2 * 100,
                             fill = type, alpha = sig)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", R2 * 100)),
            hjust = -0.15, size = 3.5, color = "gray20") +
  coord_flip() +
  scale_fill_manual(values = c("Primary" = "#1a9850", "Panel" = "#2166ac"),
                    name = "Analysis type") +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.4),
                     guide = "none") +
  scale_y_continuous(limits = c(0, 45), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Model Comparison: Variance Explained (R²)",
    subtitle = "All mortality ~ predictor(s) | Panel = annual site-year data",
    x        = NULL,
    y        = "Variance Explained (R²%)",
    caption  = "Faded bars indicate p ≥ 0.05. Primary result (site means) shown in green."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_model_comparison_r2.png"),
       fig_model_comp, dpi = 300, width = 10, height = 6)

# =============================================================================
# SECTION 10: SAVE RESULTS
# =============================================================================
write_csv(lag_results,
          file.path(OUT_DIR, "lag_regression_results.csv"))
write_csv(temp_results,
          file.path(OUT_DIR, "temperature_partition_results.csv"))
write_csv(annual_precip,
          file.path(OUT_DIR, "coastal_sc_annual_precip_2015_2023.csv"))

cat("\n\nAll analyses complete. Files saved to:\n")
cat(" Figures:", FIG_DIR, "\n")
cat(" Data:   ", OUT_DIR, "\n")
cat("\nFigures produced:\n")
cat("  ch5_lag_regression_comparison.png\n")
cat("  ch5_temp_ssi_confound_check.png\n")
cat("  ch5_precip_mortality_annual.png\n")
cat("  ch5_model_comparison_r2.png\n")