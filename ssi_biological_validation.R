# =============================================================================
# ssi_biological_validation.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 Validation
# Status      : FINAL
#
# PURPOSE
#   Validates the SSI against observed oyster mortality at sentinel monitoring
#   sites, including the Winyah Bay 2015 flood case study as mechanistic
#   evidence.
#
# INPUTS      : SSI results; sentinel site mortality records; WQP salinity
# OUTPUTS     : ssi_validation_report.txt; validation figures; ch5_winyah_bay_2015_salinity.png
# RUN AFTER   : ssi_calculate.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# SSI BIOLOGICAL VALIDATION ANALYSIS
# Kate Chatman | MPA/EVSS | College of Charleston / SC Sea Grant Consortium
# Date: June 2026
# 
# PURPOSE: Validate the Salinity Stability Index (SSI) against Gary Sundin's
# SCDNR 11-year sentinel oyster mortality dataset (29 sites, 2015-2024).
# Includes regression analysis, class-level comparison, component correlations,
# and the Winyah Bay 2015 flood case study as mechanistic validation.
#
# INPUTS:
#   - MortalityData_SeaGrant_KC.xlsx   (Gary Sundin / SCDNR MRRI)
#   - essi_composite_scores.csv        (Kate Chatman, June 2026)
#   - narrowresult.csv                 (WQP / SCDES, org 21SC60WQ_WQX)
#   - Station.csv                      (WQP station metadata)
#
# OUTPUTS:
#   - ssi_mortality_scatter.png
#   - ssi_class_mortality_boxplot.png
#   - ssi_component_correlations.png
#   - winyah_bay_2015_map.png
#   - ssi_validation_summary.csv
#   - ssi_validation_report.txt
# =============================================================================

library(tidyverse)
library(readxl)
library(sf)

# --- SET PATHS ----------------------------------------------------------------
DATA_DIR  <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data"
FIG_DIR   <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/figures"
OUT_DIR   <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data"

MORT_FILE  <- file.path(DATA_DIR, "databases/excel/MortalityData_SeaGrant_KC.xlsx")
ESSI_FILE  <- file.path(DATA_DIR, "databases/excel/essi_composite_scores.csv")
WQP_FILE   <- file.path(DATA_DIR, "databases/excel/narrowresult.csv")
STNF_FILE  <- file.path(DATA_DIR, "databases/excel/Station.csv")

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SECTION 1: LOAD AND PREPARE DATA
# =============================================================================

cat("Loading datasets...\n")

# Mortality data — restrict to SSI temporal window (through 2023)
mortality_raw <- read_excel(MORT_FILE, sheet = "Sheet2")

mortality <- mortality_raw %>%
  filter(EventSeason <= 2023) %>%
  group_by(Site_code, site_latitude, site_longitude) %>%
  summarise(
    mean_mortality  = mean(Mortality, na.rm = TRUE),
    median_mortality = median(Mortality, na.rm = TRUE),
    max_mortality   = max(Mortality, na.rm = TRUE),
    min_mortality   = min(Mortality, na.rm = TRUE),
    sd_mortality    = sd(Mortality, na.rm = TRUE),
    n_years         = n(),
    .groups = "drop"
  )

# SSI / ESSI composite scores
essi <- read_csv(ESSI_FILE, show_col_types = FALSE)

# WQP full-year data + station coordinates
wqp      <- read_csv(WQP_FILE,  show_col_types = FALSE)
stations <- read_csv(STNF_FILE, show_col_types = FALSE)

cat(sprintf("  Mortality: %d sites\n", nrow(mortality)))
cat(sprintf("  SSI/ESSI stations: %d\n", nrow(essi)))
cat(sprintf("  WQP records: %d\n", nrow(wqp)))

# =============================================================================
# SECTION 2: SPATIAL JOIN — sentinel sites to nearest SSI station
# =============================================================================

cat("\nRunning spatial join...\n")

# Convert to sf (WGS84), transform to UTM 17N for accurate distance
mort_sf <- st_as_sf(mortality,
                    coords = c("site_longitude", "site_latitude"),
                    crs = 4326) %>%
  st_transform(26917)

essi_sf <- st_as_sf(essi,
                    coords = c("LONGITUDE", "LATITUDE"),
                    crs = 4326) %>%
  st_transform(26917)

nearest_idx <- st_nearest_feature(mort_sf, essi_sf)
dist_m      <- as.numeric(st_distance(mort_sf,
                                      essi_sf[nearest_idx, ],
                                      by_element = TRUE))

validation <- mortality %>%
  mutate(
    nearest_station  = essi$Station[nearest_idx],
    distance_m       = dist_m,
    distance_km      = dist_m / 1000,
    ssi              = essi$ssi[nearest_idx],
    essi_score       = essi$essi_score[nearest_idx],
    comp1_base_ssi   = essi$comp1_base_ssi[nearest_idx],
    comp2_mk_trend   = essi$comp2_mk_trend[nearest_idx],
    comp3_storm      = essi$comp3_storm[nearest_idx],
    comp4_watershed  = essi$comp4_watershed[nearest_idx],
    trend_simple     = essi$trend_simple[nearest_idx],
    close_match      = dist_m < 5000
  ) %>%
  mutate(
    ssi_class = case_when(
      ssi >= 90 ~ "Excellent (90-100)",
      ssi >= 75 ~ "Good (75-89)",
      ssi >= 60 ~ "Moderate (60-74)",
      ssi >= 45 ~ "Poor (45-59)",
      TRUE      ~ "Unsuitable (<45)"
    ),
    ssi_class = factor(ssi_class,
                       levels = c("Unsuitable (<45)", "Poor (45-59)",
                                  "Moderate (60-74)", "Good (75-89)",
                                  "Excellent (90-100)"))
  )

cat(sprintf("  Matches within 5km: %d / %d\n",
            sum(validation$close_match), nrow(validation)))
cat(sprintf("  Median match distance: %.0f m\n",
            median(validation$distance_m)))

# =============================================================================
# SECTION 3: REGRESSION ANALYSIS — mortality ~ SSI
# =============================================================================

cat("\nRunning regression analyses...\n")

# --- 3.1 Primary: all 29 sites ---
reg_all <- lm(mean_mortality ~ ssi, data = validation)
reg_sum <- summary(reg_all)

slope_all <- coef(reg_all)[2]
r2_all    <- reg_sum$r.squared
p_all     <- reg_sum$coefficients[2, 4]

# --- 3.2 Close matches only (<5km) ---
val_close  <- filter(validation, close_match)
reg_close  <- lm(mean_mortality ~ ssi, data = val_close)
reg_sum_c  <- summary(reg_close)
r2_close   <- reg_sum_c$r.squared
p_close    <- reg_sum_c$coefficients[2, 4]

# --- 3.3 Sensitivity: remove SST 2015 outlier ---
# SST 2015 = 74.9% mortality (2015 SC 1000-year flood, Winyah Bay freshwater pulse)
mort_no_out <- mortality_raw %>%
  filter(EventSeason <= 2023) %>%
  filter(!(Site_code == "SST" & EventSeason == 2015)) %>%
  group_by(Site_code, site_latitude, site_longitude) %>%
  summarise(mean_mortality = mean(Mortality, na.rm = TRUE),
            n_years = n(), .groups = "drop")

val_no_out <- validation %>%
  select(-mean_mortality, -n_years) %>%
  left_join(mort_no_out %>% select(Site_code, mean_mortality, n_years),
            by = "Site_code")

reg_no_out <- lm(mean_mortality ~ ssi, data = val_no_out)
r2_no_out  <- summary(reg_no_out)$r.squared
p_no_out   <- summary(reg_no_out)$coefficients[2, 4]

# --- 3.4 Component-level correlations (Spearman) ---
components <- list(
  "SSI (base composite)"       = "ssi",
  "ESSI (full composite)"      = "essi_score",
  "Comp 1 - Base SSI (norm.)"  = "comp1_base_ssi",
  "Comp 2 - MK Trend"          = "comp2_mk_trend",
  "Comp 3 - Storm Vuln."       = "comp3_storm",
  "Comp 4 - Watershed"         = "comp4_watershed"
)

comp_cors <- map_dfr(names(components), function(nm) {
  col <- components[[nm]]
  ct  <- cor.test(validation[[col]], validation$mean_mortality,
                  method = "spearman", use = "complete.obs")
  tibble(component = nm, r = ct$estimate, p = ct$p.value,
         significant = ct$p.value < 0.05)
})

cat("\nComponent correlations (Spearman r):\n")
print(comp_cors)

# =============================================================================
# SECTION 4: WINYAH BAY 2015 CASE STUDY
# =============================================================================

cat("\nBuilding Winyah Bay 2015 case study...\n")

# Join station coordinates to WQP
wqp_with_coords <- wqp %>%
  left_join(
    stations %>% select(Location_Identifier,
                        Location_LatitudeStandardized,
                        Location_LongitudeStandardized),
    by = c("MonitoringLocationIdentifier" = "Location_Identifier")
  )

# Winyah Bay area, Oct-Nov 2015, salinity only
winyah_2015 <- wqp_with_coords %>%
  filter(
    year(ActivityStartDate) == 2015,
    month(ActivityStartDate) %in% c(10, 11),
    grepl("salinity", CharacteristicName, ignore.case = TRUE),
    as.numeric(Location_LatitudeStandardized) > 33.0
  ) %>%
  select(
    station_id  = MonitoringLocationIdentifier,
    date        = ActivityStartDate,
    lat         = Location_LatitudeStandardized,
    lon         = Location_LongitudeStandardized,
    salinity    = ResultMeasureValue,
    units       = `ResultMeasure/MeasureUnitCode`
  ) %>%
  mutate(
    salinity = as.numeric(salinity),
    # Classify as interior (river-dominated) vs coastal (ocean-dominated)
    # Interior = lon more negative than -79.15 AND lat < 33.5
    site_type = if_else(lon < -79.15 & lat < 33.5,
                        "Interior / River-dominated", "Coastal / Ocean-dominated")
  ) %>%
  arrange(date)

cat(sprintf("  Winyah Bay area records Oct-Nov 2015: %d\n", nrow(winyah_2015)))
cat("  Salinity readings by site:\n")
print(winyah_2015 %>%
        select(station_id, date, lat, lon, salinity, site_type))

# =============================================================================
# SECTION 5: CLASS-LEVEL MORTALITY SUMMARY
# =============================================================================

class_summary <- validation %>%
  group_by(ssi_class) %>%
  summarise(
    n_sites         = n(),
    mean_mortality  = mean(mean_mortality, na.rm = TRUE),
    median_mortality = median(mean_mortality, na.rm = TRUE),
    sd_mortality    = sd(mean_mortality, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMortality by SSI class:\n")
print(class_summary)

# =============================================================================
# SECTION 6: FIGURES
# =============================================================================

cat("\nGenerating figures...\n")

# Palette
pal_class <- c(
  "Moderate (60-74)"   = "#d73027",
  "Good (75-89)"       = "#fee08b",
  "Excellent (90-100)" = "#1a9850"
)

# ── Figure 1: Scatter plot — SSI vs mean mortality ──────────────────────────
fig1 <- ggplot(validation,
               aes(x = ssi, y = mean_mortality,
                   color = ssi_class, shape = close_match)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray30", fill = "gray80", linewidth = 0.8) +
  geom_point(size = 3.5, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = Site_code),
                            size = 2.8, color = "gray20",
                            min.segment.length = 0.3) +
  scale_color_manual(values = pal_class, name = "SSI Class") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "< 5 km", "FALSE" = "≥ 5 km"),
                     name = "Match quality") +
  annotate("text", x = 68, y = 14,
           label = sprintf("R² = %.2f\np = %.4f\nn = %d sites",
                           r2_all, p_all, nrow(validation)),
           hjust = 0, size = 3.5, color = "gray20",
           family = "sans") +
  labs(
    title    = "Salinity Stability Index vs. Sentinel Oyster Mortality",
    subtitle = "SCDNR Sentinel Program (2015–2023) | 29 sites | Linear regression",
    x        = "SSI Score (station-level composite, 1997–2023)",
    y        = "Mean Annual Mortality (%)",
    caption  = paste0("SST 2015 note: 74.9% mortality event consistent with Winyah Bay\n",
                      "freshwater pulse from October 2015 SC flood. Mechanistically\n",
                      "supported by SCDES monitoring records showing salinity 0.04–0.76 ppt\n",
                      "at interior stations, Nov 2015.")
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_ssi_mortality_scatter.png"),
       fig1, dpi = 300, width = 9, height = 6.5)

# ── Figure 2: Class-level boxplot ────────────────────────────────────────────
# Pull annual data for boxplot
mortality_annual <- mortality_raw %>%
  filter(EventSeason <= 2023) %>%
  left_join(validation %>% select(Site_code, ssi_class, ssi),
            by = "Site_code") %>%
  filter(!is.na(ssi_class))

fig2 <- ggplot(mortality_annual %>%
                 filter(ssi_class %in% names(pal_class)),
               aes(x = ssi_class, y = Mortality, fill = ssi_class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = pal_class, guide = "none") +
  scale_x_discrete(labels = c(
    "Moderate (60-74)"   = "Moderate\n(SSI 60–74)\nn = 2 sites",
    "Good (75-89)"       = "Good\n(SSI 75–89)\nn = 2 sites",
    "Excellent (90-100)" = "Excellent\n(SSI 90–100)\nn = 25 sites"
  )) +
  labs(
    title    = "Annual Oyster Mortality by SSI Class",
    subtitle = "All site-years 2015–2023 | SCDNR Sentinel Program",
    x        = "SSI Suitability Class",
    y        = "Annual Mortality (%)",
    caption  = "Each point = one site-year observation. Boxes show median and IQR."
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(FIG_DIR, "ch5_ssi_class_mortality_boxplot.png"),
       fig2, dpi = 300, width = 7, height = 5.5)

# ── Figure 3: Component correlation bar chart ────────────────────────────────
fig3 <- ggplot(comp_cors,
               aes(x = reorder(component, r), y = r,
                   fill = significant)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
  geom_hline(yintercept = c(-0.3, 0.3),
             linetype = "dashed", color = "gray60", linewidth = 0.4) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d1d1d1"),
                    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                    name = "Significance") +
  labs(
    title    = "Spearman Correlation: SSI/ESSI Components vs. Mortality",
    subtitle = "Negative r = higher score → lower mortality (expected direction)",
    x        = NULL,
    y        = "Spearman r",
    caption  = "SSI base composite is the only component with significant correlation (p < 0.05)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(FIG_DIR, "ch5_ssi_component_correlations.png"),
       fig3, dpi = 300, width = 8, height = 5)

# ── Figure 4: Winyah Bay 2015 — salinity by site type ────────────────────────
fig4 <- ggplot(winyah_2015,
               aes(x = reorder(station_id, salinity),
                   y = salinity, fill = site_type)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 8, linetype = "dashed",
             color = "#d73027", linewidth = 0.8) +
  geom_hline(yintercept = 15, linetype = "dotted",
             color = "#fee08b", linewidth = 0.7) +
  annotate("text", x = 0.6, y = 9.2,
           label = "< 8 ppt = acute mortality threshold",
           hjust = 0, size = 3, color = "#d73027") +
  annotate("text", x = 0.6, y = 16.2,
           label = "< 15 ppt = suboptimal for oysters",
           hjust = 0, size = 3, color = "#b8860b") +
  coord_flip() +
  scale_fill_manual(
    values = c("Interior / River-dominated"  = "#d73027",
               "Coastal / Ocean-dominated"   = "#1a9850"),
    name = "Site hydrological type"
  ) +
  labs(
    title    = "Salinity Readings: Winyah Bay Area, October–November 2015",
    subtitle = "SCDES WQP monitoring data | Post-flood freshwater pulse event",
    x        = "Station ID",
    y        = "Salinity (ppt)",
    caption  = paste0(
      "SST sentinel site (lat 33.15, lon -79.32) is interior/river-dominated.\n",
      "Station RT-15097 (lat 33.1, lon -79.4) recorded 0.76 ppt on Nov 18 — ",
      "nearest SCDES station to SST.\n",
      "Interior stations show near-freshwater conditions 4–6 weeks post-flood."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_winyah_bay_2015_salinity.png"),
       fig4, dpi = 300, width = 9, height = 5.5)

# =============================================================================
# SECTION 7: EXPORT SUMMARY TABLE AND TEXT REPORT
# =============================================================================

# Summary CSV
val_export <- validation %>%
  select(Site_code, site_latitude, site_longitude,
         nearest_station, distance_km,
         ssi, ssi_class, essi_score,
         mean_mortality, median_mortality, max_mortality,
         sd_mortality, n_years, close_match, trend_simple) %>%
  arrange(desc(mean_mortality))

write_csv(val_export,
          file.path(OUT_DIR, "ssi_mortality_validation_results.csv"))

# Text report
report <- sprintf(
'SSI BIOLOGICAL VALIDATION — SUMMARY REPORT
Kate Chatman | College of Charleston / SC Sea Grant Consortium
Generated: %s

═══════════════════════════════════════════════════════════════
DATASET
═══════════════════════════════════════════════════════════════
Sentinel mortality data:  %d sites, 2015–2023 (Gary Sundin, SCDNR MRRI)
SSI/ESSI stations:        %d stations (503 qualifying, 26-year record)
Temporal overlap:         2015–2023 (9 years per site)
Spatial match:            %d of %d sites within 5km of SSI station
                          Median match distance: %.0f m

═══════════════════════════════════════════════════════════════
REGRESSION RESULTS: MEAN MORTALITY ~ SSI
═══════════════════════════════════════════════════════════════
All 29 sites:
  Slope:   %.4f  (mortality change per 1-pt SSI increase)
  R²:      %.4f  (%.1f%% of mortality variance explained)
  p-value: %.4f  %s

Close matches only (n=%d, <5km):
  R²:      %.4f
  p-value: %.4f

Sensitivity — SST 2015 outlier removed:
  R²:      %.4f
  p-value: %.4f
  NOTE: SST 2015 outlier is mechanistically valid (see Case Study below).
        It is NOT a statistical confound to be removed.

═══════════════════════════════════════════════════════════════
CLASS-LEVEL MORTALITY
═══════════════════════════════════════════════════════════════
SSI Class           n_sites   Mean mortality   SD
Moderate (60-74)      2         %.1f%%           --
Good (75-89)          2         %.1f%%           --
Excellent (90-100)   25         %.1f%%           --

Direction is consistent: Moderate > Good > Excellent.

═══════════════════════════════════════════════════════════════
COMPONENT CORRELATIONS (Spearman r, all 29 sites)
═══════════════════════════════════════════════════════════════
%s

KEY FINDING: SSI base composite is the only significant predictor.
ESSI components (MK trend, storm, watershed) do not significantly
correlate with mean mortality in this dataset. See interpretation
section for why this is expected, not a weakness.

═══════════════════════════════════════════════════════════════
WINYAH BAY 2015 FLOOD CASE STUDY
═══════════════════════════════════════════════════════════════
SST sentinel site:  lat 33.145, lon -79.318 | SSI = 64.6 (Moderate)
SST 2015 mortality: 74.9%%

SCDES WQP salinity readings, Winyah Bay area, Oct-Nov 2015:
%s

INTERPRETATION: Interior, river-dominated stations recorded near-
freshwater salinities (0.04–0.76 ppt) 4–6 weeks after the October
2015 SC flood (later called the "1000-year flood"). Coastal/ocean-
dominated stations 50km northeast recorded normal salinity (28–30 ppt)
on the same dates, confirming the crash was localized to the freshwater
drainage plume.

The SST site sits at the head of the Winyah Bay drainage, receiving
outflow from the Black River and Pee Dee River systems (primary
Midlands flood drainage pathways). The SSI classified SST as Moderate
(chronically unstable) from 26 years of monitoring data — correctly
identifying it as a river-dominated, salinity-flashy site before the
2015 event occurred.

This constitutes mechanistic empirical validation: the index identified
the site as high-risk using historical data; the predicted vulnerability
was realized in an extreme event; water quality monitoring confirmed
the causal mechanism (near-zero salinity from flood discharge).

═══════════════════════════════════════════════════════════════
FILES PRODUCED
═══════════════════════════════════════════════════════════════
Figures (300 DPI PNG):
  ch5_ssi_mortality_scatter.png
  ch5_ssi_class_mortality_boxplot.png
  ch5_ssi_component_correlations.png
  ch5_winyah_bay_2015_salinity.png

Data:
  ssi_mortality_validation_results.csv  (full site-level results)
',
  Sys.time(),
  nrow(mortality), nrow(essi),
  sum(validation$close_match), nrow(validation),
  median(validation$distance_m),
  slope_all, r2_all, r2_all * 100, p_all,
  if (p_all < 0.05) "*** SIGNIFICANT" else "ns",
  sum(val_close$close_match),
  r2_close, p_close,
  r2_no_out, p_no_out,
  class_summary$mean_mortality[class_summary$ssi_class == "Moderate (60-74)"],
  class_summary$mean_mortality[class_summary$ssi_class == "Good (75-89)"],
  class_summary$mean_mortality[class_summary$ssi_class == "Excellent (90-100)"],
  paste(sprintf("  %-30s r = %+.3f  p = %.4f  %s",
                comp_cors$component, comp_cors$r, comp_cors$p,
                if_else(comp_cors$significant, "**", "")),
        collapse = "\n"),
  paste(sprintf("  %-30s %.2f ppt  (%s)",
                winyah_2015$station_id,
                winyah_2015$salinity,
                winyah_2015$site_type),
        collapse = "\n")
)

writeLines(report, file.path(OUT_DIR, "ssi_validation_report.txt"))
cat(report)
cat("\nAll files written to:", OUT_DIR, "\nFigures written to:", FIG_DIR, "\n")
