# =============================================================================
# stormcrash_mortality_table.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 table
# Status      : FINAL
#
# PURPOSE
#   Builds per-station salinity crash metrics and cross-tabulates them against
#   observed mortality.
#
# INPUTS      : Crash event records; mortality data
# OUTPUTS     : Storm crash / mortality summary table
# RUN AFTER   : ssi_biological_validation.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

library(tidyverse)
library(sf)

BASE    <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis"
SCRIPTS <- file.path(BASE, "scripts")
DATA    <- file.path(BASE, "data")
FIG_DIR <- file.path(BASE, "figures")

# ── LOAD DATA ─────────────────────────────────────────────────────────────────
crashes <- read_csv(
  file.path(SCRIPTS, "storm_crash_events.csv"),
  show_col_types = FALSE
)

mortality_raw <- readxl::read_excel(
  file.path(DATA, "databases/excel/MortalityData_SeaGrant_KC.xlsx"),
  sheet = "Sheet2"
) %>% filter(EventSeason <= 2023)

val_map <- read_csv(
  file.path(DATA, "ssi_mortality_validation_results.csv"),
  show_col_types = FALSE
) %>% select(Site_code, ssi, ssi_class)

# ── BUILD PER-STATION CRASH METRICS ──────────────────────────────────────────
station_crashes <- crashes %>%
  group_by(station_id, lat, lon) %>%
  summarise(
    n_crashes           = n(),
    mean_crash_sal      = mean(salinity, na.rm = TRUE),
    min_crash_sal       = min(salinity,  na.rm = TRUE),
    mean_storm_prcp     = mean(storm_prcp, na.rm = TRUE),
    crash_years         = n_distinct(year),
    year_span           = max(year) - min(year) + 1,
    crash_rate_per_year = n() / (max(year) - min(year) + 1),
    .groups             = "drop"
  )

cat(sprintf("Crash stations: %d | Total events: %d\n",
            nrow(station_crashes), nrow(crashes)))

# ── SPATIAL JOIN: sentinel sites → nearest crash station ─────────────────────
# Site-level mean mortality
site_mort <- mortality_raw %>%
  group_by(Site_code, site_latitude, site_longitude) %>%
  summarise(mean_mortality = mean(Mortality, na.rm = TRUE),
            n_years = n(), .groups = "drop") %>%
  left_join(val_map, by = "Site_code")

# Convert to sf UTM 17N
crash_sf <- st_as_sf(station_crashes,
                     coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(26917)

sentinel_sf <- st_as_sf(site_mort,
                        coords = c("site_longitude","site_latitude"),
                        crs = 4326) %>%
  st_transform(26917)

# Find nearest crash station to each sentinel site
nearest_idx  <- st_nearest_feature(sentinel_sf, crash_sf)
dist_m       <- as.numeric(st_distance(sentinel_sf,
                                       crash_sf[nearest_idx,],
                                       by_element = TRUE))

# Build joined table
site_crash <- site_mort %>%
  mutate(
    crash_station       = station_crashes$station_id[nearest_idx],
    crash_dist_km       = dist_m / 1000,
    n_crashes           = station_crashes$n_crashes[nearest_idx],
    crash_rate_per_year = station_crashes$crash_rate_per_year[nearest_idx],
    mean_crash_sal      = station_crashes$mean_crash_sal[nearest_idx],
    min_crash_sal       = station_crashes$min_crash_sal[nearest_idx],
    mean_storm_prcp     = station_crashes$mean_storm_prcp[nearest_idx]
  )

cat("\nSentinel sites with crash station within 10km:",
    sum(site_crash$crash_dist_km < 10), "of", nrow(site_crash))
cat("\nDistance summary (km):\n")
print(summary(site_crash$crash_dist_km))

cat("\n\nFull site-crash table (sorted by crash rate):\n")
print(site_crash %>%
        select(Site_code, ssi_class, mean_mortality,
               crash_station, crash_dist_km,
               crash_rate_per_year, mean_crash_sal) %>%
        arrange(desc(crash_rate_per_year)), n = 30)

# ── REGRESSIONS: CRASH METRICS → MORTALITY ────────────────────────────────────
cat("\n\n=== CRASH FREQUENCY → MORTALITY ===\n")
C1 <- lm(mean_mortality ~ crash_rate_per_year, data = site_crash)
s1 <- summary(C1)
cat(sprintf("R\u00b2 = %.1f%%  p = %.4f\n",
            s1$r.squared*100, s1$coefficients[2,4]))

cat("\n=== CRASH SALINITY DEPTH → MORTALITY ===\n")
C2 <- lm(mean_mortality ~ mean_crash_sal, data = site_crash)
s2 <- summary(C2)
cat(sprintf("R\u00b2 = %.1f%%  p = %.4f\n",
            s2$r.squared*100, s2$coefficients[2,4]))

cat("\n=== SSI + CRASH RATE → MORTALITY (combined) ===\n")
C3 <- lm(mean_mortality ~ ssi + crash_rate_per_year, data = site_crash)
s3 <- summary(C3)
cat(sprintf("R\u00b2 = %.1f%%\n", s3$r.squared*100))
print(round(s3$coefficients, 4))

cat("\n=== CLOSE MATCHES ONLY (<10km) ===\n")
close <- site_crash %>% filter(crash_dist_km < 10)
cat(sprintf("n = %d sites with crash station within 10km\n", nrow(close)))
if(nrow(close) > 5) {
  C4 <- lm(mean_mortality ~ crash_rate_per_year, data = close)
  s4 <- summary(C4)
  cat(sprintf("Crash rate → mortality: R\u00b2=%.1f%%  p=%.4f\n",
              s4$r.squared*100, s4$coefficients[2,4]))
}

# ── FIGURE ────────────────────────────────────────────────────────────────────
ggplot(site_crash, aes(x = crash_rate_per_year, y = mean_mortality)) +
  geom_point(aes(color = ssi_class, size = crash_dist_km), alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "gray20",
              fill = "gray85", linewidth = 0.9) +
  scale_color_manual(
    values = c("Moderate (60-74)" = "#d73027",
               "Good (75-89)"     = "#fee08b",
               "Excellent (90-100)" = "#1a9850"),
    name = "SSI Class"
  ) +
  scale_size_continuous(name = "Dist to crash\nstation (km)",
                        range = c(2, 7), trans = "reverse") +
  ggrepel::geom_text_repel(
    aes(label = Site_code), size = 2.8, color = "gray20",
    min.segment.length = 0.3, box.padding = 0.4
  ) +
  labs(
    title    = "Storm Crash Frequency vs. Mean Oyster Mortality",
    subtitle = "Each site matched to nearest WQP station with documented storm crash events",
    x        = "Crash rate at nearest WQP station (events per year)",
    y        = "Mean annual mortality (%)",
    caption  = "Point size inversely proportional to distance between sentinel site and crash station.\nLarger = closer match."
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(size = 8.5, color = "gray40", hjust = 0))

ggsave(file.path(FIG_DIR, "ch5_crash_rate_mortality.png"),
       dpi = 300, width = 10, height = 6.5)

cat("\nFigure saved.\n")