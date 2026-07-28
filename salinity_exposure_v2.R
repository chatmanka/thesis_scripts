# =============================================================================
# salinity_exposure_v2.R
# Kate Chatman | MPA/EVSS Thesis | College of Charleston / SC Sea Grant
# July 2026
#
# REPLACES: storm_vulnerability.R
#
# WHY THIS REPLACES THE OLD SCRIPT — three problems found in the original:
#
#   1. PRECIPITATION MATCH BUG. The nearest-neighbour join matched each WQP
#      station to the spatially closest NOAA gauge without checking whether
#      that gauge had data covering the salinity record. 684 of 937 stations
#      had ZERO temporal overlap; only 19.2% of salinity observations had
#      precipitation data on the sampling date. The reported storm-association
#      rates (3.4% at 1.0in, 6.8% at 0.5in) were artifacts of this gap, not
#      hydrological signal. FIX: aggregate over ALL gauges within 30 km and
#      take the maximum antecedent rainfall (86.2% coverage).
#
#   2. NO BASE-RATE COMPARISON. Once coverage is fixed, ~38% of ALL salinity
#      observations — crash or not — fall within 3 days of a >=0.5in rain.
#      Binary "storm-associated" labelling barely discriminates (relative risk
#      1.05). Any storm claim must be made against this base rate.
#
#   3. THE COMPOSITE MEASURED ONE THING THREE TIMES. Crash rate, autumn crash
#      %, and severity were collinear (r = 0.854 between the first two), so the
#      40/35/25 weights averaged a single signal. A single number — % of
#      readings below 8 ppt — reproduced the whole composite at r = 0.925
#      (Spearman 0.923). And that signal was chronic freshness, not storm
#      response: 58.8% of all sub-8 ppt readings came from 23 oligohaline
#      stations; the 428 marine stations contributed 1.8%.
#
# WHAT THIS SCRIPT DOES INSTEAD — two independent, single-number metrics:
#
#   METRIC 1  Chronic low-salinity exposure = % of readings below 8 ppt.
#             A proportion, not a per-year rate, so eligibility needs
#             observations rather than distinct years: n_obs >= 10 retains
#             659 stations instead of 100 (6.6x coverage for IPDW).
#
#   METRIC 2  Storm responsiveness = per-station OLS slope of station- and
#             month-demeaned salinity anomaly on 7-day antecedent rainfall
#             (ppt per inch). This is the "flashiness" measure Sundin asked
#             about, and it is what the old crash count failed to capture.
#
#   The two are empirically independent (r = -0.056, Spearman -0.077, n = 581),
#   which is the justification for reporting them separately rather than
#   collapsing them into a weighted index.
#
# WINDOW: 7 days (168 h), not 72 h. A 7-day window explains twice the variance
#   in salinity anomaly as a 3-day window (r = -0.126 vs -0.088; r^2 0.0158 vs
#   0.0078), and is consistent with the ~93 h storm-to-crash lag measured at
#   NERRS Thousand Acre during the October 2015 Winyah Bay flood.
#
# CAVEAT ON TEMPORAL RESOLUTION: WQP grab samples have a median 32-day gap
#   (only 1.1% of consecutive gaps <= 3 days, ~10 samples/station/year). These
#   data cannot resolve individual storm responses at event scale; Metric 2 is
#   a population-level regression across many station-observations, not an
#   event-detection method.
#
# INPUTS  (same as before)
#   narrowresult.csv, Station.csv, 4299750.csv ... 4330634.csv
# OUTPUTS
#   salinity_exposure_and_storm_response.csv
# =============================================================================

library(tidyverse)
library(lubridate)

DATA_DIR <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data/scdeswqpdata97_23"
setwd("C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts")

SALINITY_THRESHOLD <- 8      # ppt
PRECIP_WINDOW_DAYS <- 7      # 168 h
GAUGE_RADIUS_KM    <- 30
MIN_OBS            <- 10

# -----------------------------------------------------------------------------
# STEP 1: WQP salinity
# -----------------------------------------------------------------------------
narrow <- read_csv(file.path(DATA_DIR, "narrowresult.csv"),
                   col_types = cols(.default = "c")) %>%
  filter(grepl("alinit", CharacteristicName, ignore.case = TRUE)) %>%
  mutate(date = as.Date(ActivityStartDate),
         year = year(date), month = month(date),
         salinity = as.numeric(ResultMeasureValue),
         unit = `ResultMeasure/MeasureUnitCode`,
         station_id = MonitoringLocationIdentifier) %>%
  filter(unit %in% c("ppt","ppth"),
         !is.na(salinity), salinity >= 0, salinity <= 40) %>%
  select(station_id, date, year, month, salinity)

stations <- read_csv(file.path(DATA_DIR, "Station.csv"),
                     col_types = cols(.default = "c")) %>%
  filter(Org_Identifier == "21SC60WQ_WQX") %>%
  mutate(lat = as.numeric(Location_Latitude),
         lon = as.numeric(Location_Longitude)) %>%
  select(station_id = Location_Identifier, station_name = Location_Name, lat, lon) %>%
  filter(!is.na(lat), !is.na(lon))

narrow <- narrow %>% left_join(stations, by = "station_id") %>% filter(!is.na(lat))

cat("Salinity records (raw):", nrow(narrow),
    "| stations:", n_distinct(narrow$station_id), "\n")

# Collapse replicate samples to one value per station-date.
# 342 records share a station and date; 138 of these carry different salinity
# values under distinct ActivityIdentifiers (the depth field is empty for all
# of them, so they are repeat samples / separate analyses rather than
# depth-stratified profiles). Retaining both would weight those dates twice in
# the exposure proportion and treat them as independent observations in the
# Metric 2 regression, which they are not — both readings share the same
# antecedent rainfall value. Averaging is the conservative choice.
#
# This is not a consequential decision: retaining all replicates instead gives
# 662 / 584 stations, 81 significant slopes, and an independence r of -0.059,
# versus 659 / 581, 79, and -0.058 here. Reported for transparency.
narrow <- narrow %>%
  group_by(station_id, station_name, lat, lon, date, year, month) %>%
  summarise(salinity = mean(salinity), .groups = "drop")

cat("Salinity records (replicates averaged):", nrow(narrow),
    "| stations:", n_distinct(narrow$station_id), "\n")

# -----------------------------------------------------------------------------
# STEP 2: NOAA precipitation
# -----------------------------------------------------------------------------
precip_all <- list.files(DATA_DIR, pattern = "^[0-9]{7}\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) %>%
  mutate(date = as.Date(DATE), prcp = as.numeric(PRCP),
         lat = as.numeric(LATITUDE), lon = as.numeric(LONGITUDE)) %>%
  filter(!is.na(prcp), !is.na(lat)) %>%
  select(precip_station = STATION, precip_name = NAME, date, prcp, lat, lon)

cat("Precip records:", nrow(precip_all), "| gauges:", n_distinct(precip_all$precip_station), "\n")

# -----------------------------------------------------------------------------
# STEP 3: COVERAGE-ROBUST ANTECEDENT RAINFALL
#   For each salinity observation take the MAXIMUM daily rainfall recorded at
#   ANY gauge within 30 km during the preceding PRECIP_WINDOW_DAYS. Using all
#   gauges in the radius (rather than the single nearest) is what fixes the
#   temporal-coverage bug in the original script.
# -----------------------------------------------------------------------------
gauge_locs <- precip_all %>% select(precip_station, lat, lon) %>% distinct()
deg <- GAUGE_RADIUS_KM / 111

wqp_locs <- narrow %>% select(station_id, lat, lon) %>% distinct()

# station -> nearby gauges
pairs <- wqp_locs %>%
  rowwise() %>%
  mutate(gauges = list(gauge_locs$precip_station[
    sqrt((gauge_locs$lat - lat)^2 + (gauge_locs$lon - lon)^2) <= deg])) %>%
  ungroup() %>%
  select(station_id, gauges) %>%
  unnest(gauges) %>%
  rename(precip_station = gauges)

cat("Station-gauge pairs within", GAUGE_RADIUS_KM, "km:", nrow(pairs), "\n")

# antecedent max rainfall per observation
#
# IMPORTANT — join order matters here. Joining obs -> gauges -> precip_all on
# precip_station ALONE fans out to every date each gauge ever recorded (~1.5
# billion rows) before the date filter runs, and will exhaust memory. Instead
# expand the lookback dates FIRST so the precipitation join matches on the
# composite key (precip_station, p_date) and acts as a filter rather than a
# fan-out. Peak size is ~7.5M rows.
obs_keys <- narrow %>% select(station_id, date) %>% distinct()

# (station_id, date) -> one row per lookback day 0..PRECIP_WINDOW_DAYS
obs_lookback <- obs_keys %>%
  tidyr::crossing(lag_days = 0:PRECIP_WINDOW_DAYS) %>%
  mutate(p_date = date - lag_days) %>%
  select(station_id, date, p_date)

precip_keyed <- precip_all %>%
  select(precip_station, p_date = date, prcp) %>%
  filter(!is.na(prcp))

antecedent <- obs_lookback %>%
  inner_join(pairs, by = "station_id", relationship = "many-to-many") %>%
  inner_join(precip_keyed, by = c("precip_station", "p_date")) %>%
  group_by(station_id, date) %>%
  summarise(precip_7d_max_in = max(prcp), .groups = "drop")

narrow <- narrow %>% left_join(antecedent, by = c("station_id","date"))
cat("Observations with gauge coverage:",
    round(mean(!is.na(narrow$precip_7d_max_in))*100,1), "%\n")

# -----------------------------------------------------------------------------
# METRIC 1: CHRONIC LOW-SALINITY EXPOSURE
# -----------------------------------------------------------------------------
metric1 <- narrow %>%
  group_by(station_id, station_name, lat, lon) %>%
  summarise(n_obs = n(), n_years = n_distinct(year),
            first_year = min(year), last_year = max(year),
            mean_salinity = round(mean(salinity),2),
            sd_salinity   = round(sd(salinity),2),
            min_salinity  = round(min(salinity),2),
            pct_below_8ppt = round(mean(salinity < SALINITY_THRESHOLD)*100, 2),
            mean_depth_below_8 = round(
              ifelse(any(salinity < SALINITY_THRESHOLD),
                     mean(SALINITY_THRESHOLD - salinity[salinity < SALINITY_THRESHOLD]), 0), 2),
            .groups = "drop") %>%
  filter(n_obs >= MIN_OBS) %>%
  mutate(exposure_class = case_when(
    pct_below_8ppt >= 50 ~ "Chronic (>=50%)",
    pct_below_8ppt >= 20 ~ "Frequent (20-50%)",
    pct_below_8ppt >= 5  ~ "Occasional (5-20%)",
    TRUE                 ~ "Rare (<5%)"))

cat("\nMETRIC 1 stations:", nrow(metric1), "\n")
print(table(metric1$exposure_class))

# -----------------------------------------------------------------------------
# METRIC 2: STORM RESPONSIVENESS (ppt per inch)
#   Salinity is demeaned by station (removes chronic freshness) and by month
#   (removes seasonality) before regressing on antecedent rainfall. Without the
#   station demeaning the relationship is masked entirely by between-station
#   variance — the naive pooled relative risk is 0.91, i.e. apparently
#   protective, which is the confound that broke the original analysis.
# -----------------------------------------------------------------------------
anom_df <- narrow %>%
  filter(!is.na(precip_7d_max_in)) %>%
  group_by(station_id) %>% mutate(anom = salinity - mean(salinity)) %>% ungroup() %>%
  group_by(month)      %>% mutate(anom = anom - mean(anom))       %>% ungroup()

metric2 <- anom_df %>%
  group_by(station_id) %>%
  filter(n() >= MIN_OBS, sd(precip_7d_max_in) > 0, sd(anom) > 0) %>%
  group_modify(~{
    fit <- lm(anom ~ precip_7d_max_in, data = .x)
    s   <- summary(fit)
    tibble(n_obs_slope = nrow(.x),
           storm_response_ppt_per_inch = round(coef(fit)[2], 3),
           slope_se = round(s$coefficients[2,2], 3),
           slope_p  = round(s$coefficients[2,4], 4),
           slope_r2 = round(s$r.squared, 4))
  }) %>% ungroup() %>%
  mutate(storm_response_class = case_when(
    storm_response_ppt_per_inch < -1.0 & slope_p < 0.05 ~ "High storm responsiveness",
    storm_response_ppt_per_inch < -0.5 & slope_p < 0.10 ~ "Moderate storm responsiveness",
    storm_response_ppt_per_inch < 0                     ~ "Low storm responsiveness",
    TRUE                                                ~ "No detected response"))

cat("\nMETRIC 2 stations:", nrow(metric2), "\n")
print(table(metric2$storm_response_class))

# -----------------------------------------------------------------------------
# COMBINE + INDEPENDENCE CHECK
# -----------------------------------------------------------------------------
final <- metric1 %>% left_join(metric2, by = "station_id") %>%
  arrange(desc(pct_below_8ppt))

ind <- final %>% filter(!is.na(storm_response_ppt_per_inch))
cat("\nIndependence of the two metrics: Pearson r =",
    round(cor(ind$pct_below_8ppt, ind$storm_response_ppt_per_inch), 3),
    "| Spearman =",
    round(cor(ind$pct_below_8ppt, ind$storm_response_ppt_per_inch, method="spearman"), 3),
    "(n =", nrow(ind), ")\n")

write_csv(final, "salinity_exposure_and_storm_response.csv")
cat("\nWritten: salinity_exposure_and_storm_response.csv —", nrow(final), "stations\n")
