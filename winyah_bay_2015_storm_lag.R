# =============================================================================
# winyah_bay_2015_storm_lag.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : ESSI Component 3 — Storm Responsiveness
# Used in     : Ch. 4 Methods (Stormwater Responsiveness) — the ~93 hour figure
# Status      : FINAL — reproduces the storm-to-crash lag cited in the methods
#
# PURPOSE
#   Measures the observed lag between rainfall onset and salinity crossing the
#   8 ppt biological threshold during the October 2015 Winyah Bay flood, using
#   continuous 15-minute NERRS monitoring at Thousand Acre (niwtawq).
#
#   This provides one directly observed, South Carolina-specific storm response
#   time, used to justify the seven-day antecedent rainfall window in
#   salinity_exposure_storm_response.R. It replaces a borrowed 12-48 hour figure
#   from non-SC estuary literature.
#
# INPUTS
#   ../ssiworkthesis/data/nerrs/niw_2014_2016/niwtawq2015.csv
#       NERRS SWMP 15-minute water quality, Thousand Acre station, 2015
#   ../ssiworkthesis/data/scdeswqpdata97_23/[0-9]{7}.csv
#       NOAA daily precipitation summaries (gauge USC00383470 used, see below)
#
# OUTPUTS
#   winyah_bay_2015_storm_lag_curve.csv  — 15-min salinity descent, hours since trigger
#   Console summary reporting the measured lag in hours
#
# RUN AFTER : None — standalone
#
# -----------------------------------------------------------------------------
# TWO METHODOLOGICAL DECISIONS, DOCUMENTED
#
# 1. GAUGE SELECTION — why USC00383470 and not the nearest gauge
#    The nearest NOAA gauge to Thousand Acre is USC00383468 (Georgetown 2 E),
#    but its record ends in 2005 and contains no 2015 data. USC00383469
#    (Georgetown 3 W) ends in 2007. USC00383470 (Georgetown Co Airport) is the
#    nearest Georgetown-area gauge with continuous 2015 coverage, approximately
#    13 km from the NERRS station. Selecting a gauge by distance alone, without
#    checking record overlap, returns a gauge with no data for the event.
#
# 2. TRIGGER DATE — why 1 October and not an automatic threshold rule
#    A rule that selects the first day exceeding 0.5 inches within a search
#    window returns 25 September 2015: an isolated 1.45 inch day followed by
#    five near-dry days, unrelated to the flood. The October 2015 event is a
#    continuous multi-day sequence:
#
#        Oct 1  2.75 in     Oct 2  1.91 in     Oct 3  5.70 in
#        Oct 4  6.62 in     Oct 5  6.52 in
#
#    The trigger is therefore set to 1 October 2015, the onset of that
#    sustained sequence. Using the peak-rainfall day (3 October) instead would
#    give a shorter lag of approximately 45 hours; the naive 25 September
#    trigger would give approximately 237 hours. Both alternatives are reported
#    below so the sensitivity of the figure to this choice is visible.
#
# INTERPRETATION
#    This is one measured lag, from one flood, at one station. It is a
#    defensible South Carolina-specific data point, not a population estimate.
#    Salinity declined gradually across 1-4 October rather than dropping in a
#    single pulse, so the figure describes a multi-day decline and should not be
#    presented with false precision.
# =============================================================================

library(tidyverse)
library(lubridate)

# ---- CONFIGURATION ----------------------------------------------------------
NERRS_FILE   <- "../../ssiworkthesis/data/nerrs/niw_2014_2016/niwtawq2015.csv"
PRECIP_DIR   <- "../../ssiworkthesis/data/scdeswqpdata97_23"
PRECIP_GAUGE <- "USC00383470"              # Georgetown Co Airport
TRIGGER_DATE <- as.Date("2015-10-01")      # onset of the flood rainfall sequence
THRESHOLD    <- 8                          # ppt — SSI extreme-event threshold
WINDOW_BEFORE<- 1                          # days of context before trigger
WINDOW_AFTER <- 10                         # days after trigger to search

# ---- 1. PRECIPITATION -------------------------------------------------------
precip <- list.files(PRECIP_DIR, pattern = "^[0-9]{7}\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) %>%
  mutate(date = as.Date(DATE), prcp = as.numeric(PRCP)) %>%
  filter(STATION == PRECIP_GAUGE, !is.na(prcp))

event_precip <- precip %>%
  filter(date >= as.Date("2015-09-25"), date <= as.Date("2015-10-10")) %>%
  arrange(date)

cat("===== PRECIPITATION AT", PRECIP_GAUGE, "=====\n")
print(event_precip %>% select(date, prcp), n = 20)

# Confirm the gauge actually covers 2015 before proceeding
stopifnot(any(precip$date >= as.Date("2015-01-01") & precip$date <= as.Date("2015-12-31")))

# ---- 2. NERRS CONTINUOUS SALINITY -------------------------------------------
nerrs <- read_csv(NERRS_FILE, col_types = cols(.default = "c")) %>%
  rename_with(str_trim) %>%
  mutate(
    station = str_trim(StationCode),
    dt      = mdy_hm(DateTimeStamp),
    sal     = as.numeric(Sal)
  ) %>%
  filter(!is.na(dt))

window <- nerrs %>%
  filter(dt >= as_datetime(TRIGGER_DATE) - days(WINDOW_BEFORE),
         dt <= as_datetime(TRIGGER_DATE) + days(WINDOW_AFTER)) %>%
  arrange(dt)

cat("\nNERRS readings in window:", nrow(window),
    "| non-missing salinity:", sum(!is.na(window$sal)), "\n")

# ---- 3. MEASURE THE LAG -----------------------------------------------------
first_crash <- window %>% filter(!is.na(sal), sal < THRESHOLD) %>% slice(1)

if (nrow(first_crash) == 0) {
  stop("No sub-threshold reading found in window — widen WINDOW_AFTER or check station.")
}

lag_hours <- as.numeric(difftime(first_crash$dt,
                                 as_datetime(TRIGGER_DATE), units = "hours"))

pre_storm <- window %>% filter(!is.na(sal), dt <= as_datetime(TRIGGER_DATE)) %>%
  slice_tail(n = 1)

cat("\n===== MEASURED STORM-TO-CRASH LAG =====\n")
cat(sprintf("Trigger (rainfall onset)   : %s\n", TRIGGER_DATE))
cat(sprintf("Salinity at trigger        : %.1f ppt\n", pre_storm$sal))
cat(sprintf("First reading below %d ppt : %s (%.1f ppt)\n",
            THRESHOLD, format(first_crash$dt), first_crash$sal))
cat(sprintf("MEASURED LAG               : %.2f hours (%.2f days)\n",
            lag_hours, lag_hours / 24))

# ---- 4. SENSITIVITY TO TRIGGER CHOICE ---------------------------------------
cat("\n===== SENSITIVITY TO TRIGGER DATE =====\n")
for (d in c("2015-09-25", "2015-10-01", "2015-10-03")) {
  h <- as.numeric(difftime(first_crash$dt, as_datetime(as.Date(d)), units = "hours"))
  label <- c("2015-09-25" = "naive rule: first >=0.5in day (unrelated rain)",
             "2015-10-01" = "USED: onset of sustained flood rainfall",
             "2015-10-03" = "peak rainfall day (5.70 in)")[d]
  cat(sprintf("  %s  %7.2f hours   %s\n", d, h, label))
}

# ---- 5. DESCENT CURVE FOR FIGURE --------------------------------------------
descent <- window %>%
  mutate(hours_since_trigger =
           as.numeric(difftime(dt, as_datetime(TRIGGER_DATE), units = "hours"))) %>%
  filter(hours_since_trigger >= 0, hours_since_trigger <= 168) %>%
  select(dt, hours_since_trigger, sal)

write_csv(descent, "winyah_bay_2015_storm_lag_curve.csv")
cat("\nWritten: winyah_bay_2015_storm_lag_curve.csv (",
    nrow(descent), "readings, first 168 hours )\n")

# =============================================================================
# EXPECTED OUTPUT
#   Salinity at trigger        : 22.7 ppt
#   First reading below 8 ppt  : 2015-10-04 21:15 (7.7 ppt)
#   MEASURED LAG               : 93.25 hours (3.89 days)
#
# METHODS SENTENCE THIS SUPPORTS
#   "Continuous 15-minute NERRS monitoring at Thousand Acre (niwtawq) during the
#    October 2015 Winyah Bay flood recorded salinity falling from 22.7 to below
#    8 ppt approximately 93 hours after the rainfall onset."
# =============================================================================
