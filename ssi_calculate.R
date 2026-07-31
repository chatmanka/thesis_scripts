# =============================================================================
# ssi_calculate.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : SSI — core index
# Used in     : Ch. 4 Methods; Ch. 5 Results
# Status      : FINAL
#
# PURPOSE
#   Calculates the four-factor Salinity Stability Index at station level from
#   SCDES monitoring data. Components: Optimal Range Score (w=0.30), CV
#   Stability Score (w=0.30), Extreme Event Score (w=0.25), Duration Score
#   (w=0.15). Assigns ordinal suitability classes using manual breaks.
#
# INPUTS      : all_sc_wq.xlsx (61,681 obs, 593 stations, May-Sep 1997-2023)
# OUTPUTS     : ssi_results.csv / .xlsx; SSI_Results_with_coords.csv
# RUN AFTER   : None — entry point
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

df <-read_excel("all_sc_wq.xlsx")
df$Station <- gsub("^Station_", "", df$Station)
names(df) <- make.names(names(df))
df_clean <- df %>%
  filter(!is.na(Salinity), Salinity !=0)
df_clean$Date <- mdy(df_clean$Date)
df_clean_sal <- df_clean %>%
  select(Station, Salinity, Date, Time.of.Day)

ssi_results <- df_clean_sal %>%
  group_by(Station) %>%
  summarise(
    n_observations = n(),
    med_sal = median(Salinity, na.rm = TRUE),
    mean_sal = mean(Salinity, na.rm = TRUE),
    sd_sal = sd(Salinity, na.rm = TRUE),
    or_score = sum(Salinity >= 10 & Salinity <= 35) / n() * 100,
    cv = (sd(Salinity, na.rm = TRUE) / mean(Salinity, na.rm = TRUE)) * 100,
    cv_stabilityscore = pmax(0, 100 - (sd(Salinity, na.rm = TRUE) / mean(Salinity, na.rm = TRUE)) * 100),
    days_below_8 = sum(Salinity < 8),
    days_above_35 = sum(Salinity > 35),
    pct_below_8 = (sum(Salinity < 8) / n()) * 100,
    pct_above_35 = (sum(Salinity > 35) / n()) * 100,
    extreme_event_score = pmax(0, 100 -
                                 (sum(Salinity < 8) / n() * 100 * 2) -
                                 (sum(Salinity > 35) / n() * 100 * 1.25)),
    .groups = "drop"       
  )

calculate_duration_score <- function(sal_vector) {
  suboptimal <- sal_vector < 10 | sal_vector > 35
  suboptimal <- suboptimal[!is.na(suboptimal)]
  rle_result <- rle(suboptimal)
  
  max_consecutive_days <- if (any(rle_result$values)) {  
    max(rle_result$lengths[rle_result$values])
  } else {
    0
  }
  duration_score <- pmax(0, 100 - (max_consecutive_days^1.2))
  return(duration_score)
}

duration_results <- df_clean_sal %>%
  group_by(Station) %>%
  arrange(Date, .by_group = TRUE) %>%
  summarise(
    duration_score = calculate_duration_score(Salinity),
    .groups = "drop"         
  )

ssi_results <- ssi_results %>%
  left_join(duration_results, by = "Station")

w1 <- 0.30
w2 <- 0.30
w3 <- 0.25
w4 <- 0.15

ssi_results <- ssi_results %>%
  mutate(
    ssi_raw = (w1 * or_score) +
      (w2 * cv_stabilityscore) +
      (w3 * extreme_event_score) +
      (w4 * duration_score),
    ssi = round(ssi_raw, 1), 
    
    stability_class = case_when(
    ssi >= 90 ~ "Excellent (90-100)",
    ssi >= 75 ~ "Good (75-89)",
    ssi >= 60 ~ "Moderate (60-74)",
    ssi >= 45 ~ "Poor (45-59)",
    TRUE ~ "Unsuitable (<45)"),
    
    risk_level = case_when(
      ssi >= 90 ~ "Minimal Risk",
      ssi >= 75 ~ "Low Risk",
      ssi >= 60 ~ "Moderate Risk",
      ssi >= 45 ~ "High Risk",
      TRUE ~ "Very High Risk"
    )
  ) %>%
  arrange(desc(ssi))
summary(ssi_results$ssi)
table(ssi_results$stability_class)
write_xlsx(ssi_results, "ssi_results.xlsx")
write.csv(ssi_results, "ssi_results.csv")

getwd()

library(dplyr)
library(readr)

ssi_results  <- read_csv("ssi_results.csv")
march_coords <- read_csv("marchresults_with_coords.csv")

coords <- march_coords %>%
  select(Station, LATITUDE, LONGITUDE) %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE))

ssi_with_coords <- ssi_results %>%
  left_join(coords, by = "Station") %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE))

write_csv(ssi_with_coords, "SSI_Results_with_coords.csv")
library(classInt)

breaks <- classIntervals(ssi_results$ssi, n = 5, style = "jenks")
print(breaks)
