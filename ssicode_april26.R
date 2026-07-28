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
    .groups = "drop"          # <-- no extra ) here
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
