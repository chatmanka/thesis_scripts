# =============================================================================
# ssi_validation_figures.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation
# Used in     : Ch. 5 figures
# Status      : FINAL
#
# PURPOSE
#   Produces the final publication figures for the validation chapter,
#   including the revised scatter plot and the Winyah Bay 2015 salinity
#   figure.
#
# INPUTS      : Saved validation results; WQP salinity data
# OUTPUTS     : Validation figures (PNG)
# RUN AFTER   : ssi_biological_validation.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

library(tidyverse)
library(ggrepel)

FIG_DIR <- "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/figures"

# Validation results — already saved from full run
validation <- read_csv(
  "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data/ssi_mortality_validation_results.csv",
  show_col_types = FALSE
)

# WQP data for Winyah Bay figure
wqp <- read_csv(
  "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data/databases/excel/narrowresult.csv",
  show_col_types = FALSE
)

stations <- read_csv(
  "C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/data/databases/excel/Station.csv",
  show_col_types = FALSE
)

pal_class <- c(
  "Moderate (60-74)"   = "#d73027",
  "Good (75-89)"       = "#fee08b",
  "Excellent (90-100)" = "#1a9850"
)

# ── FIGURE 1 (revised): Scatter plot ─────────────────────────────────────────

reg_all <- lm(mean_mortality ~ ssi, data = validation)
r2_all  <- summary(reg_all)$r.squared
p_all   <- summary(reg_all)$coefficients[2, 4]

fig1 <- ggplot(validation,
               aes(x = ssi, y = mean_mortality,
                   color = ssi_class, shape = close_match)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray30", fill = "gray85", linewidth = 0.8) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_text_repel(
    aes(label = Site_code),
    size          = 2.9,
    color         = "gray15",
    box.padding   = 0.45,      # more padding between labels
    point.padding = 0.3,
    min.segment.length = 0.2,
    max.overlaps  = 20,
    seed          = 42         # reproducible layout
  ) +
  scale_color_manual(values = pal_class, name = "SSI Class") +
  scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1),
    labels = c("TRUE" = "< 5 km", "FALSE" = "≥ 5 km"),
    name   = "Match quality"
  ) +
  annotate("text", x = 67, y = 13.5,
           label = sprintf("R² = %.2f  |  p = %.4f  |  n = 29 sites",
                           r2_all, p_all),
           hjust = 0, size = 3.5, color = "gray20") +
  labs(
    title    = "Salinity Stability Index vs. Sentinel Oyster Mortality",
    subtitle = "SCDNR Sentinel Program (2015–2023) | 29 sites | Linear regression",
    x        = "SSI Score (station-level composite, 1997–2023)",
    y        = "Mean Annual Mortality (%)",
    caption  = "SST 2015: 74.9% mortality mechanistically supported by SCDES records showing 0.04–0.76 ppt at interior Winyah Bay stations (Nov 2015)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption    = element_text(size = 8.5, color = "gray40", hjust = 0),
    legend.position = "right"
  )

ggsave(file.path(FIG_DIR, "ch5_ssi_mortality_scatter.png"),
       fig1, dpi = 300, width = 10, height = 6.5)

# ── FIGURE 4 (revised): Winyah Bay 2015 ──────────────────────────────────────

winyah_2015 <- wqp %>%
  filter(
    year(ActivityStartDate) == 2015,
    month(ActivityStartDate) %in% c(10, 11),
    grepl("salinity", CharacteristicName, ignore.case = TRUE)
  ) %>%
  left_join(
    stations %>% select(Location_Identifier,
                        Location_LatitudeStandardized,
                        Location_LongitudeStandardized),
    by = c("MonitoringLocationIdentifier" = "Location_Identifier")
  ) %>%
  filter(as.numeric(Location_LatitudeStandardized) > 33.0) %>%
  transmute(
    # Strip the long org prefix — keep only the meaningful station code
    station_id  = str_remove(MonitoringLocationIdentifier, "^21SC60WQ_WQX-"),
    date        = ActivityStartDate,
    lat         = as.numeric(Location_LatitudeStandardized),
    lon         = as.numeric(Location_LongitudeStandardized),
    salinity    = as.numeric(ResultMeasureValue),
    site_type   = if_else(lon < -79.15 & lat < 33.5,
                          "Interior / River-dominated",
                          "Coastal / Ocean-dominated")
  ) %>%
  arrange(salinity)

fig4 <- ggplot(winyah_2015,
               aes(x = salinity,
                   y = reorder(station_id, salinity),
                   fill = site_type)) +
  geom_col(alpha = 0.85, width = 0.7) +
  # Salinity value labels on bars
  geom_text(
    aes(label = sprintf("%.1f ppt", salinity),
        x     = salinity + 0.6),
    hjust = 0, size = 3.3, color = "gray20"
  ) +
  # Threshold lines
  geom_vline(xintercept = 8,  linetype = "dashed",
             color = "#d73027", linewidth = 0.9) +
  geom_vline(xintercept = 15, linetype = "dotted",
             color = "#b8860b", linewidth = 0.8) +
  # Annotations — stacked above plot, not floating over bars
  annotate("text", x = 8.3,  y = 9.7,
           label = "8 ppt: acute\nmortality threshold",
           hjust = 0, vjust = 1, size = 3, color = "#d73027",
           lineheight = 0.9) +
  annotate("text", x = 15.3, y = 9.7,
           label = "15 ppt: suboptimal\nfor oysters",
           hjust = 0, vjust = 1, size = 3, color = "#b8860b",
           lineheight = 0.9) +
  scale_fill_manual(
    values = c("Interior / River-dominated"  = "#d73027",
               "Coastal / Ocean-dominated"   = "#1a9850"),
    name   = "Hydrological type"
  ) +
  scale_x_continuous(
    limits = c(0, 40),
    breaks = c(0, 5, 8, 10, 15, 20, 25, 30, 35),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "Salinity Readings: Winyah Bay Area, October–November 2015",
    subtitle = "SCDES WQP monitoring | Post-flood freshwater pulse | Interior vs. coastal contrast",
    x        = "Salinity (ppt)",
    y        = "Station",
    caption  = paste0(
      "SST sentinel site (lat 33.15, lon -79.32): interior/river-dominated.  ",
      "Nearest SCDES station RT-15097 recorded 0.76 ppt on Nov 18, 2015 — ",
      "six weeks after the October flood.\n",
      "Coastal stations 50 km northeast recorded 28–33 ppt on the same dates. ",
      "Salinity crash was localized to the Pee Dee / Black River drainage plume."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y     = element_text(size = 10, color = "gray20"),
    axis.text.x     = element_text(size = 10),
    plot.caption    = element_text(size = 8, color = "gray40",
                                   hjust = 0, lineheight = 1.3),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()  # cleaner with horizontal bars
  )

ggsave(file.path(FIG_DIR, "ch5_winyah_bay_2015_salinity.png"),
       fig4, dpi = 300, width = 10, height = 6)

cat("Both figures saved to", FIG_DIR, "\n")