# =============================================================================
# gap_acreage_table.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Regulatory gap analysis
# Used in     : Ch. 6 Governance — gap quantification
# Status      : FINAL — supersedes gap_acreage_table.R (IDW-based)
#
# PURPOSE
#   Recalculates biophysical suitability acreage from the masked SSI IPDW
#   surface and regenerates the gap acreage table. Replaces the earlier IDW-
#   based version.
#
# INPUTS      : ipdw_ssi_classified.tif; SCDES classification polygons; active lease layer
# OUTPUTS     : gap_acreage_table.png; acreage summary
# RUN AFTER   : ipdw_ssi_interpolation.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# update_gap_acreage_table.R
# =============================================================================
# Kate Chatman | Thesis: Closing the Governance Gap
# Recalculates biophysical suitability acreage from masked SSI IPDW surface
# and regenerates gap_acreage_table.png — REPLACES the old IDW-based file.
#
# Run AFTER ch5_ipdw_surface_maps.R so you know the new acreage numbers.
# Or run standalone — this script calculates acreage from scratch.
#
# INPUTS:
#   ../data/ipdw_ssi_surface_final.tif   — masked SSI IPDW surface
#
# OUTPUTS:
#   ../data/gap_acreage_table.png        — REPLACES old file
#   ../data/gap_acreage_table.csv        — REPLACES old file (updated numbers)
#
# PACKAGES: terra, gt, dplyr, scales, webshot2 (or gt's built-in png export)
#   install.packages(c("terra","gt","dplyr","scales","webshot2"))
# =============================================================================

library(terra)
library(sf)
library(dplyr)
library(gt)
library(scales)

setwd("C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts")

DATA_DIR   <- "../data/"
SSI_PATH   <- file.path(DATA_DIR, "ipdw_ssi_100k.tif")
GDB_PATH   <- file.path(DATA_DIR, "Permit26_051226/SFpermit26_051226.gdb")
GDB_LAYER  <- "SFpermit26_051226_StaticInternal"
OUT_PNG    <- file.path(DATA_DIR, "gap_acreage_table.png")
OUT_CSV    <- file.path(DATA_DIR, "gap_acreage_table.csv")

CELLS_TO_ACRES <- (100 * 100) / 4046.856

# =============================================================================
# STEP 0: Load SFAREA polygon from SCDNR GDB
# =============================================================================

cat("Loading SFAREA polygon from SCDNR GDB...\n")
sfarea_raw   <- sf::st_read(GDB_PATH, layer = GDB_LAYER, quiet = TRUE)
sfarea_union <- sf::st_union(sfarea_raw)
sfarea_vect  <- terra::vect(sfarea_union)

total_sfarea_acres <- as.numeric(sum(sf::st_area(sfarea_raw))) / 4046.856
cat(sprintf("  Total SFAREA: %s acres\n\n", format(round(total_sfarea_acres), big.mark=",")))

# =============================================================================
# STEP 1: Calculate SSI IPDW acreage by class
# =============================================================================

if (!file.exists(SSI_PATH)) {
  stop("SSI raw surface not found at: ", SSI_PATH)
}

cat("Clipping SSI IPDW surface to SFAREA...\n")
ssi_surf <- terra::mask(terra::rast(SSI_PATH), sfarea_vect)

ssi_class <- terra::classify(ssi_surf,
  rcl = matrix(c(
     0,  20, 1,
    20,  40, 2,
    40,  60, 3,
    60,  80, 4,
    80, 100, 5
  ), ncol = 3, byrow = TRUE),
  include.lowest = TRUE
)

freq <- terra::freq(ssi_class)
freq$acres <- freq$count * CELLS_TO_ACRES

# Named lookup
get_acres <- function(class_val) {
  val <- freq$acres[freq$value == class_val]
  if (length(val) == 0) return(0)
  round(val)
}

acres_excellent <- get_acres(5)
acres_good      <- get_acres(4)
acres_moderate  <- get_acres(3)
acres_poor_vp   <- get_acres(1) + get_acres(2)
acres_suitable  <- acres_excellent + acres_good
acres_total_ssi <- sum(freq$acres)

cat(sprintf("  Excellent (80-100): %s acres\n",  format(acres_excellent, big.mark=",")))
cat(sprintf("  Good (60-80):       %s acres\n",  format(acres_good, big.mark=",")))
cat(sprintf("  Moderate (40-60):   %s acres\n",  format(acres_moderate, big.mark=",")))
cat(sprintf("  Poor/V.Poor (<40):  %s acres\n",  format(acres_poor_vp, big.mark=",")))
cat(sprintf("  PRIMARY SUITABLE:   %s acres\n",  format(acres_suitable, big.mark=",")))
cat(sprintf("  TOTAL SURFACE:      %s acres\n\n",format(round(acres_total_ssi), big.mark=",")))

# =============================================================================
# STEP 2: Build full gap table data
# Hardcoded non-IPDW rows are unchanged from SCDES/SCDNR source data.
# Update only if those source layers have changed.
# =============================================================================

gap_data <- tibble::tribble(
  ~group,                         ~category,                                         ~acres,          ~ref_pct,  ~source_note,

  # Biophysical Suitability — UPDATED from SSI IPDW surface
  "Biophysical Suitability (SSI)",
  "Biophysically Suitable — Excellent SSI (80–100)",
  acres_excellent,
  paste0(round(acres_excellent / acres_suitable * 100, 1), "%"),
  "SSI IPDW surface — salinity stability index",

  "Biophysical Suitability (SSI)",
  "Biophysically Suitable — Good SSI (60–80)",
  acres_good,
  paste0(round(acres_good / acres_suitable * 100, 1), "%"),
  "SSI IPDW surface — salinity stability index",

  "Biophysical Suitability (SSI)",
  "Total Primary Suitable (Excellent + Good)",
  acres_suitable,
  "100%",
  "Primary suitability threshold for mariculture",

  "Biophysical Suitability (SSI)",
  "Moderate Suitability (40–60)",
  acres_moderate,
  paste0(round(acres_moderate / acres_suitable * 100, 1), "%"),
  "Marginal — not recommended for new leases",

  "Biophysical Suitability (SSI)",
  "Poor / Unsuitable (<40)",
  acres_poor_vp,
  paste0(round(acres_poor_vp / acres_suitable * 100, 1), "%"),
  "Excluded from suitability assessment",

  "Biophysical Suitability (SSI)",
  "Total SSI Surface Area",
  round(acres_total_ssi),
  "—",
  "Total SFAREA surface coverage",

  # Regulatory Classification — unchanged (SCDES source)
  "Regulatory Classification (SCDES)",
  "SCDES Approved Harvest Area",
  322542, "34.9%", "Open to commercial shellfish harvest",

  "Regulatory Classification (SCDES)",
  "SCDES Restricted Harvest Area",
  26702, "2.9%", "Harvest restricted — management controls required",

  "Regulatory Classification (SCDES)",
  "Total SCDES Classified Area",
  349244, "37.8%", "Total area under SCDES shellfish classification",

  # Exclusionary Areas — unchanged (SCDNR source)
  "Exclusionary Areas (SCDNR)",
  "Active Mariculture Leases (SCDNR_SF_MC)¹",
  988, "0.31%¹", "Existing mariculture — new applications excluded¹",

  "Exclusionary Areas (SCDNR)",
  "Public Shellfish Grounds Exclusion (PSG)",
  8685, "2.7%", "Recreational harvest — new mariculture excluded",

  "Exclusionary Areas (SCDNR)",
  "Recreational SSG Exclusion (RECSSG)",
  8297, "2.6%", "Recreational harvest — new mariculture excluded",

  "Exclusionary Areas (SCDNR)",
  "Total Exclusionary Acreage",
  17969, "5.6%", "Cannot receive new mariculture lease applications",

  # Gap Metrics — unchanged
  "Gap Metrics",
  "Approved Acreage Available for New Leases",
  304573, "33%", "Approved minus all current exclusions",

  "Gap Metrics",
  "Active Leases as % of Primary Suitable",
  NA_real_, "0.107%", "% of suitable area under active production",

  "Gap Metrics",
  "Active Leases as % of Approved Area",
  NA_real_, "0.31%", "% of approved area under active production",

  "Gap Metrics",
  "Approved Area as % of Primary Suitable",
  NA_real_, "34.9%", "% of suitable area that is Approved",

  "Gap Metrics",
  "Tier 1 Restricted Stations (dual-condition met)",
  NA_real_, "—", "Stations: both FC and SSI stable/improving, Restricted class"
)

# Format acres column for display
gap_data <- gap_data %>%
  mutate(acres_display = case_when(
    is.na(acres) ~ "—",
    TRUE          ~ format(round(acres), big.mark = ",")
  ))

# Save CSV
write.csv(gap_data %>% select(-acres_display), OUT_CSV, row.names = FALSE)
cat("Saved CSV:", OUT_CSV, "\n")

# =============================================================================
# STEP 3: Build gt table and export PNG
# =============================================================================

gt_table <- gap_data %>%
  select(group, category, acres_display, ref_pct, source_note) %>%
  gt(groupname_col = "group") %>%
  cols_label(
    category     = "Category",
    acres_display = "Acres",
    ref_pct      = "Reference %",
    source_note  = "Source / Note"
  ) %>%
  tab_header(
    title    = "Shellfish Mariculture Suitability and Utilization Gap",
    subtitle = "South Carolina Coastal Waters | SSI IPDW Analysis + SCDES/SCDNR Data, 2023"
  ) %>%
  tab_source_note(
    source_note = md(
      "¹ Utilization figure confirmed by SCDNR (G. Sundin, personal communication, May 2026)"
    )
  ) %>%
  tab_source_note(
    source_note = md(
      "Sources: SSI IPDW surface (Chatman 2026); SCDES Shellfish Harvest Classifications; SCDNR Mariculture Exclusion Layer"
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f5e9"),    # light green highlight
    locations = cells_body(
      rows = category == "Total Primary Suitable (Excellent + Good)"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#fce4ec"),    # light red highlight (utilization gap rows)
    locations = cells_body(
      rows = category %in% c(
        "Active Leases as % of Primary Suitable",
        "Active Leases as % of Approved Area"
      )
    )
  ) %>%
  tab_options(
    table.font.size      = px(12),
    heading.title.font.size   = px(14),
    heading.subtitle.font.size = px(11),
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.border.top.style    = "none",
    table.border.bottom.style = "none"
  ) %>%
  cols_width(
    category     ~ px(280),
    acres_display ~ px(90),
    ref_pct      ~ px(100),
    source_note  ~ px(300)
  )

# Export PNG
gtsave(gt_table, OUT_PNG, vwidth = 900, vheight = 700)
cat("Saved table PNG:", OUT_PNG, "\n")

cat("\n============================================================\n")
cat("Gap table update complete.\n")
cat("Key number to use throughout thesis:\n")
cat(sprintf("  Total Primary Suitable: %s acres\n",
            format(acres_suitable, big.mark=",")))
cat("(Replaces old IDW-based figure of 922,908 acres if different)\n")
cat("============================================================\n")
