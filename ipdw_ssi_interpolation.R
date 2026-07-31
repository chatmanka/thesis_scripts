# =============================================================================
# ipdw_ssi_interpolation.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Spatial interpolation
# Used in     : Ch. 4 Methods (Spatial Interpolation)
# Status      : FINAL
#
# PURPOSE
#   Interpolates SSI station scores across the coastal zone using inverse path
#   distance weighting through a UVVR-derived cost raster, so interpolation
#   follows navigable water rather than crossing land barriers (Stachelek &
#   Madden, 2015).
#
# INPUTS      : uvvr_cost_raster.tif; SSI_Results_with_coords.csv; coastal boundary layer
# OUTPUTS     : ipdw_ssi_surface_final.tif; ipdw_ssi_classified.tif
# RUN AFTER   : ssi_calculate.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# SSI IPDW INTERPOLATION — UVVR COST-WEIGHTED SUITABILITY SURFACE
# =============================================================================
# Author:  Kate Chatman
# Date:    2026-07-09
# Project: "Closing the Governance Gap" — SC Shellfish Mariculture Thesis
#          MPA/EVSS Dual Degree, College of Charleston
#          SC Sea Grant Consortium Graduate Research Assistant
#
# PURPOSE:
#   Produce a continuous Salinity Suitability Index (SSI) surface for SC's
#   coastal zone using Inverse Path Distance Weighting (IPDW).
#   IPDW routes interpolation paths through the UVVR cost raster, forcing
#   interpolation to follow waterways rather than cross marsh/upland barriers.
#
# METHODOLOGY:
#   Stachelek & Madden (2015): "Use of Inverse Path Distance Weighting for
#   Spatial Interpolation in Estuarine Environments." Journal of Coastal
#   Research. doi:10.2112/JCOASTRES-D-14-00051.1
#
# INPUTS:
#   SSI_Results_with_coords.csv              — 578 SSI station scores
#   ../data/imagery/uvvr_cost_raster.tif     — pre-built UVVR cost raster
#
# OUTPUTS:
#   ../data/raster/ipdw_ssi_surface_rerun.tif  — continuous SSI surface
#
# POST-PROCESSING MASKS APPLIED:
#   1. KC boundary mask: cost_rast < 100 (excludes upland/barrier pixels,
#      ~3-mile offshore boundary used throughout SSI analysis)
#   2. 20km station proximity mask: pixels farther than 20km from any
#      monitoring station are removed (prevents extrapolation into
#      unmonitored river drainages — Georgetown/Winyah Bay issue)
#
# SSI RISK-BASED CLASSIFICATION BREAKS:
#   Very Low (0-50)    | Low (50-70) | Moderate (70-80)
#   Good (80-90)       | Excellent (90-100)
#
# NOTES:
#   - Cost raster is pre-built; skip the UVVR classification step
#   - Run at 100m (cost raster aggregated factor=10 from 10m)
#   - ipdw requires raster objects; terra used for all other steps
#   - After running: hand .tif back to Claude for re-polygonization + AGOL
# =============================================================================

library(ipdw)
library(terra)
library(raster)
library(sf)
library(tidyverse)

setwd("C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts")

# =============================================================================
# STEP 1: LOAD PRE-BUILT COST RASTER
# =============================================================================

cat("Loading UVVR cost raster...\n")

# Cost resistance values:
#   Open water (UVVR=5):           cost = 1   (free tidal exchange)
#   Dense vegetated marsh (<0.1):  cost = 3
#   Mixed marsh (0.1-1.0):         cost = 5
#   Bare soil/tidal flat (1.0-2.0):cost = 8
#   Upland/NoData:                 cost = 100 (impassable barrier → KC boundary)

cost_rast <- terra::rast("../data/imagery/uvvr_cost_raster.tif")

if (!terra::same.crs(cost_rast, "EPSG:26917")) {
  cat("Reprojecting cost raster to NAD83 UTM Zone 17N...\n")
  cost_rast <- terra::project(cost_rast, "EPSG:26917", method = "near")
}

cat("Cost raster loaded. Shape:", nrow(cost_rast), "x", ncol(cost_rast),
    "| Res:", terra::res(cost_rast), "m\n")

# Convert to raster object for ipdw package
cost_rast_r <- raster::raster(cost_rast)

# =============================================================================
# STEP 2: LOAD SSI STATION DATA
# =============================================================================

cat("\nLoading SSI station data...\n")

ssi <- read_csv("SSI_Results_with_coords.csv") %>%
  dplyr::filter(!is.na(ssi), !is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  dplyr::mutate(ssi = as.numeric(ssi)) %>%
  dplyr::filter(!is.na(ssi))

cat("Stations loaded:", nrow(ssi), "| SSI range:",
    round(min(ssi$ssi), 1), "-", round(max(ssi$ssi), 1), "\n")

# Project to NAD83 UTM Zone 17N
ssi_sf <- sf::st_as_sf(ssi, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
ssi_sf <- sf::st_transform(ssi_sf, crs = 26917)
cat("Stations projected to NAD83 UTM Zone 17N\n")

# Sanity check — stations on valid cost cells
cost_at_pts <- terra::extract(cost_rast, terra::vect(ssi_sf))
cat("  Stations on valid water/marsh (cost < 100):",
    sum(!is.na(cost_at_pts[,2]) & cost_at_pts[,2] < 100), "\n")
cat("  Stations on upland (cost = 100):",
    sum(cost_at_pts[,2] == 100, na.rm = TRUE), "\n")

# =============================================================================
# STEP 3: AGGREGATE COST RASTER TO 100m
# =============================================================================

cat("\nAggregating cost raster: 10m → 100m (factor = 10)...\n")
cost_coarse <- raster::aggregate(cost_rast_r, fact = 10, fun = mean)
cat("Aggregated shape:", nrow(cost_coarse), "x", ncol(cost_coarse),
    "| Res:", raster::res(cost_coarse), "m\n")

# =============================================================================
# STEP 4: IPDW INTERPOLATION
# =============================================================================

cat("\nRunning IPDW — this will take 15-45 minutes. Do not interrupt.\n")

t_start <- Sys.time()
# range: large value (1e6) so KC + 20km masks control extent, not this param
ssi_ipdw <- ipdw::ipdw(
  sf_ob      = ssi_sf,
  costras    = cost_coarse,
  range      = 1e6,
  paramlist  = "ssi",
  overlapped = TRUE
)
t_end <- Sys.time()
cat("IPDW complete. Elapsed:",
    round(as.numeric(difftime(t_end, t_start, units = "mins")), 1), "min\n")

ssi_terra <- terra::rast(ssi_ipdw)
raw_stats  <- terra::global(ssi_terra, c("min","max","mean"), na.rm = TRUE)
cat("Raw surface — Min:", round(raw_stats$min,2),
    "Max:", round(raw_stats$max,2),
    "Mean:", round(raw_stats$mean,2), "\n")

# =============================================================================
# STEP 5: KC BOUNDARY MASK  (cost_rast < 100)
# =============================================================================

cat("\nApplying KC boundary mask (cost < 100)...\n")

cost_coarse_terra <- terra::rast(cost_coarse)
cost_matched      <- terra::resample(cost_coarse_terra, ssi_terra, method = "near")
water_mask        <- cost_matched < 100
ssi_kc            <- terra::mask(ssi_terra, water_mask, maskvalue = FALSE)

cat("After KC mask — valid pixels:",
    terra::global(ssi_kc, "notNA")[[1]], "\n")

# =============================================================================
# STEP 6: 20km STATION PROXIMITY MASK
# =============================================================================

cat("\nApplying 20km station proximity mask...\n")

MAX_DIST_M <- 20000

# Build a raster with 1 at station pixels, NA elsewhere
station_pts_r <- terra::rast(ssi_kc)       # same grid, all NA
terra::values(station_pts_r) <- NA

coords_utm <- sf::st_coordinates(ssi_sf)   # already EPSG:26917
xmin_r <- terra::xmin(ssi_kc)
ymax_r <- terra::ymax(ssi_kc)
xres_r <- terra::xres(ssi_kc)
yres_r <- terra::yres(ssi_kc)
nr_r   <- nrow(ssi_kc)
nc_r   <- ncol(ssi_kc)

for (i in seq_len(nrow(coords_utm))) {
  col_i <- floor((coords_utm[i, 1] - xmin_r) / xres_r) + 1L
  row_i <- floor((ymax_r - coords_utm[i, 2]) / yres_r) + 1L
  if (col_i >= 1L && col_i <= nc_r && row_i >= 1L && row_i <= nr_r) {
    cell_i <- (row_i - 1L) * nc_r + col_i
    terra::values(station_pts_r)[cell_i] <- 1
  }
}
cat("  Station pixels set:", sum(!is.na(terra::values(station_pts_r))), "\n")

dist_r     <- terra::distance(station_pts_r)   # meters, Euclidean
within_20k <- dist_r <= MAX_DIST_M
ssi_final  <- terra::mask(ssi_kc, within_20k, maskvalue = FALSE)

final_stats <- terra::global(ssi_final, c("min","max","mean"), na.rm = TRUE)
valid_n     <- terra::global(ssi_final, "notNA")[[1]]
vals        <- na.omit(terra::values(ssi_final))

cat("After 20km mask — valid pixels:", valid_n, "\n")
cat("  SSI Min:", round(final_stats$min,2),
    "Max:", round(final_stats$max,2),
    "Mean:", round(final_stats$mean,2), "\n")
cat("\nClass breakdown:\n")
cat("  Very Low  (0-50): ", sum(vals < 50), "\n")
cat("  Low      (50-70): ", sum(vals >= 50 & vals < 70), "\n")
cat("  Moderate (70-80): ", sum(vals >= 70 & vals < 80), "\n")
cat("  Good     (80-90): ", sum(vals >= 80 & vals < 90), "\n")
cat("  Excellent(90-100):", sum(vals >= 90), "\n")

# =============================================================================
# STEP 7: WRITE OUTPUT
# =============================================================================

out_path <- "../data/raster/ipdw_ssi_surface_rerun.tif"

terra::writeRaster(ssi_final,
                   out_path,
                   overwrite = TRUE,
                   datatype  = "FLT4S",
                   NAflag    = -9999)

cat("\nOutput written:", out_path, "\n")
verify <- terra::rast(out_path)
cat("Verified — valid pixels:", terra::global(verify, "notNA")[[1]],
    "| CRS:", terra::crs(verify, describe = TRUE)$code, "\n")

# =============================================================================
# DONE
# =============================================================================

cat("\n================================================================\n")
cat("  SSI IPDW — COMPLETE\n")
cat("================================================================\n")
cat("Output: ../data/raster/ipdw_ssi_surface_rerun.tif\n")
cat("\nNEXT STEPS:\n")
cat("  1. Open in ArcGIS Pro — verify Georgetown/Winyah Bay looks correct\n")
cat("  2. Confirm the ~3-mile offshore KC boundary is intact\n")
cat("  3. Hand the .tif to Claude (Cowork) to re-polygonize and re-upload\n")
cat("================================================================\n")
