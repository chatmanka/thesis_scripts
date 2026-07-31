# =============================================================================
# ipdw_essi_interpolation.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Spatial interpolation
# Used in     : Ch. 4 Methods (Spatial Interpolation)
# Status      : FINAL
#
# PURPOSE
#   Same IPDW cost-weighted approach applied to the ESSI composite layer.
#
# INPUTS      : uvvr_cost_raster.tif; ESSI composite scores; coastal boundary layer
# OUTPUTS     : ipdw_essi_surface_final.tif; ipdw_essi_classified.tif
# RUN AFTER   : essi_composite.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# ESSI IPDW INTERPOLATION — UVVR COST-WEIGHTED SUITABILITY SURFACE
# =============================================================================
# Author:  Kate Chatman
# Date:    2026-06-07
# Project: "Closing the Governance Gap" — SC Shellfish Mariculture Thesis
#          MPA/EVSS Dual Degree, College of Charleston
#          SC Sea Grant Consortium Graduate Research Assistant
#
# PURPOSE:
#   Produce a continuous Enhanced Salinity Suitability Index (ESSI) surface
#   for SC's coastal zone using Inverse Path Distance Weighting (IPDW).
#   Unlike standard IDW which interpolates in straight lines, IPDW routes
#   interpolation paths through a cost raster — in this case the UVVR
#   (Unvegetated-to-Vegetated Vegetation Ratio) raster derived from
#   Sentinel-2 imagery (2017-2023). This forces interpolation to follow
#   waterways rather than crossing marsh and upland barriers.
#
# METHODOLOGY:
#   IPDW follows Stachelek & Madden (2015): "Use of Inverse Path Distance
#   Weighting for Spatial Interpolation in Estuarine Environments."
#   Journal of Coastal Research. doi:10.2112/JCOASTRES-D-14-00051.1
#
#   UVVR cost surface: Ganju et al. (2017, 2020); Blanchard (2025)
#   UVVR formula: UVVR = (1-VF)/VF from Sentinel-2 MSAVI, 2017-2023
#   Cost resistance values:
#     UVVR < 0.1  (dense vegetated marsh) → resistance = 3
#     UVVR 0.1-1.0 (mixed marsh/tidal flat) → resistance = 5
#     UVVR 1.0-2.0 (bare soil/exposed flat) → resistance = 8
#     UVVR = 5    (open water)              → resistance = 1
#     Upland/NoData                         → resistance = 100 (barrier)
#
#   Resistance values calibrated to reflect hydrological connectivity:
#   open water = lowest resistance (free tidal exchange); vegetated marsh =
#   moderate (water moves but slowly); degraded/bare marsh = high (episodic
#   flow, poor connectivity); upland = impassable barrier.
#
# INPUTS:
#   uvvr_sc_coast_2017_2023.tif         — continuous UVVR raster from GEE
#   ../data/essi_composite_scores.csv   — 503 ESSI station scores with coords
#
# OUTPUTS:
#   ipdw_essi_surface.tif               — continuous ESSI suitability raster
#   ipdw_essi_classified.tif            — classified raster (5 classes)
#   ipdw_essi_surface.png               — diagnostic map figure
#   ipdw_validation_stats.csv           — LOO cross-validation results
#
# NOTES:
#   - IPDW is computationally intensive for large extents. The SC coast at
#     10m resolution may take 30-90 minutes. Consider running overnight or
#     using coarser resolution (30m) for initial validation.
#   - All outputs in NAD 1983 UTM Zone 17N (EPSG:26917)
#   - ipdw package requires raster objects (not terra SpatRaster).
#     We use raster:: functions for ipdw calls, terra:: for everything else.
#     Explicit namespacing avoids tidyverse/terra extract() conflicts.
# =============================================================================

library(ipdw)
library(terra)
library(raster)   # ipdw requires raster objects, not terra SpatRaster
library(sf)
library(tidyverse)

# Explicit namespace for conflict-prone functions
# terra::extract() vs tidyr::extract() — always use terra::extract() explicitly
# dplyr::select() vs raster::select() — always use dplyr::select() explicitly

setwd("C:/Users/chatm/OneDrive - College of Charleston/Desktop/thesis/ssiworkthesis/scripts")

# =============================================================================
# STEP 1: LOAD AND PREPARE UVVR RASTER
# =============================================================================

cat("Loading UVVR raster...\n")

# Load continuous UVVR raster exported from GEE
uvvr_terra <- terra::rast("../data/uvvr_sc_coast_2017_2023.tif")

cat("UVVR raster loaded.\n")
cat("  CRS:", as.character(terra::crs(uvvr_terra, describe = TRUE)$name), "\n")
cat("  Resolution:", terra::res(uvvr_terra), "\n")
cat("  Extent:", as.character(terra::ext(uvvr_terra)), "\n")
cat("  Value range:", terra::global(uvvr_terra, "range", na.rm = TRUE)[[1]],
    "to", terra::global(uvvr_terra, "range", na.rm = TRUE)[[2]], "\n")

# Verify projection — should be NAD83 UTM Zone 17N (EPSG:26917)
# GEE exported with CRS EPSG:26917 per script specification
if (!terra::same.crs(uvvr_terra, "EPSG:26917")) {
  cat("Reprojecting UVVR to NAD83 UTM Zone 17N...\n")
  uvvr_terra <- terra::project(uvvr_terra, "EPSG:26917")
  cat("Reprojection complete.\n")
} else {
  cat("CRS confirmed: NAD83 UTM Zone 17N\n")
}

# =============================================================================
# STEP 2: BUILD COST RASTER FROM UVVR VALUES
# =============================================================================

cat("\nBuilding cost raster from UVVR values...\n")

# Assign hydrological resistance values to UVVR classes
# Logic: low UVVR = healthy vegetated marsh = moderate resistance (water
# moves through marsh but slowly); open water = lowest resistance (free
# tidal exchange); degraded marsh = high resistance (poor connectivity)
#
# Resistance value rationale (Stachelek & Madden 2015):
#   Open water (UVVR=5): 1   — free tidal exchange, lowest cost
#   Dense vegetated marsh (UVVR<0.1): 3  — water moves, moderate cost
#   Mixed marsh (UVVR 0.1-1.0): 5  — transitional, moderate-high cost
#   Bare soil/tidal flat (UVVR 1.0-2.0): 8  — poor connectivity, high cost
#   NoData/upland: 100  — impassable barrier

cost_rast <- terra::classify(uvvr_terra,
  rcl = matrix(c(
    # from,  to,    becomes
    -Inf,  0.0,    100,   # NoData/negative → upland barrier
     0.0,  0.1,      3,   # Dense vegetated marsh
     0.1,  1.0,      5,   # Mixed marsh / transitional tidal flats
     1.0,  2.0,      8,   # Bare soil / exposed tidal flats
     2.0,  4.99,   100,   # Values between 2-5 → upland/barrier
     4.99, 5.01,     1,   # Open water (UVVR=5 assigned in GEE)
     5.01, Inf,    100    # Above 5 → barrier
  ), ncol = 3, byrow = TRUE),
  include.lowest = TRUE
)

cat("Cost raster built.\n")
cat("  Unique resistance values:", sort(unique(terra::values(cost_rast)[
  !is.na(terra::values(cost_rast))])), "\n")

# Save cost raster for ArcGIS inspection
terra::writeRaster(cost_rast,
                   "../data/uvvr_cost_raster.tif",
                   overwrite = TRUE)
cat("Cost raster saved: ../data/uvvr_cost_raster.tif\n")

# Convert to raster object for ipdw (ipdw requires raster package objects)
cost_rast_r <- raster::raster(cost_rast)

# =============================================================================
# STEP 3: LOAD AND PREPARE ESSI STATION POINTS
# =============================================================================

cat("\nLoading ESSI station data...\n")

essi <- read_csv("../data/essi_composite_scores.csv") %>%
  dplyr::filter(!is.na(LATITUDE), !is.na(LONGITUDE), !is.na(essi_score))

cat("  ESSI stations loaded:", nrow(essi), "\n")

# Convert to sf spatial object
essi_sf <- sf::st_as_sf(essi,
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = 4326)  # input coords are WGS84 decimal degrees

# Project to match cost raster (NAD83 UTM Zone 17N)
essi_sf <- sf::st_transform(essi_sf, crs = 26917)

cat("  Stations projected to NAD83 UTM Zone 17N\n")

# Convert to SpatialPointsDataFrame for ipdw
essi_sp <- sf::as_Spatial(essi_sf)

cat("  Stations converted to SpatialPointsDataFrame\n")
cat("  Coordinate range X:", range(sp::coordinates(essi_sp)[,1]), "\n")
cat("  Coordinate range Y:", range(sp::coordinates(essi_sp)[,2]), "\n")

# =============================================================================
# STEP 4: VERIFY ALIGNMENT
# =============================================================================

cat("\nVerifying spatial alignment...\n")

# Check that stations fall within raster extent
rast_ext <- terra::ext(cost_rast)
pts_x    <- sp::coordinates(essi_sp)[,1]
pts_y    <- sp::coordinates(essi_sp)[,2]

n_outside <- sum(
  pts_x < rast_ext$xmin | pts_x > rast_ext$xmax |
  pts_y < rast_ext$ymin | pts_y > rast_ext$ymax
)

cat("  Stations outside raster extent:", n_outside, "\n")
if (n_outside > 0) {
  cat("  WARNING: Some stations fall outside the UVVR raster extent.\n")
  cat("  These will be dropped from interpolation.\n")
}

# Extract cost values at station locations (sanity check)
cost_at_stations <- terra::extract(cost_rast, terra::vect(essi_sf))
cat("  Stations with NoData cost value:",
    sum(is.na(cost_at_stations[,2])), "\n")
cat("  Stations with barrier cost (100):",
    sum(cost_at_stations[,2] == 100, na.rm = TRUE), "\n")
cat("  Stations with valid cost value:",
    sum(!is.na(cost_at_stations[,2]) & cost_at_stations[,2] < 100), "\n")

# =============================================================================
# STEP 5: IPDW INTERPOLATION
# =============================================================================

cat("\nRunning IPDW interpolation...\n")
cat("NOTE: This step is computationally intensive.\n")
cat("Expected runtime: 30-90 minutes for full SC coast at 10m resolution.\n")
cat("Consider using coarseSample for initial test (see below).\n\n")

# --- OPTION A: FULL RESOLUTION RUN (production) ---
# Uncomment when ready for final output. Run overnight if needed.
# cat("Running full resolution interpolation...\n")
# t_start <- Sys.time()
# essi_ipdw <- ipdw::ipdw(
#   spdf      = essi_sp,
#   costras   = cost_rast_r,
#   varname   = "essi_score",
#   overlapped = TRUE
# )
# t_end <- Sys.time()
# cat("Full resolution complete. Time elapsed:", difftime(t_end, t_start), "\n")

# --- OPTION B: COARSE TEST RUN (validation, ~5-10 min) ---
# Aggregates cost raster to 100m resolution for fast test
# Run this first to confirm the pipeline works before full resolution
cat("Running coarse test (100m resolution) to validate pipeline...\n")
cat("Switch to OPTION A for final thesis output.\n\n")

cost_coarse <- raster::aggregate(cost_rast_r, fact = 10, fun = mean)

t_start <- Sys.time()
essi_ipdw_test <- ipdw::ipdw(
  spdf       = essi_sp,
  costras    = cost_coarse,
  varname    = "essi_score",
  overlapped = TRUE
)
t_end <- Sys.time()
cat("Coarse test complete. Time elapsed:",
    round(as.numeric(difftime(t_end, t_start, units = "mins")), 1), "minutes\n")

# Use test result for validation; swap to essi_ipdw for final output
essi_surface <- essi_ipdw_test   # change to essi_ipdw for production

# Convert back to terra for downstream processing
essi_surface_terra <- terra::rast(essi_surface)

# =============================================================================
# STEP 6: POST-PROCESS OUTPUT
# =============================================================================

cat("\nPost-processing interpolated surface...\n")

# Clip to SC coastal zone (mask to non-upland areas)
# Use UVVR cost raster: anywhere cost = 100 (upland/barrier) → mask out
water_mask <- cost_rast != 100
essi_masked <- terra::mask(essi_surface_terra, water_mask, maskvalue = FALSE)

# Report surface statistics
surf_stats <- terra::global(essi_masked, c("min", "max", "mean", "sd"),
                              na.rm = TRUE)
cat("\nESSI surface statistics:\n")
cat("  Min:", round(surf_stats$min, 2), "\n")
cat("  Max:", round(surf_stats$max, 2), "\n")
cat("  Mean:", round(surf_stats$mean, 2), "\n")
cat("  SD:", round(surf_stats$sd, 2), "\n")

# Classify into 5 ordinal classes matching ESSI point classification
essi_classified <- terra::classify(essi_masked,
  rcl = matrix(c(
     0,  20, 1,   # Very Poor
    20,  40, 2,   # Poor
    40,  60, 3,   # Moderate
    60,  80, 4,   # Good
    80, 100, 5    # Excellent
  ), ncol = 3, byrow = TRUE)
)

# =============================================================================
# STEP 7: LEAVE-ONE-OUT CROSS-VALIDATION
# =============================================================================

cat("\nRunning leave-one-out cross-validation...\n")
cat("NOTE: LOO CV at coarse resolution — will take several minutes.\n")

set.seed(42)

# Sample subset for LOO validation (use all 503 for production)
# For speed during testing, sample 50 stations
n_loo <- min(50, nrow(essi_sp))
loo_idx <- sample(seq_len(nrow(essi_sp)), n_loo)
essi_sp_loo <- essi_sp[loo_idx, ]

loo_results <- map_dfr(seq_len(nrow(essi_sp_loo)), function(i) {
  # Hold out station i
  train <- essi_sp_loo[-i, ]
  test  <- essi_sp_loo[ i, ]

  # Interpolate without held-out station
  pred_rast <- tryCatch({
    ipdw::ipdw(
      spdf       = train,
      costras    = cost_coarse,
      varname    = "essi_score",
      overlapped = TRUE
    )
  }, error = function(e) NULL)

  if (is.null(pred_rast)) return(NULL)

  # Extract predicted value at held-out station location
  pred_val <- raster::extract(pred_rast, test)

  tibble(
    Station   = test$Station,
    observed  = test$essi_score,
    predicted = pred_val,
    residual  = pred_val - test$essi_score
  )
}) %>%
  dplyr::filter(!is.na(predicted))

# Validation statistics
rmse <- sqrt(mean(loo_results$residual^2))
mae  <- mean(abs(loo_results$residual))
r2   <- cor(loo_results$observed, loo_results$predicted)^2

cat("\n===== LOO CROSS-VALIDATION RESULTS =====\n")
cat("Stations validated:", nrow(loo_results), "\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE:", round(mae, 3), "\n")
cat("R²:", round(r2, 4), "\n")
cat("Interpretation: RMSE < 5 ESSI points = good interpolation accuracy\n")

# =============================================================================
# STEP 8: WRITE OUTPUTS
# =============================================================================

cat("\nWriting outputs...\n")

# Continuous surface
terra::writeRaster(essi_masked,
                   "../data/ipdw_essi_surface.tif",
                   overwrite = TRUE)

# Classified surface
terra::writeRaster(essi_classified,
                   "../data/ipdw_essi_classified.tif",
                   overwrite = TRUE)

# Validation stats
write_csv(loo_results, "../data/ipdw_validation_stats.csv")

cat("Outputs written:\n")
cat("  ../data/ipdw_essi_surface.tif\n")
cat("  ../data/ipdw_essi_classified.tif\n")
cat("  ../data/ipdw_validation_stats.csv\n")

# =============================================================================
# STEP 9: DIAGNOSTIC FIGURE
# =============================================================================

cat("\nProducing diagnostic figure...\n")

# Convert to data frame for ggplot
essi_df <- as.data.frame(essi_masked, xy = TRUE) %>%
  dplyr::rename(essi_score = 3) %>%
  dplyr::filter(!is.na(essi_score))

# Station points for overlay
station_df <- as.data.frame(sp::coordinates(essi_sp)) %>%
  dplyr::rename(x = coords.x1, y = coords.x2) %>%
  dplyr::mutate(essi_score = essi_sp$essi_score)

p <- ggplot() +
  geom_raster(data = essi_df,
              aes(x = x, y = y, fill = essi_score)) +
  geom_point(data = station_df,
             aes(x = x, y = y),
             color = "black", size = 0.8, alpha = 0.6) +
  scale_fill_gradientn(
    colors = c("#7b0000", "#d95f02", "#f7d03c", "#4dac26", "#1a6b3c"),
    limits = c(20, 100),
    name   = "ESSI Score",
    breaks = c(20, 40, 60, 80, 100),
    labels = c("20\n(Poor)", "40\n(Moderate)", "60\n(Good)",
               "80\n(Excellent)", "100")
  ) +
  coord_equal() +
  labs(
    title    = "Enhanced Salinity Suitability Index (ESSI) — IPDW Surface",
    subtitle = paste0("UVVR cost-weighted interpolation | Sentinel-2 2017-2023 | ",
                      "n=503 monitoring stations"),
    x        = "Easting (m, NAD83 UTM Zone 17N)",
    y        = "Northing (m)",
    caption  = paste0("IPDW method: Stachelek & Madden (2015) | ",
                      "UVVR: Ganju et al. (2017, 2020) | Analysis: Chatman 2026")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave("../data/ipdw_essi_surface.png", p,
       width = 12, height = 10, dpi = 300)
cat("Diagnostic figure saved: ../data/ipdw_essi_surface.png\n")

# =============================================================================
# STEP 10: SESSION SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("  IPDW INTERPOLATION — SESSION SUMMARY\n")
cat("================================================================\n")
cat("Resolution used:    COARSE TEST (100m) — switch to 10m for thesis\n")
cat("Stations used:      503\n")
cat("Cost raster source: UVVR from Sentinel-2 2017-2023 (GEE)\n")
cat("\nValidation (LOO CV):\n")
cat("  RMSE:", round(rmse, 3), "\n")
cat("  MAE:", round(mae, 3), "\n")
cat("  R²:", round(r2, 4), "\n")
cat("\nOutputs in ../data/:\n")
cat("  ipdw_essi_surface.tif      — continuous ESSI raster\n")
cat("  ipdw_essi_classified.tif   — 5-class classified raster\n")
cat("  ipdw_validation_stats.csv  — LOO cross-validation results\n")
cat("  ipdw_essi_surface.png      — diagnostic map\n")
cat("  uvvr_cost_raster.tif       — cost surface for ArcGIS inspection\n")
cat("\nNEXT STEPS:\n")
cat("  1. Load ipdw_essi_surface.tif in ArcGIS Pro\n")
cat("  2. Verify spatial extent and visual pattern make sense\n")
cat("  3. Switch OPTION B → OPTION A for 10m production run\n")
cat("  4. Export publication-resolution map figures from ArcGIS\n")
cat("================================================================\n")

