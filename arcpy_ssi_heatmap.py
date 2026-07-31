# =============================================================================
# arcpy_ssi_heatmap.py
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Siting tool (ArcGIS)
# Used in     : Applied product — mariculture siting tool
# Status      : FINAL
#
# PURPOSE
#   Continuous salinity stability heatmap for the siting tool, with attribute-
#   rich station points for pop-ups so growers can read a gradient and click a
#   station for its exact SSI and risk class.
#
# INPUTS      : EBK surface; station point layer (file geodatabase)
# OUTPUTS     : SSI_Heatmap_Raster; SSI_Station_Points
# RUN AFTER   : arcpy_ssi_finalize.py
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

"""
=============================================================================
SSI HEATMAP FINALIZATION -- SC Mariculture Siting Tool
=============================================================================
Purpose : Continuous heatmap of salinity stability across SC coast.
          Farmers zoom to their lease, read the color gradient, click a
          station dot for exact SSI score and risk class.

Outputs:
  SSI_Heatmap_Raster  -- continuous EBK surface, styled as color gradient
  SSI_Station_Points  -- station dots with full SSI attributes for pop-ups

Method  : EBK (already run) -> stretch/clip -> publish as imagery layer
          Station points -> attribute-rich feature layer for pop-ups
=============================================================================
"""

import arcpy
import os

GDB      = r"D:\SeaGrant\SitingToolSalinity\ssijan\ssijan.gdb"
MASK     = "SC_coastline_kc_Buf_Dissolve"

arcpy.env.workspace        = GDB
arcpy.env.scratchWorkspace = GDB
arcpy.env.overwriteOutput  = True

# Use the smoothed EBK raster already produced by the previous script
# (skip re-running EBK -- ebk_ssi_smooth is already in your GDB)
EBK_SMOOTH = os.path.join(GDB, "ebk_ssi_smooth")

print("=" * 60)
print("SSI HEATMAP FINALIZATION")
print("=" * 60)


# -- STEP 1: VERIFY EBK SMOOTH EXISTS ----------------------------------------
# If you need to re-run EBK first, run ssi_finalize.py through Step 3,
# then come back and run this script from Step 2 onward.

if not arcpy.Exists(EBK_SMOOTH):
    raise FileNotFoundError(
        "ebk_ssi_smooth not found. Run ssi_finalize.py Steps 1-3 first."
    )

desc = arcpy.Describe(EBK_SMOOTH)
print(f"\nUsing: {EBK_SMOOTH}")
print(f"Spatial ref: {desc.spatialReference.name}")

# Check actual SSI value range in the raster
min_val = float(arcpy.GetRasterProperties_management(EBK_SMOOTH, "MINIMUM").getOutput(0))
max_val = float(arcpy.GetRasterProperties_management(EBK_SMOOTH, "MAXIMUM").getOutput(0))
print(f"SSI range in raster: {min_val:.1f} - {max_val:.1f}")


# -- STEP 2: COPY AS FINAL HEATMAP RASTER ------------------------------------
# Copy ebk_ssi_smooth to a clean named output for publishing.
# This is the layer you share to AGOL as an imagery/tile layer.

print("\nStep 2: Saving final heatmap raster...")

heatmap_raster = os.path.join(GDB, "SSI_Heatmap_Raster")
arcpy.management.CopyRaster(
    in_raster        = EBK_SMOOTH,
    out_rasterdataset= heatmap_raster,
    pixel_type       = "32_BIT_FLOAT",   # preserve full decimal precision
)
print(f"  -> Saved: {heatmap_raster}")


# -- STEP 3: BUILD STATION POINT LAYER ----------------------------------------
# Clean, attribute-rich point layer for pop-ups.
# Each station dot shows: SSI score, stability class, risk level,
# all four sub-scores, mean salinity, observation count.
# Farmers click a dot near their lease and get the full picture.

print("\nStep 3: Building station point layer...")

# Start from the clean exported feature class (has SSI_Score as DOUBLE)
stations_source = os.path.join(GDB, "DES_Stations_SSI")

ssi_points = os.path.join(GDB, "SSI_Station_Points")
arcpy.conversion.ExportFeatures(
    in_features  = stations_source,
    out_features = ssi_points,
)

# Add display-ready fields for pop-ups
new_fields = [
    ("SSI_Display",      "DOUBLE",  None),   # clean SSI score
    ("Stability_Class",  "TEXT",    30),
    ("Risk_Level",       "TEXT",    30),
    ("Risk_Color",       "TEXT",    10),      # hex for dot color
    ("Mean_Sal_Display", "DOUBLE",  None),
    ("CV_Display",       "DOUBLE",  None),
    ("OR_Score_Display", "DOUBLE",  None),
    ("EES_Display",      "DOUBLE",  None),
    ("Duration_Display", "DOUBLE",  None),
    ("N_Obs",            "LONG",    None),
    ("Pop_Title",        "TEXT",    100),     # pop-up headline
]

existing_fields = [f.name for f in arcpy.ListFields(ssi_points)]
for fname, ftype, flen in new_fields:
    if fname in existing_fields:
        print("  Skipping existing field: {}".format(fname))
        continue
    kwargs = {"field_length": flen} if flen else {}
    arcpy.management.AddField(ssi_points, fname, ftype, **kwargs)

# Color ramp matching the heatmap: low SSI = red, high SSI = blue
def ssi_to_hex(ssi):
    if ssi is None:
        return "#cccccc"
    if ssi >= 90: return "#2166ac"   # dark blue   -- Excellent
    if ssi >= 75: return "#67a9cf"   # light blue  -- Good
    if ssi >= 60: return "#fddbc7"   # pale orange -- Moderate
    if ssi >= 45: return "#ef8a62"   # orange      -- Poor
    return              "#b2182b"    # red         -- Unsuitable

def ssi_to_class(ssi):
    if ssi is None: return "No Data"
    if ssi >= 90: return "Excellent (90-100)"
    if ssi >= 75: return "Good (75-89)"
    if ssi >= 60: return "Moderate (60-74)"
    if ssi >= 45: return "Poor (45-59)"
    return "Unsuitable (< 45)"

def ssi_to_risk(ssi):
    if ssi is None: return "No Data"
    if ssi >= 90: return "Minimal Risk"
    if ssi >= 75: return "Low Risk"
    if ssi >= 60: return "Moderate Risk"
    if ssi >= 45: return "High Risk"
    return "Very High Risk"

def safe_float(val):
    try:
        return float(val)
    except (TypeError, ValueError):
        return None

fields = [
    "ssi", "mean_sal", "cv", "or_score",
    "extreme_event_score", "duration_score", "n_observations",
    "SSI_Display", "Stability_Class", "Risk_Level", "Risk_Color",
    "Mean_Sal_Display", "CV_Display", "OR_Score_Display",
    "EES_Display", "Duration_Display", "N_Obs", "Pop_Title",
    "L4Shellfish_Harvest_STAT"
]

with arcpy.da.UpdateCursor(ssi_points, fields) as cursor:
    for row in cursor:
        ssi     = safe_float(row[0])
        mean_s  = safe_float(row[1])
        cv      = safe_float(row[2])
        or_s    = safe_float(row[3])
        ees     = safe_float(row[4])
        dur     = safe_float(row[5])
        n_obs   = row[6]
        station = row[18] or "Unknown"

        row[7]  = round(ssi, 1)           if ssi    else None
        row[8]  = ssi_to_class(ssi)
        row[9]  = ssi_to_risk(ssi)
        row[10] = ssi_to_hex(ssi)
        row[11] = round(mean_s, 1)        if mean_s else None
        row[12] = round(cv, 1)            if cv     else None
        row[13] = round(or_s, 1)          if or_s   else None
        row[14] = round(ees, 1)           if ees    else None
        row[15] = round(dur, 1)           if dur    else None
        row[16] = int(n_obs)              if n_obs  else None
        row[17] = "Station {} -- SSI {:.1f} ({})".format(
                      station, ssi, ssi_to_risk(ssi)) if ssi else station

        cursor.updateRow(row)

count = int(arcpy.management.GetCount(ssi_points)[0])
print(f"  -> {count} station points with full SSI attributes")
print(f"  -> Saved: {ssi_points}")


# -- STEP 4: REPAIR GEOMETRY --------------------------------------------------
print("\nStep 4: Repairing station geometry...")
arcpy.management.RepairGeometry(ssi_points)
print("  -> Done")


# -- DONE ---------------------------------------------------------------------

print(f"""
{'=' * 60}
HEATMAP FINALIZATION COMPLETE
{'=' * 60}

Two layers to publish:

  1. SSI_Heatmap_Raster  (imagery/tile layer)
     Path: {heatmap_raster}
     -> The continuous color gradient across the coast

  2. SSI_Station_Points  (feature layer)
     Path: {ssi_points}
     -> Station dots with full pop-up data

PUBLISHING TO AGOL
------------------
Layer 1 -- Heatmap Raster:
  Right-click SSI_Heatmap_Raster in Catalog
  > Share As Web Layer
  > Layer type: TILE layer  (NOT feature -- rasters need tile)
  > Name: "SC Salinity Stability Index -- Heatmap"

  Symbology BEFORE publishing (do this in ArcGIS Pro first):
  - Stretch type: Minimum-Maximum
  - Color ramp: Red to Blue (or diverging Red-White-Blue)
  - Min value: set to {min_val:.0f} (actual raster minimum)
  - Max value: set to 100
  - Check "Invert" so red = low SSI, blue = high SSI

Layer 2 -- Station Points:
  Right-click SSI_Station_Points in Catalog
  > Share As Web Layer
  > Layer type: FEATURE layer
  > Name: "SC Salinity Stability Index -- Stations"

  Symbology: Unique Values on Stability_Class
  Use Risk_Color field hex values for dot fill colors

IN EXPERIENCE BUILDER
---------------------
Add both layers to your map widget:
  - Heatmap raster as base (bottom)
  - Station points on top

Configure pop-up for station points:
  Field             Display Label
  --------          -------------
  Pop_Title         (use as title)
  SSI_Display       SSI Score
  Stability_Class   Stability Class
  Risk_Level        Risk Level
  Mean_Sal_Display  Mean Salinity (ppt)
  OR_Score_Display  Optimal Range Score
  CV_Display        CV Stability Score
  EES_Display       Extreme Event Score
  Duration_Display  Duration Score
  N_Obs             Readings in Dataset

FARMER-FACING INTERPRETATION
------------------------------
  90-100  Excellent / Minimal Risk    Highly suitable for oyster farming
  75-89   Good / Low Risk             Well suited, minor variability
  60-74   Moderate / Moderate Risk    Viable with awareness of variability
  45-59   Poor / High Risk            Challenging -- stress events likely
  < 45    Unsuitable / Very High Risk Not recommended for cultivation
""")
