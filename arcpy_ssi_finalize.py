# =============================================================================
# arcpy_ssi_finalize.py
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Siting tool (ArcGIS)
# Used in     : Applied product — mariculture siting tool
# Status      : FINAL — EBK-based, predates the IPDW surfaces
#
# PURPOSE
#   Produces the publication-ready, web-deployable salinity stability polygon
#   layer for ArcGIS Online / Experience Builder. Method: EBK, mask, focal
#   smooth, classify, vectorise.
#
# INPUTS      : DES_Stations_KC joined to Merged_SSI_Results.csv (file geodatabase)
# OUTPUTS     : SSI_Final_Polygons feature class for AGOL
# RUN AFTER   : ssi_calculate.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

"""
=============================================================================
SSI LAYER FINALIZATION — SC Mariculture Siting Tool
=============================================================================
Purpose : Produce a publication-ready, web-deployable salinity stability
          polygon layer for ArcGIS Online / Experience Builder.
Method  : EBK -> mask -> focal smooth -> classify -> vector polygons
Input   : DES_Stations_KC (point layer joined to Merged_SSI_Results.csv)
Output  : SSI_Final_Polygons -- feature class ready for AGOL upload
Projection: NAD 1983 UTM Zone 17N (all inputs must match)
=============================================================================
CHANGE LOG
  2025-11  Initial IDW attempts (power=1, cell=500m)
  2026-01  IDW revised (power=2, cell=813m)
  2026-01  Switched to EBK for scientific defensibility -- this script
           Fix: export join before EBK to resolve joined-field type error
=============================================================================
"""

import arcpy
import os

# -- 0. PATHS -- edit these to match your machine -----------------------------

GDB      = r"D:\SeaGrant\SitingToolSalinity\ssijan\ssijan.gdb"
STATIONS = r"DES_Stations_KC\DES_Stations_KC"   # point layer WITH SSI join active
MASK     = "SC_coastline_kc_Buf_Dissolve"        # final coastal polygon mask

arcpy.env.workspace        = GDB
arcpy.env.scratchWorkspace = GDB
arcpy.env.overwriteOutput  = True

CELL_SIZE = 200   # meters -- fine enough to look smooth, coarse enough to be honest

print("=" * 60)
print("SSI LAYER FINALIZATION")
print("=" * 60)


# -- PRE-STEP: EXPORT JOIN -> CREATE TRUE NUMERIC FIELD -----------------------
#
# Root cause of ERROR 003911:
#   Fields inherited from a CSV table join are stored internally as text
#   by ArcGIS, even when the values are numeric. EBK requires a true
#   FLOAT or DOUBLE field and rejects joined fields regardless of content.
#
# Fix:
#   1. ExportFeatures materializes the join into a real standalone feature
#      class -- all joined fields become proper typed columns.
#   2. We then add a new DOUBLE field and explicitly copy ssi into it.
#   3. Rows with no SSI match (null) are dropped before interpolation.

print("\nPre-step: Exporting joined layer to resolve field type error...")

stations_clean = os.path.join(GDB, "DES_Stations_SSI")

arcpy.conversion.ExportFeatures(
    in_features  = STATIONS,
    out_features = stations_clean,
)

# The joined field name after export usually drops the table prefix.
# If the next step errors, run this to check the actual field name:
#   print([f.name for f in arcpy.ListFields(stations_clean)])
# Then update SSI_SOURCE_FIELD below to match exactly.
SSI_SOURCE_FIELD = "ssi"   # confirmed -- type is String, cast handled below

# Add a clean DOUBLE field -- this is what EBK will use
arcpy.management.AddField(stations_clean, "SSI_Score", "DOUBLE")

# ssi exported as String (length 8000) because R wrote high-precision decimals
# that ArcGIS ingested as text from the CSV. Cast explicitly to float here.
arcpy.management.CalculateField(
    in_table        = stations_clean,
    field           = "SSI_Score",
    expression      = "float(!ssi!) if !ssi! not in (None, '', ' ') else None",
    expression_type = "PYTHON3"
)

# Drop unmatched stations (null SSI = not in merged results table)
with arcpy.da.UpdateCursor(stations_clean, ["SSI_Score"]) as cursor:
    for row in cursor:
        if row[0] is None:
            cursor.deleteRow()

count = int(arcpy.management.GetCount(stations_clean)[0])
print("  -> {} stations with valid SSI scores".format(count))
print("  -> Saved: {}".format(stations_clean))


# -- STEP 1: EMPIRICAL BAYESIAN KRIGING ---------------------------------------
#
# Why EBK over IDW:
#   IDW weights purely by distance -- close stations dominate, producing
#   bulls-eye halos. EBK instead fits a semivariogram to the actual spatial
#   autocorrelation structure of your SSI data, so weights reflect how
#   salinity actually varies across SC estuaries. No bulls-eyes.
#
#   Methodologically: "parameters selected empirically via EBK" is far
#   more defensible than "power=2, radius=75km chosen by trial and error."
#
# Key parameters:
#   transformation_type="EMPIRICAL" -- best for bounded 0-100 scores
#   max_local_points=100            -- semivariogram subset size, standard for ~466 pts
#   overlap_factor=1                -- moderate subset overlap
#   number_semivariograms=100       -- more = better uncertainty (~3 min runtime)
#   RADIUS=75000m                   -- 75km captures regional salinity structure

print("\nStep 1: Running Empirical Bayesian Kriging (~2-3 minutes)...")

ebk_layer  = "ebk_ssi_layer"
ebk_raster = os.path.join(GDB, "ebk_ssi_raster")

arcpy.ga.EmpiricalBayesianKriging(
    in_features           = stations_clean,
    z_field               = "SSI_Score",
    out_ga_layer          = ebk_layer,
    out_raster            = ebk_raster,
    cell_size             = CELL_SIZE,
    transformation_type   = "EMPIRICAL",
    max_local_points      = 100,
    overlap_factor        = 1,
    number_semivariograms = 100,
    search_neighborhood   = (
        "NBRTYPE=StandardCircular RADIUS=75000 "
        "ANGLE=0 NBR_MAX=15 NBR_MIN=10 SECTOR_TYPE=ONE_SECTOR"
    ),
)

print("  -> EBK raster saved: {}".format(ebk_raster))


# -- STEP 2: EXTRACT BY MASK --------------------------------------------------
# Clips interpolated surface to your coastal boundary.
# Values outside SC_coastline_kc_Buf_Dissolve become NoData.

print("\nStep 2: Extracting by mask...")

ebk_masked = os.path.join(GDB, "ebk_ssi_masked")

with arcpy.EnvManager(snapRaster=ebk_raster, cellSize=CELL_SIZE):
    out = arcpy.sa.ExtractByMask(
        in_raster       = ebk_raster,
        in_mask_data    = MASK,
        extraction_area = "INSIDE"
    )
    out.save(ebk_masked)

print("  -> Masked raster saved: {}".format(ebk_masked))


# -- STEP 3: FOCAL STATISTICS SMOOTHING ---------------------------------------
# 3-cell radius = 600m neighborhood mean.
# Removes residual speckle without over-blurring the natural salinity gradient.
# Defensible as local averaging -- window is smaller than average station spacing.

print("\nStep 3: Focal smoothing (600m radius)...")

ebk_smooth = os.path.join(GDB, "ebk_ssi_smooth")

smoothed = arcpy.sa.FocalStatistics(
    in_raster       = ebk_masked,
    neighborhood    = arcpy.sa.NbrCircle(3, "CELL"),
    statistics_type = "MEAN",
    ignore_nodata   = "DATA"
)
smoothed.save(ebk_smooth)

print("  -> Smoothed raster saved: {}".format(ebk_smooth))


# -- STEP 4: RECLASSIFY INTO 5 SSI BANDS -------------------------------------
# Converts continuous SSI surface into the 5 published stability classes.
# Thresholds match SSI scoring system exactly -- no new classification decisions.
# Stored as integers 1-5 for clean polygon conversion.

print("\nStep 4: Reclassifying into 5 SSI bands...")

ebk_classified = os.path.join(GDB, "ebk_ssi_classified")

remap = arcpy.sa.RemapRange([
    [0,  45,  1],   # Unsuitable  -- Very High Risk
    [45, 60,  2],   # Poor        -- High Risk
    [60, 75,  3],   # Moderate    -- Moderate Risk
    [75, 90,  4],   # Good        -- Low Risk
    [90, 101, 5],   # Excellent   -- Minimal Risk
])

classified = arcpy.sa.Reclassify(
    in_raster      = ebk_smooth,
    reclass_field  = "VALUE",
    remap          = remap,
    missing_values = "NODATA"
)
classified.save(ebk_classified)

print("  -> Classified raster saved: {}".format(ebk_classified))


# -- STEP 5: RASTER TO POLYGON ------------------------------------------------
# SIMPLIFY=True smooths polygon edges for a polished appearance.
# MULTIPLE_OUTER_PART handles SC's complex coastal geometry correctly.

print("\nStep 5: Converting to vector polygons...")

ssi_polygons_raw = os.path.join(GDB, "SSI_Polygons_Raw")

arcpy.conversion.RasterToPolygon(
    in_raster                 = ebk_classified,
    out_polygon_features      = ssi_polygons_raw,
    simplify                  = "SIMPLIFY",
    raster_field              = "Value",
    create_multipart_features = "MULTIPLE_OUTER_PART",
)

print("  -> Raw polygons saved: {}".format(ssi_polygons_raw))


# -- STEP 6: ADD ATTRIBUTE FIELDS ---------------------------------------------
# Writes SSI labels, risk levels, score ranges, and hex colors directly into
# the attribute table. No join needed after publishing -- pop-ups and
# symbology work immediately in Experience Builder.

print("\nStep 6: Adding SSI attribute fields...")

for fname, ftype, flen in [
    ("SSI_Class",  "TEXT",  30),
    ("Risk_Level", "TEXT",  30),
    ("SSI_Min",    "SHORT", None),
    ("SSI_Max",    "SHORT", None),
    ("Hex_Color",  "TEXT",  10),
    ("Sort_Order", "SHORT", None),
]:
    kwargs = {"field_length": flen} if flen else {}
    arcpy.management.AddField(ssi_polygons_raw, fname, ftype, **kwargs)

# gridcode 1-5 -> human-readable attributes
# Color ramp: RdBu diverging -- perceptually uniform, colorblind-safe
CLASS_MAP = {
    1: ("Unsuitable (< 45)",  "Very High Risk",  0,  44, "#b2182b", 1),
    2: ("Poor (45-59)",        "High Risk",      45,  59, "#ef8a62", 2),
    3: ("Moderate (60-74)",    "Moderate Risk",  60,  74, "#fddbc7", 3),
    4: ("Good (75-89)",        "Low Risk",       75,  89, "#67a9cf", 4),
    5: ("Excellent (90-100)",  "Minimal Risk",   90, 100, "#2166ac", 5),
}

fields = ["gridcode", "SSI_Class", "Risk_Level",
          "SSI_Min", "SSI_Max", "Hex_Color", "Sort_Order"]

with arcpy.da.UpdateCursor(ssi_polygons_raw, fields) as cursor:
    for row in cursor:
        attrs = CLASS_MAP.get(row[0])
        if attrs:
            row[1:] = list(attrs)
            cursor.updateRow(row)

print("  -> Attribute fields populated")


# -- STEP 7: PAIRWISE DISSOLVE ------------------------------------------------
# Merges fragmented same-class polygons into clean contiguous zones.
# Reduces feature count significantly -- faster AGOL rendering.

print("\nStep 7: Dissolving by SSI class...")

ssi_final = os.path.join(GDB, "SSI_Final_Polygons")

arcpy.analysis.PairwiseDissolve(
    in_features       = ssi_polygons_raw,
    out_feature_class = ssi_final,
    dissolve_field    = ["gridcode", "SSI_Class", "Risk_Level",
                         "SSI_Min", "SSI_Max", "Hex_Color", "Sort_Order"],
    multi_part        = "MULTI_PART"
)

print("  -> Final polygons saved: {}".format(ssi_final))


# -- STEP 8: REPAIR GEOMETRY --------------------------------------------------
# Fixes slivers or topology issues introduced during raster-to-vector
# conversion. Always run before AGOL upload.

print("\nStep 8: Repairing geometry...")
arcpy.management.RepairGeometry(ssi_final)
print("  -> Geometry repaired")


# -- DONE ---------------------------------------------------------------------

count = int(arcpy.management.GetCount(ssi_final)[0])
print("""
{}
FINALIZATION COMPLETE
{}

SSI_Final_Polygons: {} features
Location: {}

NEXT STEPS -- AGOL UPLOAD
--------------------------
1. Catalog pane > right-click SSI_Final_Polygons
   > Share As Web Layer > Feature Layer
2. In Experience Builder:
   - Symbology: Unique Values on SSI_Class field
   - Fill colors from Hex_Color field values (see below)
   - Pop-up fields: SSI_Class, Risk_Level, SSI_Min, SSI_Max

SYMBOLOGY (RdBu diverging -- colorblind safe)
----------------------------------------------
  Excellent (90-100)  #2166ac  dark blue
  Good (75-89)        #67a9cf  light blue
  Moderate (60-74)    #fddbc7  pale orange
  Poor (45-59)        #ef8a62  orange
  Unsuitable (<45)    #b2182b  red

IF SSI_SOURCE_FIELD IS WRONG
-----------------------------
Run this first to check the exported field names:
  print([f.name for f in arcpy.ListFields("DES_Stations_SSI")])
Update SSI_SOURCE_FIELD near the top of this script and re-run.

NOTE: 127 stations had no coordinates and are excluded from the
interpolation. Publish DES_Stations_KC as a companion point layer
so users can click individual stations for exact SSI values.
""".format("=" * 60, "=" * 60, count, ssi_final))
