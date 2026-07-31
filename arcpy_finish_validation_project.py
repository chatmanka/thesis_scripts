# =============================================================================
# arcpy_finish_validation_project.py
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Validation (ArcGIS)
# Used in     : Ch. 5 Validation — map production
# Status      : FINAL
#
# PURPOSE
#   Completes the SSI_Mortality_Validation ArcGIS project: applies existing
#   .lyrx symbology to the three rasters and symbolises sentinel sites by the
#   concordance field.
#
# INPUTS      : SSI_Mortality_Validation.aprx; .lyrx symbology files
# OUTPUTS     : Symbolised ArcGIS project
# RUN AFTER   : ssi_biological_validation.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# =============================================================================
# FINISH SSI_Mortality_Validation PROJECT
# Kate Chatman | MPA/EVSS | College of Charleston / SC Sea Grant Consortium
# Date: July 2026
#
# PURPOSE: Completes the SSI_Mortality_Validation project setup. Does the three
#          things left undone when the project was built interactively:
#            1. Applies your existing .lyrx symbology to the three rasters
#            2. Symbolizes the sentinel sites by the `concordance` field
#            3. Adds WBDHU8 + WBDHU12 from the remaining three NHD basins
#
# HOW TO RUN: Open SSI_Mortality_Validation.aprx in ArcGIS Pro.
#             View tab > Python window (or Analysis > Python).
#             Paste this whole script and press Enter.
#
# NOTE: Run with the project OPEN. "CURRENT" refers to the open project.
# =============================================================================

import arcpy, os

BASE   = r"C:\Users\chatm\OneDrive - College of Charleston\Desktop\thesis\ssiworkthesis"
LYR    = os.path.join(BASE, "data", "layerfiles")
NHD    = os.path.join(BASE, "data", "nhd_sc")

aprx = arcpy.mp.ArcGISProject("CURRENT")
m    = aprx.listMaps("Map")[0]

# -----------------------------------------------------------------------------
# 1. RASTER SYMBOLOGY FROM YOUR EXISTING .lyrx FILES
# -----------------------------------------------------------------------------
raster_symbology = {
    "ipdw_ssi_classified.tif":     "ipdw_ssi_classified.tif.lyrx",
    "ipdw_essi_surface_final.tif": "ipdw_essi_surface_final.tif.lyrx",
    "ipdw_ssi_surface_final.tif":  "IPDW SSI.lyrx",
}

for lyr_name, lyrx_file in raster_symbology.items():
    lyrx_path = os.path.join(LYR, lyrx_file)
    if not os.path.exists(lyrx_path):
        print(f"  SKIP (missing .lyrx): {lyrx_file}")
        continue
    hits = m.listLayers(lyr_name)
    if not hits:
        print(f"  SKIP (layer not in map): {lyr_name}")
        continue
    try:
        arcpy.management.ApplySymbologyFromLayer(hits[0], lyrx_path)
        print(f"  OK  symbology applied: {lyr_name}  <-  {lyrx_file}")
    except Exception as e:
        print(f"  FAIL {lyr_name}: {e}")

# -----------------------------------------------------------------------------
# 2. SYMBOLIZE SENTINEL SITES BY CONCORDANCE
#    Four categories. Discordant sites deliberately get the loud colors --
#    they are the three worth asking Gary about (CRM, NHI, CBG).
# -----------------------------------------------------------------------------
COLORS = {
    "Concordant - stable/low volatility":   {"RGB": [ 43, 131, 186, 100]},  # blue
    "Concordant - unstable/high volatility":{"RGB": [215,  25,  28, 100]},  # red
    "DISCORDANT - high SSI but volatile":   {"RGB": [255, 165,   0, 100]},  # orange
    "DISCORDANT - low SSI but stable":      {"RGB": [128,   0, 128, 100]},  # purple
}

pts = m.listLayers("sentinel_sites_ssi_mortality")
if pts:
    lyr = pts[0]
    sym = lyr.symbology
    if hasattr(sym, "updateRenderer"):
        sym.updateRenderer("UniqueValueRenderer")
        sym.renderer.fields = ["concordance"]
        for grp in sym.renderer.groups:
            for itm in grp.items:
                val = itm.values[0][0]
                if val in COLORS:
                    itm.symbol.color = COLORS[val]
                    itm.symbol.size  = 10 if val.startswith("DISCORDANT") else 7
                    itm.symbol.outlineColor = {"RGB": [0, 0, 0, 100]}
                    itm.symbol.outlineWidth = 0.5
                    itm.label = val
        lyr.symbology = sym
        print("  OK  sentinel sites symbolized by concordance")
else:
    print("  SKIP sentinel_sites_ssi_mortality not found")

# -----------------------------------------------------------------------------
# 3. ADD REMAINING NHD BASINS (0304, 0306, 0307 -- 0305 already loaded)
#    HU8 = subbasin, the right scale for the ocean-dominated vs
#    river-dominated typology. HU12 = subwatershed detail.
# -----------------------------------------------------------------------------
for basin in ["0304", "0306", "0307"]:
    gdb = os.path.join(NHD, f"NHDPLUS_H_{basin}_HU4_GDB.gdb")
    if not os.path.exists(gdb):
        print(f"  SKIP (no gdb): {basin}")
        continue
    for hu in ["WBDHU8", "WBDHU12"]:
        fc = os.path.join(gdb, "WBD", hu)
        try:
            if arcpy.Exists(fc):
                added = m.addDataFromPath(fc)
                added.name = f"{hu}_{basin}"
                # hollow fill so it never obscures the rasters
                s = added.symbology
                s.renderer.symbol.color        = {"RGB": [0, 0, 0, 0]}
                s.renderer.symbol.outlineColor = {"RGB": [80, 80, 80, 100]}
                s.renderer.symbol.outlineWidth = 0.7
                added.symbology = s
                print(f"  OK  added {hu}_{basin}")
            else:
                print(f"  SKIP (not found): {basin}/{hu}")
        except Exception as e:
            print(f"  FAIL {basin}/{hu}: {e}")

# -----------------------------------------------------------------------------
# 4. ALSO SET THE TWO ALREADY-LOADED 0305 WATERSHEDS TO HOLLOW
# -----------------------------------------------------------------------------
for nm in ["WBDHU12", "WBDHU8"]:
    for l in m.listLayers(nm):
        try:
            s = l.symbology
            s.renderer.symbol.color        = {"RGB": [0, 0, 0, 0]}
            s.renderer.symbol.outlineColor = {"RGB": [80, 80, 80, 100]}
            s.renderer.symbol.outlineWidth = 0.7
            l.symbology = s
            l.visible = True
            print(f"  OK  {nm} set hollow")
        except Exception as e:
            print(f"  FAIL {nm}: {e}")

aprx.save()
print("\nDone. Project saved.")
