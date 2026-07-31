# =============================================================================
# development_change_analysis.py
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Watershed drivers
# Used in     : Ch. 4 Methods (Watershed Drivers)
# Status      : FINAL (July 2026)
#
# PURPOSE
#   Tests whether CHANGE in development 1997-2023 is associated with change in
#   salinity behaviour and with storm responsiveness. Extracts developed land
#   in 1 km and 5 km station buffers at both epochs, differences them, and
#   benchmarks change against static development.
#
# INPUTS      : Annual NLCD FctImp 1997 and 2023; MK_results_all_stations.csv; wq_period_change.csv; salinity_exposure_and_storm_response.csv
# OUTPUTS     : development_change_results.csv
# RUN AFTER   : salinity_exposure_storm_response.R
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

#!/usr/bin/env python3
"""
development_change_analysis.py
Kate Chatman | MPA/EVSS Thesis | College of Charleston / SC Sea Grant
July 2026

PURPOSE
    Test whether CHANGE in development between the late 1990s and early 2020s is
    associated with CHANGE in salinity behaviour and fecal coliform at SCDES
    shellfish monitoring stations.

WHY THIS DESIGN
    The existing watershed model regresses a 2021 land cover SNAPSHOT against a
    1997-2023 water quality TREND. A snapshot identifies where development IS,
    not where development HAPPENED. Those differ: a watershed built out by 1990
    scores high but changed little during the study period, while farmland that
    became subdivisions after 2000 scores moderate but changed enormously. The
    second is where a water quality signal would be expected. Differencing two
    years of impervious surface separates them.

STATUS
    Outcome side is BUILT (wq_period_change.csv).
    Exposure side needs TWO rasters. Only 2021 is currently on disk.

WHAT TO DOWNLOAD
    Annual NLCD Collection 1, Version 1 — Fractional Impervious Surface
    Source: MRLC (https://www.mrlc.gov/data) — Annual NLCD product suite.
    Need:  Annual_NLCD_FctImp_1997_CU_C1V1.tif   (early, matches WQ record start)
           Annual_NLCD_FctImp_2023_CU_C1V1.tif   (late,  matches WQ record end)
    You already have: Annual_NLCD_FctImp_2021_CU_C1V1.tif

    NOTE: the 2021 file on disk is the full CONUS tile (~970 MB). The MRLC
    viewer allows clipping to an area of interest before download, which for
    coastal SC would be a small fraction of that. Either works with this script.

    If 1997 is unavailable, 2001 is an acceptable early year — it costs the first
    four years of the water quality record but is a standard NLCD epoch.

USAGE
    Place the two rasters in ../data/nlcd_sc/ and set EARLY_TIF / LATE_TIF below.
    python3 development_change_analysis.py
"""

import numpy as np
import pandas as pd
import rasterio
from rasterio.windows import Window
from pyproj import Transformer
from scipy import stats
from scipy.spatial import cKDTree
import os, sys, re

# --- CONFIG ------------------------------------------------------------------
NLCD_DIR   = "../data/nlcd_sc/"
EARLY_YEAR = "1997"
LATE_YEAR  = "2023"
BUFFERS_M  = [1000, 5000]
NODATA     = 250

# Filenames are auto-detected by year, because the collection-version suffix
# varies: files already on disk are C1V1, while mrlc.gov now serves Collection 1
# Version 2 (C1V2). Both years used in the differencing MUST come from the same
# collection version — differencing across versions can manufacture apparent
# change that is only reprocessing. Download 1997 and 2023 together from the
# current site and do NOT mix in the older C1V1 2021 file.
# Files are located RECURSIVELY, so you can paste the extracted product folders
# straight from the MRLC zips into NLCD_DIR without flattening them, e.g.
#   data/nlcd_sc/Annual_NLCD_FctImp_1997_CU_C1V2/Annual_NLCD_FctImp_1997_CU_C1V2.tif
# Both layouts work.
import glob as _glob
def _find(year):
    pat = os.path.join(NLCD_DIR, "**", f"*FctImp*{year}*.tif")
    hits = [h for h in _glob.glob(pat, recursive=True)
            if not h.lower().endswith((".aux.xml", ".ovr", ".xml"))]
    # exclude other Annual NLCD products that also carry a year in the name
    hits = [h for h in hits if "FctImp" in os.path.basename(h)]
    if not hits:
        return os.path.join(NLCD_DIR, f"Annual_NLCD_FctImp_{year}_CU_C1V2.tif")
    if len(hits) > 1:
        print(f"NOTE: {len(hits)} candidate rasters found for {year}:")
        for h in sorted(hits): print("      ", os.path.relpath(h, NLCD_DIR))
        # prefer the highest collection version
        hits = sorted(hits, key=lambda h: (re.search(r"C1V(\d)", h).group(1)
                                           if re.search(r"C1V(\d)", h) else "0"))
        print("      -> using", os.path.relpath(hits[-1], NLCD_DIR))
    return hits[-1]

SCRIPTS    = "./"
WQ_CHANGE  = SCRIPTS + "wq_period_change.csv"          # built already
MK_SSI     = SCRIPTS + "MK_results_all_stations.csv"
STORM      = SCRIPTS + "salinity_exposure_and_storm_response.csv"

# --- CHECK INPUTS ------------------------------------------------------------
EARLY_TIF = _find(EARLY_YEAR)
LATE_TIF  = _find(LATE_YEAR)
missing = [f for f in (EARLY_TIF, LATE_TIF) if not os.path.exists(f)]
if missing:
    print("MISSING RASTER(S):")
    for f in missing: print("   ", f)
    print("\nDownload from mrlc.gov/data -> Projects: Annual NLCD ->")
    print("Products: Fractional Impervious Surface -> years 1997 and 2023 (CONUS).")
    print("Place both in", NLCD_DIR, "and re-run. Everything else is ready.")
    sys.exit(1)

def _ver(p):
    m = re.search(r"C1V(\d)", os.path.basename(p))
    return m.group(1) if m else "?"
if _ver(EARLY_TIF) != _ver(LATE_TIF):
    print(f"WARNING: collection-version mismatch — early is C1V{_ver(EARLY_TIF)}, "
          f"late is C1V{_ver(LATE_TIF)}.")
    print("Differencing across collection versions can manufacture apparent change.")
    print("Re-download both years from the same version before trusting results.\n")
print(f"Early: {os.path.basename(EARLY_TIF)}\nLate : {os.path.basename(LATE_TIF)}\n")

# --- STATION COORDS ----------------------------------------------------------
mk = pd.read_csv(MK_SSI).dropna(subset=["LATITUDE", "LONGITUDE"])
stations = mk[["Station", "LATITUDE", "LONGITUDE", "Area", "mk_tau", "mean_ssi"]].copy()
stations = stations.rename(columns={"mk_tau": "ssi_tau"})

# --- EXTRACT DEVELOPED LAND AT BOTH EPOCHS -----------------------------------
def extract(tif, lats, lons, buffers):
    src = rasterio.open(tif)
    tr = Transformer.from_crs("EPSG:4326", src.crs, always_xy=True)
    xs, ys = tr.transform(lons, lats)
    rows, cols = rasterio.transform.rowcol(src.transform, xs, ys)
    rows, cols = np.array(rows), np.array(cols)
    out = {}
    for buf in buffers:
        npix = int(buf / abs(src.res[0]))
        imp, dev_any, dev20 = [], [], []
        for r_, c_ in zip(rows, cols):
            r0, c0, w = r_ - npix, c_ - npix, 2 * npix
            if r0 < 0 or c0 < 0 or r0 + w >= src.height or c0 + w >= src.width:
                imp.append(np.nan); dev_any.append(np.nan); dev20.append(np.nan); continue
            b = src.read(1, window=Window(c0, r0, w, w)).astype(float).ravel()
            b = b[b != NODATA]
            if b.size < 10:
                imp.append(np.nan); dev_any.append(np.nan); dev20.append(np.nan); continue
            imp.append(b.mean()); dev_any.append((b > 0).mean() * 100); dev20.append((b >= 20).mean() * 100)
        out[f"imp_{buf}"]     = imp
        out[f"devany_{buf}"]  = dev_any
        out[f"dev20_{buf}"]   = dev20
    src.close()
    return out

print("Extracting EARLY epoch...")
e = extract(EARLY_TIF, stations["LATITUDE"].values, stations["LONGITUDE"].values, BUFFERS_M)
print("Extracting LATE epoch...")
l = extract(LATE_TIF,  stations["LATITUDE"].values, stations["LONGITUDE"].values, BUFFERS_M)

for k, v in e.items(): stations[k + "_early"] = v
for k, v in l.items(): stations[k + "_late"]  = v
for buf in BUFFERS_M:
    for m in ["imp", "devany", "dev20"]:
        stations[f"d_{m}_{buf}"] = stations[f"{m}_{buf}_late"] - stations[f"{m}_{buf}_early"]

print("\n=== DEVELOPMENT CHANGE, EARLY -> LATE ===")
for buf in BUFFERS_M:
    s = stations[f"d_devany_{buf}"].dropna()
    print(f"  {buf}m buffer  Δ%developed: mean={s.mean():+6.2f}  median={s.median():+6.2f}  "
          f"p90={s.quantile(.9):+6.2f}  max={s.max():+6.2f}  n={len(s)}")

# --- JOIN OUTCOMES -----------------------------------------------------------
wq = pd.read_csv(WQ_CHANGE)
d = stations.merge(wq, on="Station", how="left")

st = pd.read_csv(STORM).dropna(subset=["lat", "lon", "storm_response_ppt_per_inch"])
t = cKDTree(st[["lat", "lon"]].values)
dist, i = t.query(d[["LATITUDE", "LONGITUDE"]].values, k=1)
d["storm_dist_km"] = dist * 111
d["storm_response"] = st["storm_response_ppt_per_inch"].values[i]
d.loc[d.storm_dist_km > 2.0, "storm_response"] = np.nan

# --- TESTS -------------------------------------------------------------------
def test(x, y, lab, df):
    s = df.dropna(subset=[x, y])
    if len(s) < 25:
        print(f"  {lab:<52} n too small ({len(s)})"); return
    r, p   = stats.pearsonr(s[x], s[y])
    rs, ps = stats.spearmanr(s[x], s[y])
    print(f"  {lab:<52} r={r:+.3f} p={p:.4f} | rho={rs:+.3f} p={ps:.4f} | n={len(s)}")

print("\n=== CHANGE IN DEVELOPMENT vs CHANGE IN WATER QUALITY ===")
for buf in BUFFERS_M:
    print(f"\n-- {buf} m buffer --")
    test(f"d_devany_{buf}", "d_salinity",  "Δ developed  vs  Δ mean salinity",        d)
    test(f"d_devany_{buf}", "d_sal_sd",    "Δ developed  vs  Δ salinity variability", d)
    test(f"d_devany_{buf}", "d_fc",        "Δ developed  vs  Δ FC geomean",           d)
    test(f"d_devany_{buf}", "ssi_tau",     "Δ developed  vs  SSI MK trend",           d)
    test(f"d_devany_{buf}", "storm_response", "Δ developed  vs  storm responsiveness", d)

print("\n=== BENCHMARK: STATIC (late-epoch) DEVELOPMENT, same outcomes ===")
print("    (if Δ outperforms static, the change design is doing real work)")
for buf in BUFFERS_M:
    print(f"\n-- {buf} m buffer, STATIC --")
    test(f"devany_{buf}_late", "d_salinity", "static developed  vs  Δ mean salinity",  d)
    test(f"devany_{buf}_late", "d_fc",       "static developed  vs  Δ FC geomean",     d)
    test(f"devany_{buf}_late", "ssi_tau",    "static developed  vs  SSI MK trend",     d)

# --- AREA-LEVEL (guards against spatial pseudo-replication) ------------------
print("\n=== AREA-LEVEL (guards against spatial autocorrelation) ===")
for buf in BUFFERS_M:
    a = d.dropna(subset=[f"d_devany_{buf}", "d_salinity"]).groupby("Area").agg(
        dev=(f"d_devany_{buf}", "mean"), sal=("d_salinity", "mean"),
        fc=("d_fc", "mean"), tau=("ssi_tau", "mean")).reset_index()
    if len(a) >= 8:
        for oc, lab in [("sal", "Δ salinity"), ("fc", "Δ FC"), ("tau", "SSI tau")]:
            s = a.dropna(subset=["dev", oc])
            r, p = stats.pearsonr(s["dev"], s[oc])
            print(f"  {buf}m  Δdeveloped vs {lab:<12} r={r:+.3f} p={p:.3f} (n={len(s)} areas)")

d.to_csv("development_change_results.csv", index=False)
print("\nWritten: development_change_results.csv")
