# =============================================================================
# landcover_change_extraction.py
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Watershed drivers
# Used in     : Ch. 4 Methods (Watershed Drivers)
# Status      : FINAL (July 2026)
#
# PURPOSE
#   Extracts categorical NLCD land cover at 1997 and 2023 in station buffers
#   and computes the conversion matrix, identifying which land cover types
#   became developed.
#
# INPUTS      : Annual NLCD LndCov 1997 and 2023; MK_results_all_stations.csv
# OUTPUTS     : landcover_change_stations.csv
# RUN AFTER   : None — standalone
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

import rasterio, numpy as np, pandas as pd, os
from rasterio.windows import Window
from pyproj import Transformer
B="/sessions/adoring-focused-darwin/mnt/thesis/ssiworkthesis/data/nlcd_sc/"
S="/sessions/adoring-focused-darwin/mnt/thesis/ssiworkthesis/scripts/"
E=f"{B}Annual_NLCD_LndCov_1997_CU_C1V2/Annual_NLCD_LndCov_1997_CU_C1V2.tif"
L=f"{B}Annual_NLCD_LndCov_2023_CU_C1V2/Annual_NLCD_LndCov_2023_CU_C1V2.tif"
DEV={21,22,23,24}
GROUPS={"forest":{41,42,43},"wetland":{90,95},"ag":{81,82},"shrubgrass":{52,71},
        "water":{11},"barren":{31}}
se=rasterio.open(E); sl=rasterio.open(L)
assert se.shape==sl.shape and se.transform==sl.transform, "grids differ"
mk=pd.read_csv(S+"MK_results_all_stations.csv").dropna(subset=["LATITUDE","LONGITUDE"])
tr=Transformer.from_crs("EPSG:4326",se.crs,always_xy=True)
xs,ys=tr.transform(mk["LONGITUDE"].values,mk["LATITUDE"].values)
rows,cols=rasterio.transform.rowcol(se.transform,xs,ys); rows=np.array(rows);cols=np.array(cols)
out=[]
for buf in [1000,5000]:
    npix=int(buf/30)
    rec={k:[] for k in ["pct_dev_1997","pct_dev_2023","d_pct_dev","n_valid"]}
    for g in GROUPS: rec[f"conv_{g}_to_dev"]=[]
    for r_,c_ in zip(rows,cols):
        r0,c0,w=r_-npix,c_-npix,2*npix
        if r0<0 or c0<0 or r0+w>=se.height or c0+w>=se.width:
            for k in rec: rec[k].append(np.nan); continue
        a=se.read(1,window=Window(c0,r0,w,w)).ravel()
        b=sl.read(1,window=Window(c0,r0,w,w)).ravel()
        m=(a!=250)&(b!=250)
        a=a[m]; b=b[m]
        if a.size<10:
            for k in rec: rec[k].append(np.nan); continue
        dev_a=np.isin(a,list(DEV)); dev_b=np.isin(b,list(DEV))
        rec["pct_dev_1997"].append(dev_a.mean()*100)
        rec["pct_dev_2023"].append(dev_b.mean()*100)
        rec["d_pct_dev"].append((dev_b.mean()-dev_a.mean())*100)
        rec["n_valid"].append(a.size)
        newdev=dev_b&(~dev_a)
        for g,cls in GROUPS.items():
            rec[f"conv_{g}_to_dev"].append((newdev&np.isin(a,list(cls))).mean()*100)
    for k,v in rec.items(): mk[f"{k}_{buf}"]=v
    print(f"buffer {buf}m done",flush=True)
mk.to_csv("/tmp/work/landcover_change_stations.csv",index=False)
print("WROTE /tmp/work/landcover_change_stations.csv")
