# Script Reference — Thesis Appendix

**Closing the Governance Gap: Shellfish Mariculture in South Carolina**  
Kate Chatman | MPA/EVSS Dual Degree | College of Charleston / S.C. Sea Grant Consortium

Twenty-seven production scripts across eight thesis components. For each: what it does, what
it consumes, what it produces, what must run first, and where the output appears in the thesis.

---

## Contents

- **01_salinity_stability_index** — Salinity Stability Index (SSI) (4 scripts)
- **02_validation** — Biological Validation (7 scripts)
- **03_storm_responsiveness** — Storm Responsiveness (1 script)
- **04_watershed_drivers** — Watershed Drivers (3 scripts)
- **05_essi_composite** — ESSI Composite (1 script)
- **06_spatial_interpolation** — Spatial Interpolation and Siting Tool (5 scripts)
- **07_regulatory_gap** — Regulatory Gap Analysis (3 scripts)
- **08_network_analysis** — Stakeholder Network Analysis (3 scripts)
- **archive** — superseded and diagnostic scripts, not for citation

---


## Salinity Stability Index — Salinity Stability Index (SSI)

The core index and its temporal trend layer. Everything else in the thesis depends on these outputs.


### `ssi_calculate.R`

**Used in:** Ch. 4 Methods; Ch. 5 Results  
**Status:** FINAL

Calculates the four-factor Salinity Stability Index at station level from SCDES monitoring data. Components: Optimal Range Score (w=0.30), CV Stability Score (w=0.30), Extreme Event Score (w=0.25), Duration Score (w=0.15). Assigns ordinal suitability classes using manual breaks.

| | |
|---|---|
| **Inputs** | all_sc_wq.xlsx (61,681 obs, 593 stations, May-Sep 1997-2023) |
| **Outputs** | ssi_results.csv / .xlsx; SSI_Results_with_coords.csv |
| **Run after** | None — entry point |
| **Original filename** | `ssicode_april26.R` |


### `ssi_annual_mannkendall.R`

**Used in:** Ch. 4 Methods (Temporal Trend Analysis); ESSI Component 2  
**Status:** FINAL

Extends the static SSI by computing annual SSI scores per station 1997-2023, then applying a Mann-Kendall trend test to each station time series. Produces the SSI Trend layer.

| | |
|---|---|
| **Inputs** | all_sc_wq.xlsx; ssi_results.csv |
| **Outputs** | ssi_annual_scores.csv; MK_results_all_stations.csv; MK_results_significant.csv; MK_area_summary.csv |
| **Run after** | ssi_calculate.R |
| **Original filename** | `ssi_annual_mannkendall.R` |


### `ssi_trend_visualisations.R`

**Used in:** Ch. 5 figures  
**Status:** FINAL

Plots annual SSI trajectories for worsening and improving stations to confirm Mann-Kendall results are real rather than data artifacts; produces the tau distribution figure; flags the Areas 17-19 convergence for cross-dataset validation.

| | |
|---|---|
| **Inputs** | ssi_annual_scores.csv; MK_results_all_stations.csv |
| **Outputs** | MK trajectory and tau distribution figures (PNG) |
| **Run after** | ssi_annual_mannkendall.R |
| **Original filename** | `mk_ssi_results_visulizations.R` |


### `ssi_trend_trajectory_plots.R`

**Used in:** Ch. 5 figures  
**Status:** FINAL

Corrected trajectory plots for worsening and improving stations. Resolves an Area column name collision that occurred when joining MK results to annual scores.

| | |
|---|---|
| **Inputs** | MK_results_all_stations.csv; ssi_annual_scores.csv |
| **Outputs** | Trajectory figures (PNG) |
| **Run after** | ssi_annual_mannkendall.R, ssi_trend_visualisations.R |
| **Original filename** | `mk_trajectory_fix.R` |


## Validation — Biological Validation

Tests the SSI against observed oyster mortality. Establishes that the index tracks a real biological signal.


### `ssi_biological_validation.R`

**Used in:** Ch. 5 Validation  
**Status:** FINAL

Validates the SSI against observed oyster mortality at sentinel monitoring sites, including the Winyah Bay 2015 flood case study as mechanistic evidence.

| | |
|---|---|
| **Inputs** | SSI results; sentinel site mortality records; WQP salinity |
| **Outputs** | ssi_validation_report.txt; validation figures; ch5_winyah_bay_2015_salinity.png |
| **Run after** | ssi_calculate.R |
| **Original filename** | `ssi_biological_validation.R` |


### `ssi_extended_validation.R`

**Used in:** Ch. 5 Validation  
**Status:** FINAL

Extended validation testing lagged salinity effects, temperature, rainfall, and multi-factor models against observed mortality.

| | |
|---|---|
| **Inputs** | Annual SSI; mortality panel; NOAA precipitation; water temperature |
| **Outputs** | Extended validation tables and figures |
| **Run after** | ssi_biological_validation.R |
| **Original filename** | `extendedvalid_lag_temp_rain.R` |


### `ssi_annual_means_regression.R`

**Used in:** Ch. 5 Validation  
**Status:** FINAL

Builds the site-year panel dataset linking annual mortality to annual SSI scores via the site-to-station spatial join, then fits the regression used in biological validation.

| | |
|---|---|
| **Inputs** | Site-to-station mapping; annual mortality; ssi_annual_scores.csv |
| **Outputs** | Panel dataset and regression output |
| **Run after** | ssi_annual_mannkendall.R |
| **Original filename** | `annualmeansbiovaldscriptsregression.R` |


### `ssi_validation_figures.R`

**Used in:** Ch. 5 figures  
**Status:** FINAL

Produces the final publication figures for the validation chapter, including the revised scatter plot and the Winyah Bay 2015 salinity figure.

| | |
|---|---|
| **Inputs** | Saved validation results; WQP salinity data |
| **Outputs** | Validation figures (PNG) |
| **Run after** | ssi_biological_validation.R |
| **Original filename** | `validationfigures6202026.R` |


### `ssi_residual_diagnostic.R`

**Used in:** Ch. 5 Validation / Limitations  
**Status:** FINAL

Tests two questions arising from La Peyre et al. (2009): whether a Dermo (Perkinsus marinus) disease signal causes the SSI to systematically under-predict mortality at high-salinity sites, and how alternative outcome variables compare.

| | |
|---|---|
| **Inputs** | Validation panel; SSI results; mortality records |
| **Outputs** | Residual diagnostic tables and figures |
| **Run after** | ssi_biological_validation.R |
| **Original filename** | `ssi_residual_diagnostic.R` |


### `mortality_ssi_precipitation_interaction.R`

**Used in:** Ch. 5 Validation  
**Status:** FINAL

Interaction model testing whether rainfall affects low-SSI sites more severely than high-SSI sites. Compares additive and interaction models by ANOVA using the static 26-year composite SSI.

| | |
|---|---|
| **Inputs** | panel_rain (mortality + rainfall panel); static SSI |
| **Outputs** | Interaction model output and figure |
| **Run after** | ssi_annual_means_regression.R |
| **Original filename** | `mortality_ssi_precipitation_interaction.R` |


### `stormcrash_mortality_table.R`

**Used in:** Ch. 5 table  
**Status:** FINAL

Builds per-station salinity crash metrics and cross-tabulates them against observed mortality.

| | |
|---|---|
| **Inputs** | Crash event records; mortality data |
| **Outputs** | Storm crash / mortality summary table |
| **Run after** | ssi_biological_validation.R |
| **Original filename** | `stormcrash_mortality_table.R` |


## Storm Responsiveness — Storm Responsiveness

Measures how strongly each site's salinity responds to rainfall — the ESSI dimension the SSI cannot express.


### `salinity_exposure_storm_response.R`

**Used in:** Ch. 4 Methods (Stormwater Responsiveness)  
**Status:** FINAL (July 2026)

Calculates station-level storm responsiveness as the OLS slope of station- and month-demeaned salinity anomaly on 7-day antecedent rainfall (ppt per inch). Uses radius-based rainfall aggregation across all gauges within 30 km. REPLACES storm_vulnerability.R, which contained a gauge-matching error.

| | |
|---|---|
| **Inputs** | narrowresult.csv (WQP); Station.csv; 14 NOAA daily precipitation files |
| **Outputs** | salinity_exposure_and_storm_response.csv (659 stations) |
| **Run after** | None — entry point |
| **Original filename** | `salinity_exposure_v2.R` |


## Watershed Drivers — Watershed Drivers

Land cover and development change evaluated as explanatory drivers of storm responsiveness rather than as scored index components.


### `watershed_impervious_model.R`

**Used in:** Ch. 4 Methods (Watershed Drivers)  
**Status:** FINAL — see caveat in reference doc

Joins monitoring stations to HUC12 watersheds, calculates mean fractional impervious surface per watershed from NLCD 2021, and tests whether impervious surface predicts SSI and FC Mann-Kendall trend direction.

| | |
|---|---|
| **Inputs** | stations_with_HUC12.csv; HUC12_impervious_stats.csv; MK_results_all_stations.csv; FC_MK_results_all.csv |
| **Outputs** | watershed_analysis_full.csv; watershed_area_summary.csv; scatter and table figures |
| **Run after** | ssi_annual_mannkendall.R |
| **Original filename** | `watershed_model.R` |


### `development_change_analysis.py`

**Used in:** Ch. 4 Methods (Watershed Drivers)  
**Status:** FINAL (July 2026)

Tests whether CHANGE in development 1997-2023 is associated with change in salinity behaviour and with storm responsiveness. Extracts developed land in 1 km and 5 km station buffers at both epochs, differences them, and benchmarks change against static development.

| | |
|---|---|
| **Inputs** | Annual NLCD FctImp 1997 and 2023; MK_results_all_stations.csv; wq_period_change.csv; salinity_exposure_and_storm_response.csv |
| **Outputs** | development_change_results.csv |
| **Run after** | salinity_exposure_storm_response.R |
| **Original filename** | `development_change_analysis.py` |


### `landcover_change_extraction.py`

**Used in:** Ch. 4 Methods (Watershed Drivers)  
**Status:** FINAL (July 2026)

Extracts categorical NLCD land cover at 1997 and 2023 in station buffers and computes the conversion matrix, identifying which land cover types became developed.

| | |
|---|---|
| **Inputs** | Annual NLCD LndCov 1997 and 2023; MK_results_all_stations.csv |
| **Outputs** | landcover_change_stations.csv |
| **Run after** | None — standalone |
| **Original filename** | `landcover_change.py` |


## Essi Composite — ESSI Composite

Combines the SSI baseline with its additional dimensions into the Enhanced Salinity Suitability Index.


### `essi_composite.R`

**Used in:** Ch. 4 Methods; Ch. 5 Results  
**Status:** FINAL — predates the July 2026 ESSI restructure; see reference doc

Builds the Enhanced Salinity Suitability Index composite score per station from the SSI baseline and its additional dimensions.

| | |
|---|---|
| **Inputs** | SSI results; MK trend results; component layers |
| **Outputs** | ESSI composite scores per station |
| **Run after** | ssi_calculate.R, ssi_annual_mannkendall.R |
| **Original filename** | `essi_composite.R` |


## Spatial Interpolation — Spatial Interpolation and Siting Tool

Converts station point scores into continuous coastal surfaces, and builds the deployed ArcGIS Online siting tool.


### `ipdw_ssi_interpolation.R`

**Used in:** Ch. 4 Methods (Spatial Interpolation)  
**Status:** FINAL

Interpolates SSI station scores across the coastal zone using inverse path distance weighting through a UVVR-derived cost raster, so interpolation follows navigable water rather than crossing land barriers (Stachelek & Madden, 2015).

| | |
|---|---|
| **Inputs** | uvvr_cost_raster.tif; SSI_Results_with_coords.csv; coastal boundary layer |
| **Outputs** | ipdw_ssi_surface_final.tif; ipdw_ssi_classified.tif |
| **Run after** | ssi_calculate.R |
| **Original filename** | `ipdw_ssi_interpolation.R` |


### `ipdw_essi_interpolation.R`

**Used in:** Ch. 4 Methods (Spatial Interpolation)  
**Status:** FINAL

Same IPDW cost-weighted approach applied to the ESSI composite layer.

| | |
|---|---|
| **Inputs** | uvvr_cost_raster.tif; ESSI composite scores; coastal boundary layer |
| **Outputs** | ipdw_essi_surface_final.tif; ipdw_essi_classified.tif |
| **Run after** | essi_composite.R |
| **Original filename** | `ipdw_essi_interpolation.R` |


### `arcpy_ssi_finalize.py`

**Used in:** Applied product — mariculture siting tool  
**Status:** FINAL — EBK-based, predates the IPDW surfaces

Produces the publication-ready, web-deployable salinity stability polygon layer for ArcGIS Online / Experience Builder. Method: EBK, mask, focal smooth, classify, vectorise.

| | |
|---|---|
| **Inputs** | DES_Stations_KC joined to Merged_SSI_Results.csv (file geodatabase) |
| **Outputs** | SSI_Final_Polygons feature class for AGOL |
| **Run after** | ssi_calculate.R |
| **Original filename** | `ssi_finalize.py` |


### `arcpy_ssi_heatmap.py`

**Used in:** Applied product — mariculture siting tool  
**Status:** FINAL

Continuous salinity stability heatmap for the siting tool, with attribute-rich station points for pop-ups so growers can read a gradient and click a station for its exact SSI and risk class.

| | |
|---|---|
| **Inputs** | EBK surface; station point layer (file geodatabase) |
| **Outputs** | SSI_Heatmap_Raster; SSI_Station_Points |
| **Run after** | arcpy_ssi_finalize.py |
| **Original filename** | `ssi_heatmap_1.py` |


### `arcpy_finish_validation_project.py`

**Used in:** Ch. 5 Validation — map production  
**Status:** FINAL

Completes the SSI_Mortality_Validation ArcGIS project: applies existing .lyrx symbology to the three rasters and symbolises sentinel sites by the concordance field.

| | |
|---|---|
| **Inputs** | SSI_Mortality_Validation.aprx; .lyrx symbology files |
| **Outputs** | Symbolised ArcGIS project |
| **Run after** | ssi_biological_validation.R |
| **Original filename** | `finish_ssi_validation_project.py` |


## Regulatory Gap — Regulatory Gap Analysis

Quantifies the difference between ecologically suitable acreage and actively permitted acreage, and classifies the constraints producing it.


### `station_tier_classification.R`

**Used in:** Ch. 6 Governance  
**Status:** FINAL

Cross-joins SSI and fecal coliform Mann-Kendall results to classify each station into a three-tier mariculture suitability framework based on dual biological and water quality conditions.

| | |
|---|---|
| **Inputs** | MK_results_all_stations.csv; FC_MK_results_all.csv; CUR_CLASS spatial join |
| **Outputs** | SSI_MK_stations_with_classification.csv; tier summary |
| **Run after** | ssi_annual_mannkendall.R |
| **Original filename** | `station_tier_classification.R` |


### `reclassification_frequency.R`

**Used in:** Ch. 6 Governance  
**Status:** FINAL

Analyses frequency and direction of SCDES shellfish harvest area reclassification 2017-2023 against the dual-condition tier framework, testing whether improving areas are actually reclassified.

| | |
|---|---|
| **Inputs** | SCDES classification history; tier classification output |
| **Outputs** | Reclassification frequency tables |
| **Run after** | station_tier_classification.R |
| **Original filename** | `reclassification_frequency.R` |


### `gap_acreage_table.R`

**Used in:** Ch. 6 Governance — gap quantification  
**Status:** FINAL — supersedes gap_acreage_table.R (IDW-based)

Recalculates biophysical suitability acreage from the masked SSI IPDW surface and regenerates the gap acreage table. Replaces the earlier IDW-based version.

| | |
|---|---|
| **Inputs** | ipdw_ssi_classified.tif; SCDES classification polygons; active lease layer |
| **Outputs** | gap_acreage_table.png; acreage summary |
| **Run after** | ipdw_ssi_interpolation.R |
| **Original filename** | `update_gap_acreage_table.R` |


## Network Analysis — Stakeholder Network Analysis

Maps relationships among South Carolina mariculture stakeholders to explain why the gap persists.


### `edgelist_prep.R`

**Used in:** Ch. 7 Network Analysis  
**Status:** FINAL — reusable prep

Cleans the edge list for igraph and visNetwork: trims leading/trailing whitespace, removes duplicates, and resolves formatting errors. Run whenever the edge list is updated, before any network script.

| | |
|---|---|
| **Inputs** | Raw edge list CSV |
| **Outputs** | Cleaned edge list CSV |
| **Run after** | None — entry point |
| **Original filename** | `edgelist_cleaning_prep_code.R` |


### `network_pipeline.R`

**Used in:** Ch. 7 Network Analysis  
**Status:** FINAL (June 2026)

Unified network analysis and interactive visualisation pipeline using the FINAL_June2026 roster and edge list. Calculates degree and betweenness centrality via igraph and builds the interactive visNetwork product.

| | |
|---|---|
| **Inputs** | FINAL_June2026 roster CSV; cleaned edge list CSV (networkanalysis/sheets/) |
| **Outputs** | Centrality results; interactive network HTML |
| **Run after** | edgelist_prep.R |
| **Original filename** | `thesis_network_pipeline_June2026.R` |


### `network_static_figures.R`

**Used in:** Ch. 7 figures  
**Status:** FINAL (June 2026)

Publication-quality static network maps for the thesis, exported at 300 dpi PNG and vector PDF.

| | |
|---|---|
| **Inputs** | Roster and edge list; network objects from network_pipeline.R |
| **Outputs** | Static network figures (PNG 300 dpi + PDF) |
| **Run after** | network_pipeline.R |
| **Original filename** | `thesis_static_figures.R` |


### `winyah_bay_2015_storm_lag.R`
**Used in:** Ch. 4 Methods (Stormwater Responsiveness) — the ~93 hour figure  
**Status:** FINAL — written 29 July 2026 to make the reported figure reproducible

Measures the observed lag between rainfall onset and salinity crossing 8 ppt during the
October 2015 Winyah Bay flood, using continuous 15-minute NERRS monitoring at Thousand Acre.
Provides the South Carolina-specific storm response time justifying the seven-day antecedent
window, replacing a borrowed 12-48 hour figure from non-SC estuary literature. Documents both
methodological decisions inline: why gauge USC00383470 was used rather than the nearest gauge
(the nearest has no 2015 data), and why the trigger is 1 October rather than an automatically
detected threshold day. Reports sensitivity to the trigger choice.

| | |
|---|---|
| **Inputs** | `niwtawq2015.csv` (NERRS SWMP 15-min); NOAA daily precipitation files |
| **Outputs** | `winyah_bay_2015_storm_lag_curve.csv`; console lag summary |
| **Run after** | None — standalone |
| **Expected result** | 22.7 ppt at trigger, first sub-8 ppt reading 2015-10-04 21:15 (7.7 ppt), lag 93.25 hours |

### Update — 29 July 2026: two corrections and a measurement rebuild

**Correction 1 — em-dash name normalisation (`network_pipeline.R`).**
`clean_names()` previously added an em dash to the roster to match the edge list, but handled
only the Nature Conservancy. Coastal Conservation Association remained mismatched, so
`graph_from_data_frame()` failed on an edge endpoint absent from the vertices frame. Replaced
with a direction-agnostic strip of em dashes from both sides. The pipeline now regenerates its
own results.

**Correction 2 — transposed brokerage roles (`network_pipeline.R`). Consequential.**
`sna::brokerage()` returns `raw.nli` with columns ordered w_I, w_O, b_IO, b_OI, b_O —
Coordinator, **Itinerant**, Representative, **Gatekeeper**, Liaison. The script assigned names
by position in the order Coordinator, Gatekeeper, Representative, Itinerant, Liaison,
transposing columns 2 and 4. Independent recomputation confirms Coordinator, Representative
and Liaison match exactly (104/104) while Gatekeeper and Itinerant are reversed throughout.

The published `brokerage_results.csv` therefore mislabels these two roles, and the script's
console output headed "Top Gatekeepers (filtering/stalling cross-group flows)" in fact lists
top *itinerant* brokers. This matters for interpretation: a gatekeeper controls access into its
own group and is a candidate for procedural reform, whereas an itinerant broker sits outside a
group and connects two of its members, implying a different intervention entirely. Corrected
values are in `brokerage_results_v2_July2026.csv`.

Corrected top-three brokerage profiles:

```
                                          Coord  Gatekpr  Repres  Itin  Liaison  Total
SC Sea Grant Consortium (SCSGC)             258      231     230   113       44    876
SCDNR Shellfish Management Section          113       70     355    12      219    769
SC Shellfish Growers Association (SCSGA)     56      165     149   175      163    708
```

Dominant role by actor — the policy-relevant read: SCDES Shellfish Sanitation is 74%
Representative and the USACE Charleston office is 98% Representative, meaning both speak
outward from the regulatory sector rather than controlling entry to it. SCDNR MRRI is 71%
Coordinator, brokering almost entirely within its own sector. SCSGC is the most balanced
broker in the network and the only actor scoring high across all five roles.

**Measurement rebuild — tie strength recoded as interaction frequency.**
The original `strength` field correlated with `evidence_tier` at Spearman −0.807, indicating it
recorded confidence that a tie existed rather than the intensity of the relationship. It has
been renamed `evidence_confidence` and a new `tie_frequency` field added, defined as the
interaction frequency *mandated by the regulatory or institutional framework* at the actor's
operational stage:

```
3  Routine    contact required repeatedly within a season or year        121 ties
2  Periodic   contact required on an annual or cyclical basis             72 ties
1  Episodic   contact at application, renewal, or event trigger           82 ties
```

Defining frequency as mandated rather than observed is deliberate. Observed frequency varies
with relationship posture — some operators are in litigation with the permitting agency, others
cultivate cordial relations strategically — and encoding that variation on named individuals in
a population this size would reintroduce the deductive disclosure risk that justified excluding
conflict ties. Mandated frequency is derivable from the permitting framework, defensible, and
disclosure-safe.

The new measure is close to independent of documentation confidence (Spearman −0.298 against
`evidence_tier`, versus −0.807 for the field it replaces), confirming the two now capture
different constructs.

A `permit_stage` field was added to the roster (`operating`, `permit_pending`) so that
regulatory tie frequency can be revised as committee review updates the operational status of
individual farms. Every coded tie carries a `freq_rule` value recording the basis for its
assignment; `_tie_frequency_AUDIT.csv` presents these for review.

Revised data files, with originals preserved:
`node_roster_FINAL_v2_July2026.csv`, `edge_list_FINAL_v2_July2026.csv`,
`brokerage_results_v2_July2026.csv`, `_tie_frequency_AUDIT.csv`.

**HITS hub score removed from reporting.** The `influence_hub` column remains in the pipeline
output but is not reported; degree, betweenness and brokerage roles cover connectedness,
bridging, and bridging type without requiring hub-versus-authority exposition.

---

## Caveats attached to specific scripts

**`04_watershed_drivers/watershed_impervious_model.R`**
The relationship this script reports between impervious surface and SSI trend
(R² ≈ 0.035–0.09) is statistically unstable. Because impervious surface is a watershed-level
attribute shared by all stations within a watershed, station-level significance is inflated by
roughly three orders of magnitude relative to the correct unit of analysis (55 watersheds, not
503 stations). The direction of the relationship also reverses when Charleston is excluded from
the sample. The script is retained because it produced results reported in the proposal, but
those results should be described as a non-finding rather than as a counterintuitive result.

**`05_essi_composite/essi_composite.R`**
Written June 2026, before the ESSI was restructured in July 2026. The current framing treats
the SSI, the SSI trend, and storm responsiveness as components, with watershed development
serving as the projection mechanism rather than a scored component. This script may require
revision to match.

**`06_spatial_interpolation/arcpy_ssi_finalize.py` and `arcpy_ssi_heatmap.py`**
These use Empirical Bayesian Kriging and predate the IPDW cost-weighted surfaces. They built
the deployed siting tool and remain the source of that product, but the thesis surfaces come
from the IPDW scripts.

---

## Action items

**1. RESOLVED — the 93-hour Winyah Bay lag is now reproducible.**
`03_storm_responsiveness/winyah_bay_2015_storm_lag.R` regenerates the figure and documents the gauge and trigger-date decisions inline.

**2. RESOLVED — `clean_names()` and the transposed brokerage columns are both fixed** in `08_network_analysis/03_final_june2026/network_pipeline.R`, marked inline and documented in the 29 July update above.

**3. `07_regulatory_gap/gap_acreage_table.R` references a script that does not exist.**
Its header says "Run AFTER ch5_ipdw_surface_maps.R." No file by that name exists in the project.
The script also states it can run standalone, so this is likely a stale reference, but the
header should be corrected.

**4. File paths are absolute.**
Every script hardcodes paths from the original working environment. If the repository is
intended to be runnable by others, paths should be relativised or moved to a configuration
block at the top of each script.
