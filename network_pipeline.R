# =============================================================================
# network_pipeline.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Stakeholder Network Analysis (Ch. 7)
# Stage       : Final analysis
# Date        : 12 June 2026
# Status      : FINAL — current thesis version
#
# PURPOSE
#   Unified network analysis and interactive visualisation using the
#   FINAL_June2026 roster and edge list. Computes degree and betweenness
#   centrality via igraph. This is the version whose results are reported in
#   the thesis.
#
# INPUTS      : node_roster_FINAL_June2026.csv; edge_list_FINAL_June2026.csv
# OUTPUTS     : centrality_results.csv; brokerage_results.csv; network_full.html; index_updated.html
# RUN AFTER   : edgelist_prep.R
# =============================================================================

# ============================================================
# thesis_network_pipeline_June2026.R
# Unified Network Analysis & Interactive Visualizations
# Kate Chatman | MPA/EVSS Thesis | College of Charleston 2026
# Updated June 2026 — uses FINAL_June2026 roster + edge list CSVs
# ============================================================

library(igraph)
library(readr)
library(dplyr)
library(stringr)
library(visNetwork)
library(sna)

# ---- FILE PATHS ----------------------------------------
# Both files live in networkanalysis/sheets/
# Run this script from that directory, or set setwd() here:
# setwd("~/path/to/thesis/networkanalysis/sheets")

EDGE_FILE   <- "edge_list_FINAL_June2026.csv"
ROSTER_FILE <- "node_roster_FINAL_June2026.csv"

# ---- 1. LOAD & CLEAN DATA ----------------------------------

edges_raw  <- read_csv(EDGE_FILE,   show_col_types = FALSE)
roster_raw <- read_csv(ROSTER_FILE, show_col_types = FALSE)

# Defensive name cleaning (TNC uses an em dash — kept for safety)
clean_names <- function(text) str_replace_all(text, "\\s*\u2014\\s*", " ")
# CORRECTED 29 Jul 2026. The previous version added an em dash to the roster to
# match the edge list, but handled only the Nature Conservancy, leaving
# "Coastal Conservation Association" mismatched — graph_from_data_frame() then
# failed because an edge endpoint was absent from the vertices frame. Stripping
# em dashes from BOTH sides is direction-agnostic and covers future entries.

edges_raw <- edges_raw %>%
  mutate(from = clean_names(from), to = clean_names(to))

roster <- roster_raw %>%
  mutate(name = clean_names(name))
# Note: Tracy Ross is retained — she now has a staff-affiliation edge
# to SCDNR Marine Permitting Office (Indigenous Molluscan Importation).

# ---- 2. EXPAND BIDIRECTIONAL EDGES -------------------------

edges_uni   <- edges_raw %>% filter(direction == "unidirectional")
edges_bi_ab <- edges_raw %>% filter(direction == "bidirectional")
edges_bi_ba <- edges_bi_ab %>% rename(from = to, to = from)

edges_directed <- bind_rows(edges_uni, edges_bi_ab, edges_bi_ba)

cat("Edges (raw):", nrow(edges_raw), "\n")
cat("Edges (directed, bidirectional expanded):", nrow(edges_directed), "\n")
cat("Roster nodes:", nrow(roster), "\n")

# ---- 3. BUILD GRAPH AND CALCULATE METRICS ------------------

g <- igraph::graph_from_data_frame(d = edges_directed, directed = TRUE, vertices = roster)

# Use igraph:: prefix to avoid masking by sna/network packages
btw       <- igraph::betweenness(g, directed = TRUE, normalized = TRUE)
deg_in    <- igraph::degree(g, mode = "in")
deg_out   <- igraph::degree(g, mode = "out")
deg_total <- igraph::degree(g, mode = "all")

# Hub score: outbound structural influence in directed systems
# Reflects how strongly each node pushes information/authority outward
influence_hub <- igraph::hits_scores(g)$hub

results <- data.frame(
  name             = V(g)$name,
  actor_type       = V(g)$actor_type,
  coalition        = V(g)$coalition,
  governance_level = V(g)$governance_level,
  node_status      = V(g)$node_status,
  degree_in        = deg_in,
  degree_out       = deg_out,
  degree_total     = deg_total,
  betweenness      = round(btw, 4),
  influence_hub    = round(influence_hub, 4),
  stringsAsFactors = FALSE
) %>% arrange(desc(betweenness))

write_csv(results, "centrality_results.csv")
cat("Centrality results written to centrality_results.csv\n")

# ---- 4. PALETTES & VISUAL CONFIGURATIONS -------------------

actor_colors <- c(
  "regulatory"       = "#C0392B",
  "science-extension"= "#2980B9",
  "advocacy"         = "#27AE60",
  "industry"         = "#F39C12"
)

coalition_borders <- c(
  "regulatory-cautious" = "#922B21",
  "neutral"             = "#1A5276",
  "pro-expansion"       = "#1E8449",
  "conservation"        = "#6C3483"
)

edge_colors <- c(
  "regulatory_authority"    = "#E74C3C",
  "regulatory_consultation" = "#E67E22",
  "collaboration"           = "#2ECC71",
  "information"             = "#85C1E9",
  "commercial_supply"       = "#F1C40F",
  "funding"                 = "#9B59B6"
)

scale_node_size <- function(metric_vector) {
  8 + (sqrt(metric_vector) / max(sqrt(metric_vector + 1e-9))) * 42
}

# ---- 5. GENERATE DATA FRAMES FOR VISNETWORK ----------------

nodes <- data.frame(
  id               = igraph::V(g)$name,
  label            = igraph::V(g)$name,
  title            = paste0("<b>", igraph::V(g)$name, "</b><br>Role: ", igraph::V(g)$role,
                            "<br>Betweenness: ", round(btw, 4)),
  value            = scale_node_size(btw),
  color.background = actor_colors[igraph::V(g)$actor_type],
  color.border     = coalition_borders[igraph::V(g)$coalition],
  borderWidth      = 2.5,
  group            = igraph::V(g)$actor_type,
  shape            = ifelse(igraph::V(g)$category == "institutional node", "square", "dot"),
  opacity          = ifelse(igraph::V(g)$node_status == "historical", 0.5, 1.0),
  borderDashes     = ifelse(igraph::V(g)$node_status == "historical", TRUE, FALSE),
  stringsAsFactors = FALSE
)

edge_width_map <- c("1" = 1, "2" = 2.3, "3" = 4.0)

edges_vis <- data.frame(
  from   = igraph::ends(g, igraph::E(g))[, 1],
  to     = igraph::ends(g, igraph::E(g))[, 2],
  color  = edge_colors[igraph::E(g)$relationship_type],
  width  = as.numeric(edge_width_map[as.character(igraph::E(g)$strength)]),
  arrows = "to",
  dashes = igraph::E(g)$relationship_type == "information",
  smooth = TRUE,
  stringsAsFactors = FALSE
)

# ---- 6. SHARED INTERACTION / PHYSICS TEMPLATE -------------

apply_shared_settings <- function(network) {
  network %>%
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -100,
        centralGravity        = 0.01,
        springLength          = 140,
        springConstant        = 0.06,
        damping               = 0.4
      ),
      stabilization = list(iterations = 250)
    ) %>%
    visInteraction(hover = TRUE, navigationButtons = TRUE,
                   keyboard = TRUE, tooltipDelay = 150)
}

# ---- 7. RENDER VIEW 1: FULL NETWORK -----------------------

net_full <- visNetwork(
  nodes, edges_vis,
  main    = list(text  = "SC Shellfish Mariculture Governance Network",
                 style = "font-family:Georgia;font-size:16px;font-weight:bold;"),
  submain = list(text  = "Node size = Betweenness Centrality | Color = Actor Type | Border = Coalition",
                 style = "font-family:Georgia;font-size:12px;color:#555555;"),
  width   = "100%", height = "850px"
) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    selectedBy       = "group",
    nodesIdSelection = TRUE
  ) %>%
  apply_shared_settings()

visSave(net_full, "network_full.html", selfcontained = TRUE)
cat("Full network visualization saved: network_full.html\n")

# ---- 8. BURDEN SUBGRAPH VISUALIZATION ---------------------

burden_raw      <- edges_raw %>% filter(burden_generating == TRUE)
burden_uni      <- burden_raw %>% filter(direction == "unidirectional")
burden_bi_ab    <- burden_raw %>% filter(direction == "bidirectional")
burden_bi_ba    <- burden_bi_ab %>% rename(from = to, to = from)
burden_directed <- bind_rows(burden_uni, burden_bi_ab, burden_bi_ba)

burden_node_names <- unique(c(burden_directed$from, burden_directed$to))
roster_burden <- roster %>% filter(name %in% burden_node_names)

g_burden <- igraph::graph_from_data_frame(d = burden_directed, directed = TRUE, vertices = roster_burden)

btw_b     <- igraph::betweenness(g_burden, directed = TRUE, normalized = TRUE)
hub_b     <- igraph::hits_scores(g_burden)$hub
deg_in_b  <- igraph::degree(g_burden, mode = "in")
deg_out_b <- igraph::degree(g_burden, mode = "out")

nodes_burden <- data.frame(
  id               = igraph::V(g_burden)$name,
  label            = igraph::V(g_burden)$name,
  title            = paste0("<b>", igraph::V(g_burden)$name, "</b><br>Role: ", igraph::V(g_burden)$role,
                            "<br>Betweenness: ", round(btw_b, 4),
                            "<br>Hub Score: ", round(hub_b, 3)),
  value            = scale_node_size(btw_b),
  color.background = actor_colors[igraph::V(g_burden)$actor_type],
  color.border     = coalition_borders[igraph::V(g_burden)$coalition],
  borderWidth      = 2.5,
  group            = igraph::V(g_burden)$actor_type,
  shape            = ifelse(igraph::V(g_burden)$category == "institutional node", "square", "dot"),
  stringsAsFactors = FALSE
)

edges_burden_vis <- data.frame(
  from   = igraph::ends(g_burden, igraph::E(g_burden))[, 1],
  to     = igraph::ends(g_burden, igraph::E(g_burden))[, 2],
  color  = "#E74C3C",
  width  = as.numeric(edge_width_map[as.character(igraph::E(g_burden)$strength)]),
  arrows = "to",
  smooth = TRUE,
  stringsAsFactors = FALSE
)

net_burden <- visNetwork(
  nodes_burden, edges_burden_vis,
  main    = list(text  = "Administrative Burden-Generating Relationships",
                 style = "font-family:Georgia;font-size:16px;font-weight:bold;"),
  submain = list(text  = "Burden-generating edges only | Node size = Betweenness Centrality within subgraph",
                 style = "font-family:Georgia;font-size:12px;color:#555555;"),
  width   = "100%", height = "850px"
) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    selectedBy       = "group",
    nodesIdSelection = TRUE
  ) %>%
  apply_shared_settings()

visSave(net_burden, "burden.html", selfcontained = TRUE)
cat("Burden network visualization saved: burden.html\n")
cat("Burden subgraph:", nrow(roster_burden), "nodes,", nrow(burden_directed), "directed edges\n")

# ============================================================
# DIAGNOSTIC ANALYSIS
# Advanced SNA Diagnostics for SC Mariculture Stagnation
# ============================================================

cat("\n### MULTI-AGENCY GAUNTLET DIAGNOSTIC ###\n")

# Regulatory density among agencies only
reg_agencies <- roster %>% filter(actor_type == "regulatory") %>% pull(name)

g_agencies_only <- igraph::induced_subgraph(g, vids = igraph::V(g)$name %in% reg_agencies)
agency_density  <- igraph::edge_density(g_agencies_only)

cat("Horizontal Regulatory Density:", round(agency_density, 4), "\n")
cat("Low = agencies regulate in silos; growers must navigate uncoordinated channels.\n\n")

# Administrative burden subgraph: burden-generating edges only
burden_edges <- edges_raw %>% filter(burden_generating == TRUE)
cat("Total burden-generating edges:", nrow(burden_edges), "\n")

# Out/in asymmetry for the four primary permitting agencies
primary_agencies <- c(
  "SCDNR Shellfish Management Section",
  "SCDES Shellfish Sanitation Section",
  "SCDES Bureau of Coastal Management",
  "U.S. Army Corps of Engineers Charleston Regulatory Office"
)
cat("\n--- Primary Permitting Agency Degree Asymmetry (Out vs In) ---\n")
for (ag in primary_agencies) {
  out <- deg_out[ag]
  inn <- deg_in[ag]
  ratio <- if (inn > 0) round(out / inn, 1) else Inf
  cat(sprintf("  %-52s  out=%d  in=%d  ratio=%s\n", ag, out, inn, ratio))
}

# ---- GOULD-FERNANDEZ BROKERAGE ----------------------------
cat("\n### GOULD-FERNANDEZ BROKERAGE DIAGNOSTIC ###\n")

adj_matrix   <- as.matrix(igraph::as_adjacency_matrix(g))
group_names  <- igraph::V(g)$actor_type
group_vector <- as.numeric(as.factor(group_names))

brokerage_results <- sna::brokerage(adj_matrix, cl = group_vector)

# Build cleanly by column index to avoid name-collision issues
b_mat <- brokerage_results$raw.nli
raw_brokerage <- data.frame(
  name           = igraph::V(g)$name,
  actor_type     = group_names,
  # CORRECTED 29 Jul 2026 — sna::brokerage() returns raw.nli in the order
  # w_I, w_O, b_IO, b_OI, b_O = Coordinator, Itinerant, Representative,
  # Gatekeeper, Liaison. Assigning by position previously transposed
  # Gatekeeper and Itinerant. Referencing columns by name prevents recurrence.
  Coordinator    = b_mat[, "w_I"],
  Itinerant      = b_mat[, "w_O"],
  Representative = b_mat[, "b_IO"],
  Gatekeeper     = b_mat[, "b_OI"],
  Liaison        = b_mat[, "b_O"],
  stringsAsFactors = FALSE
)
raw_brokerage$Total <- rowSums(raw_brokerage[, 3:7])

write_csv(raw_brokerage %>% arrange(desc(Total)),
          "brokerage_results.csv")
cat("Brokerage results written to brokerage_results.csv\n")

cat("\n--- Top Gatekeepers (filtering/stalling cross-group flows) ---\n")
raw_brokerage %>%
  select(name, actor_type, Gatekeeper) %>%
  arrange(desc(Gatekeeper)) %>% head(10) %>% print(row.names = FALSE)

cat("\n--- Top Coordinators (insular within-type loops) ---\n")
raw_brokerage %>%
  select(name, actor_type, Coordinator) %>%
  arrange(desc(Coordinator)) %>% head(10) %>% print(row.names = FALSE)

cat("\n--- Top Liaisons (bridges across disconnected sectors) ---\n")
raw_brokerage %>%
  select(name, actor_type, Liaison) %>%
  arrange(desc(Liaison)) %>% head(10) %>% print(row.names = FALSE)

cat("\nPipeline complete. Files written: centrality_results.csv, brokerage_results.csv, network_full.html\n")
