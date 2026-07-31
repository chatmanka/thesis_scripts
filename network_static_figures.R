# =============================================================================
# network_static_figures.R
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
#   Publication-quality static network maps, exported at 300 dpi PNG and
#   vector PDF. Run from networkanalysis/sheets/.
#
# INPUTS      : node_roster_FINAL_June2026.csv; edge_list_FINAL_June2026.csv
# OUTPUTS     : Static network figures (PNG + PDF)
# RUN AFTER   : network_pipeline.R
# =============================================================================

# ============================================================
# thesis_static_figures.R
# Publication-Quality Static Network Maps for Thesis
# Kate Chatman | MPA/EVSS Thesis | College of Charleston 2026
# Output: PNG (300 dpi) + PDF (vector) for each figure
# Run from: networkanalysis/sheets/
# ============================================================

library(igraph)
library(readr)
library(dplyr)
library(stringr)

EDGE_FILE   <- "edge_list_FINAL_June2026.csv"
ROSTER_FILE <- "node_roster_FINAL_June2026.csv"

edges_raw  <- read_csv(EDGE_FILE,   show_col_types = FALSE)
roster_raw <- read_csv(ROSTER_FILE, show_col_types = FALSE)

# Expand bidirectional
edges_uni      <- edges_raw %>% filter(direction == "unidirectional")
edges_bi_ab    <- edges_raw %>% filter(direction == "bidirectional")
edges_bi_ba    <- edges_bi_ab %>% rename(from = to, to = from)
edges_directed <- bind_rows(edges_uni, edges_bi_ab, edges_bi_ba)

g <- igraph::graph_from_data_frame(d = edges_directed, directed = TRUE, vertices = roster_raw)

btw     <- igraph::betweenness(g, directed = TRUE, normalized = TRUE)
deg     <- igraph::degree(g, mode = "all")
hub     <- igraph::hits_scores(g)$hub

# ── COLOUR PALETTES ──────────────────────────────────────────
# ── THESIS-READY AQUATIC COLOR PALETTES ───────────────────────
# Deep oceanic, sage, and warm terracotta hues optimized for light backgrounds
actor_pal <- c( 
  "regulatory"        = "#2C5E7A",  # Deep Slate Blue
  "science-extension" = "#4A8F84",  # Muted Teal/Sage
  "advocacy"          = "#7AA874",  # Soft Olive Green
  "industry"          = "#D68A57"   # Muted Terracotta/Clay
)

# ── LAYOUT (Fruchterman-Reingold, fixed seed) ────────────────
set.seed(2026)
layout_fr <- igraph::layout_with_fr(g, niter = 1000, start.temp = 20)

# Normalise to [-1, 1]
norm <- function(x) { 
  r <- range(x)
  (x - r[1]) / (r[2] - r[1]) * 2 - 1 
}
layout_fr[, 1] <- norm(layout_fr[, 1])
layout_fr[, 2] <- norm(layout_fr[, 2])

# ── NODE ATTRIBUTES ──────────────────────────────────────────
node_colors <- actor_pal[igraph::V(g)$actor_type]
node_shapes <- ifelse(igraph::V(g)$category == "institutional node", "square", "circle")
node_opacity <- ifelse(igraph::V(g)$node_status == "historical", 0.4, 0.95)

# Apply opacity to colours
apply_alpha <- function(hex, alpha) {
  rgb <- col2rgb(hex) / 255
  rgb(rgb[1], rgb[2], rgb[3], alpha = alpha)
}
node_colors_alpha <- mapply(apply_alpha, node_colors, node_opacity)

# Size by betweenness (min 4, max 22 points)
node_size <- 4 + (sqrt(btw) / max(sqrt(btw + 1e-9))) * 18

# Edge colour by type - Muted crisp tones
etype_pal <- c(
  "regulatory_authority"   = "#222222",  # Near-black for maximum contrast
  "regulatory_consultation" = "#5A6577",  # Slate gray
  "collaboration"           = "#416F62",  # Deep Muted Sage
  "information"             = "#6384A6",  # Soft Steel Blue
  "commercial_supply"       = "#B86B3D",  # Deep Muted Copper
  "funding"                 = "#3E5C76"   # Navy Blue
)
edge_colors <- etype_pal[igraph::E(g)$relationship_type]

# Edge width by strength
edge_width <- as.numeric(c("1"=0.4, "2"=0.9, "3"=1.6)[as.character(igraph::E(g)$strength)])

# ── HELPER: DRAW LEGEND ──────────────────────────────────────
# Updated text.col, box vectors, and boundary frames to match dark charcoal formatting
draw_legend <- function() {
  legend("bottomleft", 
         legend = c("Regulatory", "Science/Extension", "Advocacy/NGO", "Industry/Operator"), 
         pch = 21, 
         pt.bg = c("#2C5E7A", "#4A8F84", "#7AA874", "#D68A57"), 
         col = "#1A1A1A", 
         pt.cex = 1.5, 
         cex = 0.65, 
         bty = "n", 
         title = expression(bold("Actor Type")), 
         text.col = "#1A1A1A"
  )
  legend("bottomright", 
         legend = c("Regulatory authority", "Reg. consultation", "Collaboration", "Information", "Commercial", "Funding"), 
         lwd = c(1.5, 1.5, 1.5, 1, 1, 1), 
         col = c("#222222", "#5A6577", "#416F62", "#6384A6", "#B86B3D", "#3E5C76"), 
         cex = 0.65, 
         bty = "n", 
         title = expression(bold("Edge Type")), 
         text.col = "#1A1A1A"
  )
  legend("topleft", 
         legend = c("Institutional node", "Individual/org", "Former actor"), 
         pch = c(22, 21, 21), 
         pt.bg = c("#D5D8DC", "#D5D8DC", "#D5D8DC"), 
         col = "#1A1A1A", 
         pt.cex = c(1.6, 1.4, 1.0), 
         cex = 0.65, 
         bty = "n", 
         title = expression(bold("Node Shape/Opacity")), 
         text.col = "#1A1A1A"
  )
}

# Label only top-15 by betweenness
top_idx <- order(btw, decreasing = TRUE)[1:15]
v_labels <- rep("", igraph::vcount(g))
v_labels[top_idx] <- igraph::V(g)$name[top_idx]

# Shorten long labels
v_labels <- gsub("South Carolina Shellfish Growers Association(SCSGA)", "SCSGA", v_labels)
v_labels <- gsub("SC Sea Grant Consortium(SCSGC)", "SCSGC", v_labels)
v_labels <- gsub("SCDNR Shellfish Management Section", "SCDNR SMS", v_labels)
v_labels <- gsub("SCDES Shellfish Sanitation Section", "SCDES SSS", v_labels)
v_labels <- gsub("SCDES Bureau of Coastal Management", "SCDES BCM", v_labels)
v_labels <- gsub("College of Charleston(institutional)", "CofC", v_labels)
v_labels <- gsub("SCDNR MRRI / Shellfish Research", "SCDNR MRRI", v_labels)
v_labels <- gsub("SCDNR Marine Permitting Office.*", "SCDNR MPO", v_labels)

plot_network <- function(label_top = TRUE) {
  igraph::plot.igraph(g, 
                      layout = layout_fr, 
                      vertex.color = node_colors_alpha, 
                      vertex.frame.color = adjustcolor("#1A1A1A", alpha.f = 0.2), # Switched frame border to dark
                      vertex.shape = node_shapes, 
                      vertex.size = node_size, 
                      vertex.label = if (label_top) v_labels else NA, 
                      vertex.label.cex = 0.50, 
                      vertex.label.color = "#1A1A1A", # Switched text labels to charcoal
                      vertex.label.dist = 0.5, 
                      vertex.label.font = 2, 
                      edge.color = adjustcolor(edge_colors, alpha.f = 0.60), 
                      edge.width = edge_width, 
                      edge.arrow.size = 0.12, 
                      edge.curved = 0.15, 
                      margin = c(0, 0, 0, 0), 
                      rescale = FALSE, 
                      xlim = c(-1.05, 1.05), 
                      ylim = c(-1.05, 1.05)
  )
}

# ── FIGURE 1: FULL NETWORK — PNG ─────────────────────────────
cat("Generating Figure 1: Full Network (PNG)...\n")
png("figure_full_network.png", width = 3200, height = 2800, res = 300, bg = "#F8F9FA")
par(mar = c(0.5, 0.5, 2.5, 0.5), bg = "#F8F9FA")
plot_network(label_top = TRUE)
title(main = "SC Shellfish Mariculture Governance Network", 
      sub = paste0("104 nodes · 423 directed edges · Node size = Betweenness Centrality", " · Color = Actor Type | Chatman (2026)"), 
      col.main = "#1A1A1A", col.sub = "#555555", cex.main = 1.1, cex.sub = 0.55, font.main = 2)
draw_legend()
dev.off()
cat(" Saved: figure_full_network.png\n")

# ── FIGURE 1: FULL NETWORK — PDF ─────────────────────────────
cat("Generating Figure 1: Full Network (PDF)...\n")
pdf("figure_full_network.pdf", width = 14, height = 12, bg = "#F8F9FA")
par(mar = c(0.5, 0.5, 2.5, 0.5), bg = "#F8F9FA")
plot_network(label_top = TRUE)
title(main = "SC Shellfish Mariculture Governance Network", 
      sub = paste0("104 nodes · 423 directed edges · Node size = Betweenness Centrality", " · Color = Actor Type | Chatman (2026)"), 
      col.main = "#1A1A1A", col.sub = "#555555", cex.main = 1.1, cex.sub = 0.55, font.main = 2)
draw_legend()
dev.off()
cat(" Saved: figure_full_network.pdf\n")

# ── FIGURE 2: BURDEN SUBGRAPH ─────────────────────────────────
cat("Generating Figure 2: Burden Subgraph...\n")
burden_raw <- edges_raw %>% filter(burden_generating == TRUE)
burden_uni <- burden_raw %>% filter(direction == "unidirectional")
burden_bi_ab <- burden_raw %>% filter(direction == "bidirectional")
burden_bi_ba <- burden_bi_ab %>% rename(from = to, to = from)
burden_directed <- bind_rows(burden_uni, burden_bi_ab, burden_bi_ba)
burden_node_names <- unique(c(burden_directed$from, burden_directed$to))
roster_burden <- roster_raw %>% filter(name %in% burden_node_names)

g_b <- igraph::graph_from_data_frame(d = burden_directed, directed = TRUE, vertices = roster_burden)
btw_b <- igraph::betweenness(g_b, directed = TRUE, normalized = TRUE)

set.seed(2026)
layout_b <- igraph::layout_with_fr(g_b, niter = 800, start.temp = 15)
layout_b[, 1] <- norm(layout_b[, 1])
layout_b[, 2] <- norm(layout_b[, 2])

nc_b <- actor_pal[igraph::V(g_b)$actor_type]
ns_b <- 5 + (sqrt(btw_b) / max(sqrt(btw_b + 1e-9))) * 18
nsh_b <- ifelse(igraph::V(g_b)$category == "institutional node", "square", "circle")

# Labels for all nodes
vlb_b <- igraph::V(g_b)$name
vlb_b <- gsub("South Carolina Shellfish Growers Association(SCSGA)", "SCSGA", vlb_b)
vlb_b <- gsub("SC Sea Grant Consortium(SCSGC)", "SCSGC", vlb_b)
vlb_b <- gsub("SCDNR Shellfish Management Section", "SCDNR SMS", vlb_b)
vlb_b <- gsub("SCDES Shellfish Sanitation Section", "SCDES SSS", vlb_b)
vlb_b <- gsub("SCDES Bureau of Coastal Management", "SCDES BCM", vlb_b)
vlb_b <- gsub("U.S. Army Corps.*", "USACE", vlb_b)
vlb_b <- gsub("College of Charleston(institutional)", "CofC", vlb_b)
vlb_b <- gsub("SCDNR Marine Permitting Office.*", "SCDNR MPO", vlb_b)
vlb_b <- gsub("SC Department of Agriculture.*", "SCDA", vlb_b)

# ── FIGURE 2: BURDEN SUBGRAPH ─────────────────────────────────
cat("Generating Figure 2: Burden Subgraph...\n")
burden_raw <- edges_raw %>% filter(burden_generating == TRUE)
burden_uni <- burden_raw %>% filter(direction == "unidirectional")
burden_bi_ab <- burden_raw %>% filter(direction == "bidirectional")
burden_bi_ba <- burden_bi_ab %>% rename(from = to, to = from)
burden_directed <- bind_rows(burden_uni, burden_bi_ab, burden_bi_ba)
burden_node_names <- unique(c(burden_directed$from, burden_directed$to))
roster_burden <- roster_raw %>% filter(name %in% burden_node_names)

g_b <- igraph::graph_from_data_frame(d = burden_directed, directed = TRUE, vertices = roster_burden)
btw_b <- igraph::betweenness(g_b, directed = TRUE, normalized = TRUE)

set.seed(2026)
layout_b <- igraph::layout_with_fr(g_b, niter = 800, start.temp = 15)
layout_b[, 1] <- norm(layout_b[, 1])
layout_b[, 2] <- norm(layout_b[, 2])

nc_b <- actor_pal[igraph::V(g_b)$actor_type]
ns_b <- 5 + (sqrt(btw_b) / max(sqrt(btw_b + 1e-9))) * 18
nsh_b <- ifelse(igraph::V(g_b)$category == "institutional node", "square", "circle")

# Labels for all nodes
vlb_b <- igraph::V(g_b)$name
vlb_b <- gsub("South Carolina Shellfish Growers Association(SCSGA)", "SCSGA", vlb_b)
vlb_b <- gsub("SC Sea Grant Consortium(SCSGC)", "SCSGC", vlb_b)
vlb_b <- gsub("SCDNR Shellfish Management Section", "SCDNR SMS", vlb_b)
vlb_b <- gsub("SCDES Shellfish Sanitation Section", "SCDES SSS", vlb_b)
vlb_b <- gsub("SCDES Bureau of Coastal Management", "SCDES BCM", vlb_b)
vlb_b <- gsub("U.S. Army Corps.*", "USACE", vlb_b)
vlb_b <- gsub("College of Charleston(institutional)", "CofC", vlb_b)
vlb_b <- gsub("SCDNR Marine Permitting Office.*", "SCDNR MPO", vlb_b)
vlb_b <- gsub("SC Department of Agriculture.*", "SCDA", vlb_b)

# Loop correctly explicitly opens and closes device setups
for (ext in c("png", "pdf")) {
  if (ext == "png") {
    png("figure_burden_network.png", width = 3000, height = 2600, res = 300, bg = "#F8F9FA")
  } else {
    pdf("figure_burden_network.pdf", width = 13, height = 11, bg = "#F8F9FA")
  }
  par(mar = c(0.5, 0.5, 2.5, 0.5), bg = "#F8F9FA")
  
  igraph::plot.igraph(g_b, 
                      layout = layout_b, 
                      vertex.color = nc_b, 
                      vertex.frame.color = adjustcolor("#1A1A1A", alpha.f = 0.2), 
                      vertex.shape = nsh_b, 
                      vertex.size = ns_b, 
                      vertex.label = vlb_b, 
                      vertex.label.cex = 0.50, 
                      vertex.label.color = "#1A1A1A", 
                      vertex.label.dist = 0.55, 
                      vertex.label.font = 2, 
                      edge.color = adjustcolor("#C0392B", alpha.f = 0.65), 
                      edge.width = 1.2, 
                      edge.arrow.size = 0.14, 
                      edge.curved = 0.2, 
                      margin = c(0, 0, 0, 0), 
                      rescale = FALSE, 
                      xlim = c(-1.05, 1.05), 
                      ylim = c(-1.05, 1.05)
  )
  
  title(main = "Administrative Burden-Generating Relationships", 
        sub = paste0("40 nodes · 117 burden-generating edges · Node size = Betweenness Centrality", " | Chatman (2026)"), 
        col.main = "#1A1A1A", col.sub = "#555555", cex.main = 1.1, cex.sub = 0.55, font.main = 2)
  
  legend("bottomleft", 
         legend = c("Regulatory", "Science/Extension", "Advocacy/NGO", "Industry/Operator"), 
         pch = 21, 
         pt.bg = c("#2C5E7A", "#4A8F84", "#7AA874", "#D68A57"), 
         col = "#1A1A1A", 
         pt.cex = 1.5, 
         cex = 0.7, 
         bty = "n", 
         title = expression(bold("Actor Type")), 
         text.col = "#1A1A1A")
  
  dev.off()
  cat(" Saved: figure_burden_network.", ext, "\n", sep="")
}

cat("\nAll figures complete. Files written:\n")
cat("  figure_full_network.png   (300 dpi, thesis-ready)\n")
cat("  figure_full_network.pdf   (vector, for Word/LaTeX)\n")
cat("  figure_burden_network.png (300 dpi, thesis-ready)\n")
cat("  figure_burden_network.pdf (vector, for Word/LaTeX)\n")

