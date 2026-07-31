# =============================================================================
# edgelist_prep.R
# =============================================================================
# Thesis      : Closing the Governance Gap — Shellfish Mariculture in South Carolina
# Author      : Kate Chatman | MPA/EVSS Dual Degree
#               College of Charleston / S.C. Sea Grant Consortium
# Component   : Stakeholder network analysis
# Used in     : Ch. 7 Network Analysis
# Status      : FINAL — reusable prep
#
# PURPOSE
#   Cleans the edge list for igraph and visNetwork: trims leading/trailing
#   whitespace, removes duplicates, and resolves formatting errors. Run
#   whenever the edge list is updated, before any network script.
#
# INPUTS      : Raw edge list CSV
# OUTPUTS     : Cleaned edge list CSV
# RUN AFTER   : None — entry point
# =============================================================================
# NOTE: This is the archived thesis version. Logic is unchanged from the script
# that produced the reported results; only this header has been added.
# =============================================================================

# A script to clean my edge list for igraph and visnetwork to avoid formating errors and duplicates
# to be run anytime I want to update my edge list, this is the prep before network code

library(dplyr)
library(readr)
library(stringr)

edges_raw <- read_csv("edge_list_may162026.csv",
                      show_col_types = FALSE) 
cat("Rows in raw file:", nrow(raw), "\n")


#we have to trim the whitespaces first both leading and trailing make errors

raw <- edges_raw %>%
  mutate(
    from = str_trim(from),
    to   = str_trim(to)
  )
cat("Whitespace trimmed.\n")


#we have to standardize the node names where they differ between files
# Rule: use the roster spelling as the canonical form.
# If add more mismatches, follow the same pattern:
#   "edge list version" = "roster version"

name_fixes <- c(
  # Edge list spelling                         = Roster spelling
  "Coastal Conservation Association SC Chapter" = "Coastal Conservation Association — SC Chapter",
  "U.S. Fish and Wildlife Service"              = "U.S. Fish and Wildlife Service (USFWS)",
  "SCDNR Marine Permitting Office"              = "SCDNR Marine Permitting Office (Indigenous Molluscan Importation)",
  "South Carolina Shellfish Growers Association" = "South Carolina Shellfish Growers Association (SCSGA)"
)
raw <- raw %>%
  mutate(
    from = recode(from, !!!name_fixes),
    to   = recode(to,   !!!name_fixes)
  )

cat("Node name inconsistencies corrected.\n")


#we have to remove all the duplicates from adding and combining and adding and combining
# distinct() keeps the first occurrence and drops the rest

before_dedup <- nrow(raw)

clean <- raw %>%
  distinct()

after_dedup <- nrow(clean)

cat("Rows before dedup:", before_dedup, "\n")
cat("Rows after dedup: ", after_dedup,  "\n")
cat("Duplicates removed:", before_dedup - after_dedup, "\n")


#validate my vocabulary (check for typos and such)

expected_relationship_types <- c(
  "collaboration", "commercial_supply", "employment",
  "funding", "information", "regulatory_authority",
  "regulatory_consultation"
)

expected_directions <- c("bidirectional", "unidirectional")

expected_strength    <- c(1L, 2L, 3L)
expected_tier        <- c(1L, 2L, 3L)
expected_burden      <- c(TRUE, FALSE)

check_vocab <- function(col, expected, col_name) {
  bad <- setdiff(unique(col), expected)
  if (length(bad) > 0) {
    cat("WARNING —", col_name, "has unexpected values:", paste(bad, collapse = ", "), "\n")
  } else {
    cat("OK —", col_name, "values all valid.\n")
  }
}

check_vocab(clean$relationship_type, expected_relationship_types, "relationship_type")
check_vocab(clean$direction,         expected_directions,          "direction")
check_vocab(clean$strength,          expected_strength,            "strength")
check_vocab(clean$evidence_tier,     expected_tier,                "evidence_tier")
check_vocab(clean$burden_generating, expected_burden,              "burden_generating")


# set my column types as logical or integer so igraph can use them

clean <- clean %>%
  mutate(
    strength          = as.integer(strength),
    evidence_tier     = as.integer(evidence_tier),
    burden_generating = as.logical(burden_generating)
  )

cat("Column types coerced.\n")


#Spot check for errors

all_nodes <- c(clean$from, clean$to)
node_counts <- sort(table(all_nodes), decreasing = TRUE)

cat("\n--- Top 20 most-connected node names (edge appearances) ---\n")
print(head(node_counts, 20))

cat("\n--- Nodes appearing only once (potential isolates or typos) ---\n")
print(names(node_counts[node_counts == 1]))


#Write it clean 


write_csv(clean, "edge_list_CLEAN.csv")

cat("\nDone. Clean file written to: edge_list_CLEAN.csv\n")
cat("Final row count:", nrow(clean), "\n")



#NEXT STEP: Load into igraph
#
#   library(igraph)
#   edges <- read_csv("edge_list_CLEAN.csv")
#   g <- graph_from_data_frame(edges, directed = TRUE)
#
# For undirected (treating all ties as undirected):
#   g <- graph_from_data_frame(edges, directed = FALSE)
#
# Note: igraph uses the 'from' and 'to' columns automatically.
# All other columns become edge attributes accessible via
# E(g)$relationship_type, E(g)$strength, etc.
