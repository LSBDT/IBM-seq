#!/usr/bin/env Rscript
# =============================================================================
# verify_fixed_version.R
# 修正版と元のバージョンを比較
# =============================================================================

library(igraph)
library(data.table)

# ---- 修正版の検証 ----
cat("======================================================================\n")
cat("Verifying FIXED version\n")
cat("======================================================================\n\n")

graph_file <- "tmp/fixed/V5P2_24aB_CTCF_2_3000_02_graph.rds"
memb_file  <- "tmp/fixed/V5P2_24aB_CTCF_2_3000_02_membership.rds"

graph         <- readRDS(graph_file)
df_membership <- readRDS(memb_file)

cat("Graph vertices:", vcount(graph), "\n")
cat("Membership rows:", nrow(df_membership), "\n\n")

# ---- 1. 基本的な順番チェック ----
cat("--- Basic Order Check ---\n")
graph_names      <- V(graph)$name
membership_names <- df_membership$name

cat("First 10 nodes in graph:\n")
print(head(graph_names, 10))
cat("\nFirst 10 nodes in membership:\n")
print(head(membership_names, 10))

# 完全一致チェック
identical_order <- identical(graph_names, membership_names)
cat("\n*** Identical order (all positions):", identical_order, "***\n")

if (!identical_order) {
  mismatch_idx <- which(graph_names != membership_names)
  n_mismatch <- length(mismatch_idx)
  cat("ERROR: Number of mismatched positions:", n_mismatch, "/", length(graph_names), "\n")
  cat("First 5 mismatches:\n")
  for (i in head(mismatch_idx, 5)) {
    cat(sprintf("  Position %d: graph='%s' vs membership='%s'\n",
                i, graph_names[i], membership_names[i]))
  }
} else {
  cat("SUCCESS: All node positions match!\n")
}

# ---- 2. コミュニティID対応チェック ----
cat("\n--- Community ID Mapping Check ---\n")

graph_comm_id <- V(graph)$community_id
memb_comm_id  <- df_membership$community_id

# community_idが一致するかチェック
comm_match <- identical(graph_comm_id, memb_comm_id)
cat("*** Community IDs match at same positions:", comm_match, "***\n")

# 各コミュニティのノード集合を比較
cat("\nChecking node sets for each community (all communities)...\n")
unique_comms <- unique(graph_comm_id)
unique_comms <- unique_comms[!is.na(unique_comms)]

mismatch_count <- 0
total_tested <- 0

for (comm_id in unique_comms) {
  total_tested <- total_tested + 1
  # graphから該当するノード名を取得
  nodes_graph <- sort(graph_names[graph_comm_id == comm_id])
  # membershipから該当するノード名を取得
  nodes_memb  <- sort(membership_names[memb_comm_id == comm_id])

  if (!setequal(nodes_graph, nodes_memb)) {
    mismatch_count <- mismatch_count + 1
    if (mismatch_count <= 5) {  # 最初の5個だけ詳細表示
      cat(sprintf("  Community %s: MISMATCH (graph: %d nodes, membership: %d nodes)\n",
                  comm_id, length(nodes_graph), length(nodes_memb)))
      # 差分を表示
      only_graph <- setdiff(nodes_graph, nodes_memb)
      only_memb  <- setdiff(nodes_memb, nodes_graph)
      if (length(only_graph) > 0) {
        cat("    Only in graph:", head(only_graph, 3), "...\n")
      }
      if (length(only_memb) > 0) {
        cat("    Only in membership:", head(only_memb, 3), "...\n")
      }
    }
  }
}

cat(sprintf("\n*** Tested ALL %d communities: %d mismatches ***\n", total_tested, mismatch_count))

if (mismatch_count == 0) {
  cat("\nSUCCESS: All community node sets match!\n")
} else {
  cat("\nERROR: Some communities have mismatched node sets!\n")
}

# ---- 3. 元のバージョン（test）と比較 ----
cat("\n======================================================================\n")
cat("Comparing with ORIGINAL version (tmp/)\n")
cat("======================================================================\n\n")

orig_graph_file <- "tmp/V5P2_24aB_CTCF_2_3000_02_graph.rds"
orig_memb_file  <- "tmp/V5P2_24aB_CTCF_2_3000_02_membership.rds"

if (file.exists(orig_graph_file) && file.exists(orig_memb_file)) {
  orig_graph <- readRDS(orig_graph_file)
  orig_memb  <- readRDS(orig_memb_file)

  orig_graph_names <- V(orig_graph)$name
  orig_memb_names  <- orig_memb$name

  orig_identical <- identical(orig_graph_names, orig_memb_names)
  cat("Original version - Identical order:", orig_identical, "\n")

  if (!orig_identical) {
    orig_mismatch <- sum(orig_graph_names != orig_memb_names)
    cat("Original version - Mismatched positions:", orig_mismatch, "/", length(orig_graph_names), "\n")
  }
} else {
  cat("Original version files not found.\n")
}

# ---- まとめ ----
cat("\n======================================================================\n")
cat("SUMMARY\n")
cat("======================================================================\n")
cat("Fixed version:\n")
cat("  - Node order identical:     ", identical_order, "\n")
cat("  - Community IDs match:      ", comm_match, "\n")
cat("  - Community sets mismatched:", mismatch_count, "/", total_tested, "\n")
cat("\n")

if (identical_order && comm_match && mismatch_count == 0) {
  cat("*** ALL CHECKS PASSED! ***\n")
  cat("The fix successfully resolved the node order issue.\n")
} else {
  cat("*** SOME CHECKS FAILED ***\n")
  cat("Please review the errors above.\n")
}
