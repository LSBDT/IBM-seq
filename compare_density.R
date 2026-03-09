#!/usr/bin/env Rscript
# =============================================================================
# compare_density.R
# 修正版と元のバージョンで密度計算を比較
# =============================================================================

library(igraph)
library(data.table)

# ---- 密度計算関数 ----
calculate_density <- function(graph, membership_df, label) {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("Calculating density for:", label, "\n")
  cat(rep("=", 70), "\n", sep = "")

  unique_comms <- unique(membership_df$community_id)
  unique_comms <- unique_comms[!is.na(unique_comms)]

  result_list <- list()

  for (comm_id in unique_comms) {
    # membershipからノード名を取得
    nodes_in_comm <- membership_df$name[membership_df$community_id == comm_id]
    n_nodes <- length(nodes_in_comm)

    if (n_nodes < 2) next  # 1ノードのコミュニティはスキップ

    # graphからサブグラフを抽出
    subgraph <- induced_subgraph(graph, nodes_in_comm)
    n_edges <- ecount(subgraph)
    n_vertices <- vcount(subgraph)

    # 密度計算
    max_edges <- n_vertices * (n_vertices - 1) / 2
    density <- if (max_edges > 0) n_edges / max_edges else 0

    result_list[[as.character(comm_id)]] <- data.table(
      community_id = comm_id,
      n_nodes      = n_nodes,
      n_vertices   = n_vertices,  # サブグラフ内の実際の頂点数
      n_edges      = n_edges,
      max_edges    = max_edges,
      density      = density
    )
  }

  result_dt <- rbindlist(result_list)
  return(result_dt)
}

# ---- 修正版を読み込み ----
cat("Loading FIXED version...\n")
fixed_graph <- readRDS("tmp/fixed/V5P2_24aB_CTCF_2_3000_02_graph.rds")
fixed_memb  <- readRDS("tmp/fixed/V5P2_24aB_CTCF_2_3000_02_membership.rds")
fixed_density <- calculate_density(fixed_graph, fixed_memb, "FIXED")

# ---- 元のバージョンを読み込み ----
cat("\nLoading ORIGINAL version...\n")
orig_graph <- readRDS("tmp/V5P2_24aB_CTCF_2_3000_02_graph.rds")
orig_memb  <- readRDS("tmp/V5P2_24aB_CTCF_2_3000_02_membership.rds")
orig_density <- calculate_density(orig_graph, orig_memb, "ORIGINAL")

# ---- 比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("COMPARISON\n")
cat(rep("=", 70), "\n", sep = "")

# データをマージ
setkey(fixed_density, community_id)
setkey(orig_density, community_id)

comparison <- merge(
  fixed_density,
  orig_density,
  by = "community_id",
  suffixes = c("_fixed", "_orig")
)

# 差分を計算
comparison[, density_diff := density_fixed - density_orig]
comparison[, n_nodes_match := n_nodes_fixed == n_nodes_orig]

# サマリー統計
cat("\nDensity Statistics:\n")
cat("FIXED version:\n")
cat("  Mean density:  ", mean(fixed_density$density, na.rm = TRUE), "\n")
cat("  Median density:", median(fixed_density$density, na.rm = TRUE), "\n")
cat("  Min density:   ", min(fixed_density$density, na.rm = TRUE), "\n")
cat("  Max density:   ", max(fixed_density$density, na.rm = TRUE), "\n")
cat("  SD:            ", sd(fixed_density$density, na.rm = TRUE), "\n")

cat("\nORIGINAL version:\n")
cat("  Mean density:  ", mean(orig_density$density, na.rm = TRUE), "\n")
cat("  Median density:", median(orig_density$density, na.rm = TRUE), "\n")
cat("  Min density:   ", min(orig_density$density, na.rm = TRUE), "\n")
cat("  Max density:   ", max(orig_density$density, na.rm = TRUE), "\n")
cat("  SD:            ", sd(orig_density$density, na.rm = TRUE), "\n")

cat("\nDifference (Fixed - Original):\n")
cat("  Mean diff:     ", mean(comparison$density_diff, na.rm = TRUE), "\n")
cat("  Median diff:   ", median(comparison$density_diff, na.rm = TRUE), "\n")
cat("  Max abs diff:  ", max(abs(comparison$density_diff), na.rm = TRUE), "\n")

# 大きな差分があるコミュニティ
large_diff <- comparison[abs(density_diff) > 0.1]
cat("\nCommunities with |density_diff| > 0.1:", nrow(large_diff), "\n")

if (nrow(large_diff) > 0) {
  cat("\nTop 10 communities with largest density differences:\n")
  large_diff[, abs_diff := abs(density_diff)]
  setorder(large_diff, -abs_diff)
  print(head(large_diff[, .(community_id, n_nodes_fixed, density_fixed, density_orig, density_diff)], 10))
}

# ノード数が一致しないコミュニティ
node_mismatch <- comparison[n_nodes_match == FALSE]
cat("\nCommunities with different node counts:", nrow(node_mismatch), "\n")

if (nrow(node_mismatch) > 0) {
  cat("\nTop 10 communities with node count mismatches:\n")
  print(head(node_mismatch[, .(community_id, n_nodes_fixed, n_nodes_orig,
                               density_fixed, density_orig)], 10))
}

# ---- 結果を保存 ----
fwrite(comparison, "density_comparison.csv")
cat("\nFull comparison saved to: density_comparison.csv\n")

# ---- 密度分布のヒストグラム（テキストベース）----
cat("\n", rep("=", 70), "\n", sep = "")
cat("Density Distribution (binned)\n")
cat(rep("=", 70), "\n", sep = "")

bins <- seq(0, 1, by = 0.1)
fixed_hist <- hist(fixed_density$density, breaks = bins, plot = FALSE)
orig_hist  <- hist(orig_density$density, breaks = bins, plot = FALSE)

cat("\nBin Range       | FIXED count | ORIGINAL count | Difference\n")
cat(rep("-", 70), "\n", sep = "")
for (i in seq_along(fixed_hist$mids)) {
  cat(sprintf("[%.1f - %.1f)  | %11d | %14d | %+10d\n",
              bins[i], bins[i+1],
              fixed_hist$counts[i],
              orig_hist$counts[i],
              fixed_hist$counts[i] - orig_hist$counts[i]))
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("CONCLUSION\n")
cat(rep("=", 70), "\n", sep = "")

if (mean(abs(comparison$density_diff), na.rm = TRUE) < 0.01) {
  cat("The density values are very similar between fixed and original versions.\n")
  cat("This suggests the node order issue may not have affected density calculations significantly.\n")
} else {
  cat("Significant differences detected in density calculations.\n")
  cat("The fix has corrected the density calculation issues.\n")
}
