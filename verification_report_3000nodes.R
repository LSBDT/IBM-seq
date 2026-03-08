#!/usr/bin/env Rscript
# =============================================================================
# verification_report_3000nodes.R
# 3000ノードテストデータでの包括的な検証レポート
# =============================================================================

library(igraph)
library(data.table)

cat("======================================================================\n")
cat("3000ノードテストデータでの修正版検証レポート\n")
cat("======================================================================\n\n")

# ---- データ読み込み ----
cat("Loading data...\n")
fixed_graph <- readRDS("tmp/fixed/V5P2_24aB_CTCF_2_3000_02_graph.rds")
fixed_memb  <- readRDS("tmp/fixed/V5P2_24aB_CTCF_2_3000_02_membership.rds")
orig_graph  <- readRDS("tmp/V5P2_24aB_CTCF_2_3000_02_graph.rds")
orig_memb   <- readRDS("tmp/V5P2_24aB_CTCF_2_3000_02_membership.rds")

# ---- セクション1: ノード順番の検証 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("1. ノード順番の検証\n")
cat(rep("=", 70), "\n", sep = "")

# FIXED版
fixed_graph_names <- V(fixed_graph)$name
fixed_memb_names  <- fixed_memb$name
fixed_order_match <- identical(fixed_graph_names, fixed_memb_names)

# ORIGINAL版
orig_graph_names <- V(orig_graph)$name
orig_memb_names  <- orig_memb$name
orig_order_match <- identical(orig_graph_names, orig_memb_names)

order_summary <- data.table(
  Version = c("FIXED", "ORIGINAL"),
  Total_Nodes = c(length(fixed_graph_names), length(orig_graph_names)),
  Order_Identical = c(fixed_order_match, orig_order_match),
  Mismatched_Positions = c(
    sum(fixed_graph_names != fixed_memb_names),
    sum(orig_graph_names != orig_memb_names)
  )
)

print(order_summary)

# ---- セクション2: コミュニティIDマッピングの検証 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("2. コミュニティIDマッピングの検証\n")
cat(rep("=", 70), "\n", sep = "")

# FIXED版
fixed_comm_id <- V(fixed_graph)$community_id
fixed_memb_comm_id <- fixed_memb$community_id
fixed_comm_match <- identical(fixed_comm_id, fixed_memb_comm_id)

# ORIGINAL版
orig_comm_id <- V(orig_graph)$community_id
orig_memb_comm_id <- orig_memb$community_id
orig_comm_match <- identical(orig_comm_id, orig_memb_comm_id)

cat("Community ID match at same positions:\n")
cat("  FIXED:    ", fixed_comm_match, "\n")
cat("  ORIGINAL: ", orig_comm_match, "\n\n")

# 各コミュニティのノード集合を検証
verify_community_sets <- function(graph_names, memb_names, graph_comm_id, memb_comm_id, label) {
  unique_comms <- unique(graph_comm_id)
  unique_comms <- unique_comms[!is.na(unique_comms)]

  mismatch_count <- 0
  for (comm_id in unique_comms) {
    nodes_graph <- sort(graph_names[graph_comm_id == comm_id])
    nodes_memb  <- sort(memb_names[memb_comm_id == comm_id])

    if (!setequal(nodes_graph, nodes_memb)) {
      mismatch_count <- mismatch_count + 1
    }
  }

  return(data.table(
    Version = label,
    Total_Communities = length(unique_comms),
    Mismatched_Communities = mismatch_count,
    Match_Rate = sprintf("%.1f%%", (1 - mismatch_count/length(unique_comms)) * 100)
  ))
}

fixed_comm_sets <- verify_community_sets(fixed_graph_names, fixed_memb_names,
                                         fixed_comm_id, fixed_memb_comm_id, "FIXED")
orig_comm_sets  <- verify_community_sets(orig_graph_names, orig_memb_names,
                                         orig_comm_id, orig_memb_comm_id, "ORIGINAL")

comm_summary <- rbind(fixed_comm_sets, orig_comm_sets)
print(comm_summary)

# ---- セクション3: 密度計算の比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("3. 密度計算の比較\n")
cat(rep("=", 70), "\n", sep = "")

# 密度計算関数
calc_density_stats <- function(graph, membership_df, label) {
  unique_comms <- unique(membership_df$community_id)
  unique_comms <- unique_comms[!is.na(unique_comms)]

  densities <- numeric(length(unique_comms))
  edge_counts <- integer(length(unique_comms))
  node_counts <- integer(length(unique_comms))

  for (i in seq_along(unique_comms)) {
    comm_id <- unique_comms[i]
    nodes <- membership_df$name[membership_df$community_id == comm_id]

    if (length(nodes) < 2) next

    subgraph <- induced_subgraph(graph, nodes)
    n_edges <- ecount(subgraph)
    n_vertices <- vcount(subgraph)
    max_edges <- n_vertices * (n_vertices - 1) / 2

    densities[i] <- if (max_edges > 0) n_edges / max_edges else 0
    edge_counts[i] <- n_edges
    node_counts[i] <- length(nodes)
  }

  # 統計量を計算
  return(list(
    densities = densities,
    edge_counts = edge_counts,
    node_counts = node_counts,
    stats = data.table(
      Version = label,
      Mean_Density = mean(densities, na.rm = TRUE),
      Median_Density = median(densities, na.rm = TRUE),
      SD_Density = sd(densities, na.rm = TRUE),
      Min_Density = min(densities, na.rm = TRUE),
      Max_Density = max(densities, na.rm = TRUE),
      Communities_Zero_Density = sum(densities == 0, na.rm = TRUE),
      Communities_High_Density = sum(densities > 0.9, na.rm = TRUE),
      Mean_Edges = mean(edge_counts, na.rm = TRUE),
      Total_Edges_In_Communities = sum(edge_counts, na.rm = TRUE)
    )
  ))
}

cat("Computing densities for FIXED version...\n")
fixed_density_result <- calc_density_stats(fixed_graph, fixed_memb, "FIXED")

cat("Computing densities for ORIGINAL version...\n")
orig_density_result <- calc_density_stats(orig_graph, orig_memb, "ORIGINAL")

density_comparison <- rbind(fixed_density_result$stats, orig_density_result$stats)
print(density_comparison)

# ---- セクション4: 密度分布の詳細比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("4. 密度分布の詳細比較\n")
cat(rep("=", 70), "\n", sep = "")

bins <- seq(0, 1, by = 0.1)
fixed_hist <- hist(fixed_density_result$densities, breaks = bins, plot = FALSE)
orig_hist  <- hist(orig_density_result$densities, breaks = bins, plot = FALSE)

density_dist <- data.table(
  Bin_Range = sprintf("[%.1f-%.1f)", bins[-length(bins)], bins[-1]),
  FIXED_Count = fixed_hist$counts,
  ORIGINAL_Count = orig_hist$counts,
  Difference = fixed_hist$counts - orig_hist$counts
)

print(density_dist)

# ---- セクション5: エッジ数の比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("5. コミュニティ内エッジ数の比較\n")
cat(rep("=", 70), "\n", sep = "")

edge_comparison <- data.table(
  Version = c("FIXED", "ORIGINAL"),
  Communities_With_Edges = c(
    sum(fixed_density_result$edge_counts > 0),
    sum(orig_density_result$edge_counts > 0)
  ),
  Communities_No_Edges = c(
    sum(fixed_density_result$edge_counts == 0),
    sum(orig_density_result$edge_counts == 0)
  ),
  Mean_Edges = c(
    mean(fixed_density_result$edge_counts[fixed_density_result$edge_counts > 0], na.rm = TRUE),
    mean(orig_density_result$edge_counts[orig_density_result$edge_counts > 0], na.rm = TRUE)
  ),
  Total_Edges = c(
    sum(fixed_density_result$edge_counts),
    sum(orig_density_result$edge_counts)
  )
)

print(edge_comparison)

# ---- セクション6: サンプルコミュニティの詳細比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("6. サンプルコミュニティの詳細比較 (上位10個)\n")
cat(rep("=", 70), "\n", sep = "")

# コミュニティサイズでソート
comm_sizes <- table(fixed_memb$community_id)
large_comms <- as.integer(names(sort(comm_sizes, decreasing = TRUE)[1:10]))

sample_comparison <- data.table()
for (comm_id in large_comms) {
  # FIXED
  fixed_nodes <- fixed_memb$name[fixed_memb$community_id == comm_id]
  fixed_sg <- induced_subgraph(fixed_graph, fixed_nodes)

  # ORIGINAL
  orig_nodes <- orig_memb$name[orig_memb$community_id == comm_id]
  orig_sg <- induced_subgraph(orig_graph, orig_nodes)

  sample_comparison <- rbind(sample_comparison, data.table(
    Community_ID = comm_id,
    Nodes_FIXED = length(fixed_nodes),
    Edges_FIXED = ecount(fixed_sg),
    Density_FIXED = ifelse(vcount(fixed_sg) > 1,
                           ecount(fixed_sg) / (vcount(fixed_sg) * (vcount(fixed_sg) - 1) / 2),
                           0),
    Nodes_ORIGINAL = length(orig_nodes),
    Edges_ORIGINAL = ecount(orig_sg),
    Density_ORIGINAL = ifelse(vcount(orig_sg) > 1,
                              ecount(orig_sg) / (vcount(orig_sg) * (vcount(orig_sg) - 1) / 2),
                              0)
  ))
}

print(sample_comparison)

# ---- 結果を保存 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("結果の保存\n")
cat(rep("=", 70), "\n", sep = "")

fwrite(order_summary, "verification_3000_order_summary.csv")
fwrite(comm_summary, "verification_3000_community_summary.csv")
fwrite(density_comparison, "verification_3000_density_comparison.csv")
fwrite(density_dist, "verification_3000_density_distribution.csv")
fwrite(edge_comparison, "verification_3000_edge_comparison.csv")
fwrite(sample_comparison, "verification_3000_sample_communities.csv")

cat("\n保存完了:\n")
cat("  - verification_3000_order_summary.csv\n")
cat("  - verification_3000_community_summary.csv\n")
cat("  - verification_3000_density_comparison.csv\n")
cat("  - verification_3000_density_distribution.csv\n")
cat("  - verification_3000_edge_comparison.csv\n")
cat("  - verification_3000_sample_communities.csv\n")

# ---- 最終判定 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("最終判定\n")
cat(rep("=", 70), "\n", sep = "")

all_tests_passed <-
  fixed_order_match &&
  fixed_comm_match &&
  (fixed_comm_sets$Mismatched_Communities == 0) &&
  (fixed_density_result$stats$Mean_Density > 0.5) &&
  (orig_density_result$stats$Mean_Density < 0.01)

if (all_tests_passed) {
  cat("✓ 全てのテストに合格しました！\n\n")
  cat("修正版 (FIXED) の結果:\n")
  cat("  - ノード順番: 完全一致\n")
  cat("  - コミュニティマッピング: 完全一致\n")
  cat("  - 平均密度: ", sprintf("%.3f", fixed_density_result$stats$Mean_Density), "\n")
  cat("  - エッジを持つコミュニティ: ",
      sum(fixed_density_result$edge_counts > 0), " / ",
      length(fixed_density_result$edge_counts), "\n\n")

  cat("元のバージョン (ORIGINAL) の問題:\n")
  cat("  - ノード順番: ", orig_order_match, " (不一致数: ", sum(orig_graph_names != orig_memb_names), ")\n")
  cat("  - コミュニティノード集合: ", orig_comm_sets$Mismatched_Communities,
      " / ", orig_comm_sets$Total_Communities, " コミュニティで不一致\n")
  cat("  - 平均密度: ", sprintf("%.3f", orig_density_result$stats$Mean_Density), " (ほぼ全てのコミュニティで0)\n")
  cat("  - エッジを持つコミュニティ: ",
      sum(orig_density_result$edge_counts > 0), " / ",
      length(orig_density_result$edge_counts), "\n\n")

  cat("結論: sort = FALSE の追加により、問題が完全に解決されました。\n")
} else {
  cat("✗ 一部のテストに失敗しました。\n")
  cat("詳細は上記の出力を確認してください。\n")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("レポート作成完了\n")
cat(rep("=", 70), "\n\n")
