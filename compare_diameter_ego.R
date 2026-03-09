#!/usr/bin/env Rscript
# =============================================================================
# compare_diameter_ego.R
# DiameterとEgo Sizeの計算比較（FIXED vs ORIGINAL）
# =============================================================================

library(igraph)
library(data.table)

cat("======================================================================\n")
cat("Diameter と Ego Size の比較分析\n")
cat("======================================================================\n\n")

# ---- データ読み込み ----
cat("Loading data...\n")
fixed_graph <- readRDS("tmp/fixed/V5P2_24aB_CTCF_2_3000_02_graph.rds")
fixed_memb  <- readRDS("tmp/fixed/V5P2_24aB_CTCF_2_3000_02_membership.rds")
orig_graph  <- readRDS("tmp/V5P2_24aB_CTCF_2_3000_02_graph.rds")
orig_memb   <- readRDS("tmp/V5P2_24aB_CTCF_2_3000_02_membership.rds")

cat("  FIXED: ", vcount(fixed_graph), "nodes, ", ecount(fixed_graph), "edges\n")
cat("  ORIGINAL: ", vcount(orig_graph), "nodes, ", ecount(orig_graph), "edges\n\n")

# ---- セクション1: Diameter計算 ----
cat(rep("=", 70), "\n", sep = "")
cat("1. Diameter（直径）の計算と比較\n")
cat(rep("=", 70), "\n", sep = "")

calculate_diameter <- function(graph, membership_df, label) {
  cat("\nCalculating diameter for:", label, "\n")

  unique_comms <- unique(membership_df$community_id)
  unique_comms <- unique_comms[!is.na(unique_comms)]

  result_list <- list()

  for (comm_id in unique_comms) {
    # membershipからノード名を取得
    nodes_in_comm <- membership_df$name[membership_df$community_id == comm_id]
    n_nodes <- length(nodes_in_comm)

    if (n_nodes < 2) next  # 1ノード以下はスキップ

    # graphからサブグラフを抽出
    tryCatch({
      subgraph <- induced_subgraph(graph, nodes_in_comm)

      # サブグラフが連結かチェック
      is_connected <- is.connected(subgraph)

      if (is_connected && vcount(subgraph) > 1) {
        diam <- diameter(subgraph, directed = FALSE)
      } else {
        diam <- NA  # 非連結の場合はNA
      }

      result_list[[as.character(comm_id)]] <- data.table(
        community_id = comm_id,
        n_nodes      = n_nodes,
        n_vertices   = vcount(subgraph),
        n_edges      = ecount(subgraph),
        is_connected = is_connected,
        diameter     = diam
      )
    }, error = function(e) {
      cat("  Error in community", comm_id, ":", e$message, "\n")
    })
  }

  result_dt <- rbindlist(result_list)
  return(result_dt)
}

cat("Computing diameter for FIXED version...\n")
fixed_diameter <- calculate_diameter(fixed_graph, fixed_memb, "FIXED")

cat("Computing diameter for ORIGINAL version...\n")
orig_diameter <- calculate_diameter(orig_graph, orig_memb, "ORIGINAL")

# 統計サマリー
cat("\n--- Diameter統計 ---\n")
diameter_stats <- data.table(
  Version = c("FIXED", "ORIGINAL"),
  Total_Communities = c(nrow(fixed_diameter), nrow(orig_diameter)),
  Connected_Communities = c(
    sum(fixed_diameter$is_connected, na.rm = TRUE),
    sum(orig_diameter$is_connected, na.rm = TRUE)
  ),
  Mean_Diameter = c(
    mean(fixed_diameter$diameter, na.rm = TRUE),
    mean(orig_diameter$diameter, na.rm = TRUE)
  ),
  Median_Diameter = c(
    median(fixed_diameter$diameter, na.rm = TRUE),
    median(orig_diameter$diameter, na.rm = TRUE)
  ),
  Max_Diameter = c(
    max(fixed_diameter$diameter, na.rm = TRUE),
    max(orig_diameter$diameter, na.rm = TRUE)
  ),
  Communities_With_Diameter = c(
    sum(!is.na(fixed_diameter$diameter)),
    sum(!is.na(orig_diameter$diameter))
  )
)

print(diameter_stats)

# Diameter分布
cat("\n--- Diameter分布 ---\n")
if (sum(!is.na(fixed_diameter$diameter)) > 0) {
  cat("FIXED version:\n")
  print(table(fixed_diameter$diameter, useNA = "ifany"))
}

if (sum(!is.na(orig_diameter$diameter)) > 0) {
  cat("\nORIGINAL version:\n")
  print(table(orig_diameter$diameter, useNA = "ifany"))
} else {
  cat("\nORIGINAL version: 全てNA（計算不可）\n")
}

# ---- セクション2: Ego Size計算 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("2. Ego Size（次数）の計算と比較\n")
cat(rep("=", 70), "\n", sep = "")

calculate_ego_size <- function(graph, membership_df, label) {
  cat("\nCalculating ego size for:", label, "\n")

  unique_comms <- unique(membership_df$community_id)
  unique_comms <- unique_comms[!is.na(unique_comms)]

  result_list <- list()

  for (comm_id in unique_comms) {
    # membershipからノード名を取得
    nodes_in_comm <- membership_df$name[membership_df$community_id == comm_id]
    n_nodes <- length(nodes_in_comm)

    if (n_nodes < 2) next

    # graphからサブグラフを抽出
    tryCatch({
      subgraph <- induced_subgraph(graph, nodes_in_comm)

      # 各ノードの次数を計算
      degrees <- degree(subgraph)

      result_list[[as.character(comm_id)]] <- data.table(
        community_id = comm_id,
        n_nodes      = n_nodes,
        n_vertices   = vcount(subgraph),
        n_edges      = ecount(subgraph),
        mean_degree  = mean(degrees, na.rm = TRUE),
        median_degree = median(degrees, na.rm = TRUE),
        max_degree   = max(degrees, na.rm = TRUE),
        min_degree   = min(degrees, na.rm = TRUE)
      )
    }, error = function(e) {
      cat("  Error in community", comm_id, ":", e$message, "\n")
    })
  }

  result_dt <- rbindlist(result_list)
  return(result_dt)
}

cat("Computing ego size for FIXED version...\n")
fixed_ego <- calculate_ego_size(fixed_graph, fixed_memb, "FIXED")

cat("Computing ego size for ORIGINAL version...\n")
orig_ego <- calculate_ego_size(orig_graph, orig_memb, "ORIGINAL")

# 統計サマリー
cat("\n--- Ego Size統計 ---\n")
ego_stats <- data.table(
  Version = c("FIXED", "ORIGINAL"),
  Total_Communities = c(nrow(fixed_ego), nrow(orig_ego)),
  Overall_Mean_Degree = c(
    mean(fixed_ego$mean_degree, na.rm = TRUE),
    mean(orig_ego$mean_degree, na.rm = TRUE)
  ),
  Overall_Median_Degree = c(
    median(fixed_ego$median_degree, na.rm = TRUE),
    median(orig_ego$median_degree, na.rm = TRUE)
  ),
  Max_Degree = c(
    max(fixed_ego$max_degree, na.rm = TRUE),
    max(orig_ego$max_degree, na.rm = TRUE)
  ),
  Communities_Zero_Degree = c(
    sum(fixed_ego$mean_degree == 0, na.rm = TRUE),
    sum(orig_ego$mean_degree == 0, na.rm = TRUE)
  )
)

print(ego_stats)

# ---- セクション3: 詳細比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("3. サンプルコミュニティの詳細比較（上位10個）\n")
cat(rep("=", 70), "\n", sep = "")

# コミュニティサイズでソート
comm_sizes <- table(fixed_memb$community_id)
large_comms <- as.integer(names(sort(comm_sizes, decreasing = TRUE)[1:10]))

# Diameterの比較
setkey(fixed_diameter, community_id)
setkey(orig_diameter, community_id)

diameter_comparison <- merge(
  fixed_diameter[community_id %in% large_comms],
  orig_diameter[community_id %in% large_comms],
  by = "community_id",
  suffixes = c("_fixed", "_orig"),
  all.x = TRUE
)

cat("\n--- Diameter比較 (上位10コミュニティ) ---\n")
print(diameter_comparison[, .(
  community_id,
  n_nodes_fixed,
  n_edges_fixed,
  is_connected_fixed,
  diameter_fixed,
  n_edges_orig,
  is_connected_orig,
  diameter_orig
)])

# Ego Sizeの比較
setkey(fixed_ego, community_id)
setkey(orig_ego, community_id)

ego_comparison <- merge(
  fixed_ego[community_id %in% large_comms],
  orig_ego[community_id %in% large_comms],
  by = "community_id",
  suffixes = c("_fixed", "_orig"),
  all.x = TRUE
)

cat("\n--- Ego Size比較 (上位10コミュニティ) ---\n")
print(ego_comparison[, .(
  community_id,
  n_nodes_fixed,
  n_edges_fixed,
  mean_degree_fixed,
  max_degree_fixed,
  n_edges_orig,
  mean_degree_orig,
  max_degree_orig
)])

# ---- 全コミュニティの比較 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("4. 全コミュニティのDiameter/Ego Size比較\n")
cat(rep("=", 70), "\n", sep = "")

# 全てのコミュニティでマージ
full_diameter_comparison <- merge(
  fixed_diameter,
  orig_diameter,
  by = "community_id",
  suffixes = c("_fixed", "_orig"),
  all = TRUE
)

full_ego_comparison <- merge(
  fixed_ego,
  orig_ego,
  by = "community_id",
  suffixes = c("_fixed", "_orig"),
  all = TRUE
)

# Diameterの差分
if (nrow(full_diameter_comparison) > 0) {
  cat("\nDiameter差分の統計:\n")
  full_diameter_comparison[, diameter_diff := diameter_fixed - diameter_orig]

  cat("  NA割合 (FIXED):",
      sprintf("%.1f%%", sum(is.na(full_diameter_comparison$diameter_fixed))/nrow(full_diameter_comparison)*100), "\n")
  cat("  NA割合 (ORIGINAL):",
      sprintf("%.1f%%", sum(is.na(full_diameter_comparison$diameter_orig))/nrow(full_diameter_comparison)*100), "\n")

  valid_diffs <- full_diameter_comparison[!is.na(diameter_diff)]
  if (nrow(valid_diffs) > 0) {
    cat("  平均差分:", mean(valid_diffs$diameter_diff, na.rm = TRUE), "\n")
    cat("  最大差分:", max(abs(valid_diffs$diameter_diff), na.rm = TRUE), "\n")
  }
}

# Ego Sizeの差分
if (nrow(full_ego_comparison) > 0) {
  cat("\nEgo Size (mean degree) 差分の統計:\n")
  full_ego_comparison[, degree_diff := mean_degree_fixed - mean_degree_orig]

  cat("  平均差分:", mean(full_ego_comparison$degree_diff, na.rm = TRUE), "\n")
  cat("  最大差分:", max(abs(full_ego_comparison$degree_diff), na.rm = TRUE), "\n")
  cat("  元のバージョンで次数0のコミュニティ:",
      sum(full_ego_comparison$mean_degree_orig == 0, na.rm = TRUE), "/",
      nrow(full_ego_comparison), "\n")
}

# ---- 結果を保存 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("結果の保存\n")
cat(rep("=", 70), "\n", sep = "")

fwrite(diameter_stats, "verification_3000_diameter_stats.csv")
fwrite(ego_stats, "verification_3000_ego_stats.csv")
fwrite(full_diameter_comparison, "verification_3000_diameter_comparison_full.csv")
fwrite(full_ego_comparison, "verification_3000_ego_comparison_full.csv")

cat("\n保存完了:\n")
cat("  - verification_3000_diameter_stats.csv\n")
cat("  - verification_3000_ego_stats.csv\n")
cat("  - verification_3000_diameter_comparison_full.csv\n")
cat("  - verification_3000_ego_comparison_full.csv\n")

# ---- 最終判定 ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("最終判定\n")
cat(rep("=", 70), "\n", sep = "")

diameter_ok <- sum(!is.na(fixed_diameter$diameter)) > 0 &&
               sum(!is.na(orig_diameter$diameter)) == 0

ego_ok <- mean(fixed_ego$mean_degree, na.rm = TRUE) > 0 &&
          mean(orig_ego$mean_degree, na.rm = TRUE) == 0

if (diameter_ok && ego_ok) {
  cat("✓ 修正版は正常にDiameterとEgo Sizeを計算\n")
  cat("✗ 元のバージョンは計算できていない（全てNA/0）\n\n")

  cat("修正版の結果:\n")
  cat("  - Diameter計算可能なコミュニティ:", sum(!is.na(fixed_diameter$diameter)), "\n")
  cat("  - 平均Diameter:", sprintf("%.2f", mean(fixed_diameter$diameter, na.rm = TRUE)), "\n")
  cat("  - 平均次数:", sprintf("%.2f", mean(fixed_ego$mean_degree, na.rm = TRUE)), "\n\n")

  cat("元のバージョンの問題:\n")
  cat("  - Diameter計算可能なコミュニティ:", sum(!is.na(orig_diameter$diameter)), "\n")
  cat("  - 平均次数:", sprintf("%.2f", mean(orig_ego$mean_degree, na.rm = TRUE)), "\n")
  cat("  → ノード順番の不一致により、サブグラフが正しく構築されていない\n")
} else {
  cat("予期しない結果が得られました。\n")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("分析完了\n")
cat(rep("=", 70), "\n\n")
