#!/usr/bin/env Rscript
# =============================================================================
# verify_node_order.R
# graph.rds と membership.rds のノード順番の一致を検証
# =============================================================================

library(igraph)
library(data.table)

# ---- ファイルパス設定 ----
# 比較する3つのバージョン
versions <- list(
  sm         = "tmp/sm",
  test       = "tmp",
  test_no_rds = "tmp/test_no_rds"
)

prefix <- "V5P2_24aB_CTCF_2_3000"

# ---- 検証関数 ----
verify_node_order <- function(graph_path, membership_path, label) {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("Verifying:", label, "\n")
  cat(rep("=", 70), "\n", sep = "")

  # ファイル読み込み
  if (!file.exists(graph_path)) {
    cat("ERROR: Graph file not found:", graph_path, "\n")
    return(NULL)
  }
  if (!file.exists(membership_path)) {
    cat("ERROR: Membership file not found:", membership_path, "\n")
    return(NULL)
  }

  graph         <- readRDS(graph_path)
  df_membership <- readRDS(membership_path)

  cat("Graph vertices:", vcount(graph), "\n")
  cat("Membership rows:", nrow(df_membership), "\n\n")

  # ---- 1. 基本的な順番チェック ----
  cat("--- Basic Order Check ---\n")
  graph_names      <- V(graph)$name
  membership_names <- df_membership$name

  # 先頭10個を比較
  cat("First 10 nodes in graph:\n")
  print(head(graph_names, 10))
  cat("\nFirst 10 nodes in membership:\n")
  print(head(membership_names, 10))

  # 完全一致チェック
  identical_order <- identical(graph_names, membership_names)
  cat("\nIdentical order (all positions):", identical_order, "\n")

  if (!identical_order) {
    # 不一致の箇所を探す
    mismatch_idx <- which(graph_names != membership_names)
    n_mismatch <- length(mismatch_idx)
    cat("Number of mismatched positions:", n_mismatch, "/", length(graph_names), "\n")
    if (n_mismatch > 0) {
      cat("First 5 mismatches:\n")
      for (i in head(mismatch_idx, 5)) {
        cat(sprintf("  Position %d: graph='%s' vs membership='%s'\n",
                    i, graph_names[i], membership_names[i]))
      }
    }
  }

  # ---- 2. コミュニティID対応チェック ----
  cat("\n--- Community ID Mapping Check ---\n")

  # graphのcommunity_id属性
  graph_comm_id <- V(graph)$community_id
  memb_comm_id  <- df_membership$community_id

  if (is.null(graph_comm_id)) {
    cat("WARNING: Graph has no community_id attribute\n")
  } else {
    # community_idが一致するかチェック
    comm_match <- identical(graph_comm_id, memb_comm_id)
    cat("Community IDs match at same positions:", comm_match, "\n")

    # 各コミュニティのノード集合を比較
    cat("\nChecking node sets for each community...\n")
    unique_comms <- unique(graph_comm_id)
    unique_comms <- unique_comms[!is.na(unique_comms)]
    n_test <- min(10, length(unique_comms))  # 最大10個のコミュニティをテスト

    test_comms <- sample(unique_comms, n_test)
    mismatch_count <- 0

    for (comm_id in test_comms) {
      # graphから該当するノード名を取得
      nodes_graph <- sort(graph_names[graph_comm_id == comm_id])
      # membershipから該当するノード名を取得
      nodes_memb  <- sort(membership_names[memb_comm_id == comm_id])

      if (!setequal(nodes_graph, nodes_memb)) {
        mismatch_count <- mismatch_count + 1
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

    cat(sprintf("\nTested %d communities: %d mismatches\n", n_test, mismatch_count))
  }

  # ---- 3. 結果をdatatableで返す ----
  result <- data.table(
    version           = label,
    n_vertices        = vcount(graph),
    n_membership_rows = nrow(df_membership),
    order_identical   = identical_order,
    has_comm_id       = !is.null(graph_comm_id),
    comm_id_match     = if (!is.null(graph_comm_id)) comm_match else NA,
    tested_communities = if (!is.null(graph_comm_id)) n_test else 0,
    mismatched_comms  = if (!is.null(graph_comm_id)) mismatch_count else NA
  )

  return(result)
}

# ---- 全バージョンを検証 ----
results_list <- list()

for (ver_name in names(versions)) {
  ver_path <- versions[[ver_name]]
  graph_file <- file.path(ver_path, paste0(prefix, "_02_graph.rds"))
  memb_file  <- file.path(ver_path, paste0(prefix, "_02_membership.rds"))

  result <- verify_node_order(graph_file, memb_file, ver_name)
  if (!is.null(result)) {
    results_list[[ver_name]] <- result
  }
}

# ---- 結果サマリー ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("SUMMARY TABLE\n")
cat(rep("=", 70), "\n", sep = "")

if (length(results_list) > 0) {
  summary_dt <- rbindlist(results_list)
  print(summary_dt)

  # 結果を保存
  fwrite(summary_dt, "verify_node_order_summary.csv")
  cat("\nSummary saved to: verify_node_order_summary.csv\n")
} else {
  cat("No valid results to display.\n")
}
