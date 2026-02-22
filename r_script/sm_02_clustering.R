# =============================================================================
# sm_02_clustering.R  [suffix mode]
# Louvainクラスタリング（連結成分ごと）+ ノード名サフィックスの整数コード化
#
# 02_clustering.R との差分:
#   - df_membership 構築後に node_type 列（整数）を追加して保存する
#   - node_type は run_umi_uei() の grepl ループを置き換えるための事前計算
#
# node_type 対応表（sm_04_features.R の NODE_TYPE_LABELS と一致させること）:
#   1 = UMI1 (.m1)   2 = UMI2 (.t2)   3 = UEI1 (.e1)   4 = UEI2 (.e2)
#   NA = 上記以外のサフィックス
#
# Input : {save_path}/{name}_01_graph.rds
#
# Output: {save_path}/{name}_02_graph.rds
#         {save_path}/{name}_02_membership.rds  ← node_type 列を含む
#         {save_path}/{name}_02_community.rds
#         {save_path}/{name}_02_membership.tsv
#         {save_path}/{name}_02_summary.txt
#
# 使い方（スタンドアロン）:
#   Rscript sm_02_clustering.R <name> <save_path>
# =============================================================================

# サフィックス → 整数コード 対応表
# sm_04_features.R の NODE_TYPE_LABELS と対応が取れていること
NODE_TYPE_MAP <- c(m1 = 1L, t2 = 2L, e1 = 3L, e2 = 4L)

run_clustering <- function(name, save_path, num_cores = 1L, louvain_min_size = 3L,
                           dedup_after = FALSE) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[sm_02_clustering] START: ", Sys.time(),
                   "  mode=suffix  num_cores=", num_cores,
                   "  louvain_min_size=", louvain_min_size,
                   "  dedup_after=", dedup_after))

  # ---- Step1 出力を読み込み ----
  graph_file <- file.path(save_path, paste0(name, "_01_graph.rds"))
  if (!file.exists(graph_file)) stop(paste("RDS not found:", graph_file))
  graph <- readRDS(graph_file)
  write_log(paste0("  Loaded graph: V=", vcount(graph), "  E=", ecount(graph)))

  # ---- 頂点属性テーブル構築 ----
  write_log("  Building vertex attribute table (Target type counts)...")
  df      <- igraph::as_data_frame(graph, "both")
  df_from <- data.frame(to = df$edges[["from"]], Target2 = df$edges[["Target1"]],
                        stringsAsFactors = FALSE)
  df_to   <- df$edges[, c("to", "Target2"), drop = FALSE]
  df_list <- rbind(df_from, df_to)

  test_dt   <- setDT(df_list)[, .(Count = .N), by = .(to, Target2)]
  test_wide <- dcast(test_dt, to ~ Target2, value.var = "Count", fill = 0L)
  df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                       by.x = "name", by.y = "to", all.x = TRUE)
  for (col in setdiff(names(df$vertices), "name")) {
    df$vertices[[col]][is.na(df$vertices[[col]])] <- 0L
  }
  rm(df_from, df_to, df_list, test_dt, test_wide)
  gc(verbose = FALSE)

  # ---- 連結成分の取得 ----
  write_log("  Computing connected components...")
  comp     <- components(graph)
  clusters <- comp$membership
  n_comp   <- comp$no
  V(graph)$subgraph_id <- clusters
  write_log(paste0("  Connected components: ", n_comp))

  # ---- コンポーネントサイズ分類 ----
  comp_dt      <- data.table(node_idx = seq_len(vcount(graph)),
                              comp_id  = as.integer(clusters))
  comp_size_dt <- comp_dt[, .(size = .N), by = comp_id]
  large_ids    <- comp_size_dt[size >= louvain_min_size, comp_id]
  n_large      <- length(large_ids)
  n_trivial    <- n_comp - n_large
  write_log(paste0("  trivial(<", louvain_min_size, "): ", n_trivial,
                   "  Louvain(>=", louvain_min_size, "): ", n_large,
                   "  (num_cores=", num_cores, ")"))

  community_id <- as.integer(clusters)
  offset       <- as.integer(n_comp)

  if (n_large > 0) {
    write_log(paste0("  Running Louvain on ", n_large, " large components..."))
    large_comp_nodes <- comp_dt[comp_id %in% large_ids,
                                 .(nodes = list(node_idx)), by = comp_id][order(comp_id)]

    lv_raw <- parallel::mclapply(large_comp_nodes$nodes, function(nodes) {
      subg <- induced_subgraph(graph, nodes)
      lv   <- cluster_louvain(subg)
      rm(subg)
      as.integer(membership(lv))
    }, mc.cores = num_cores)

    for (i in seq_along(large_comp_nodes$nodes)) {
      nodes               <- large_comp_nodes$nodes[[i]]
      memb                <- lv_raw[[i]] + offset
      community_id[nodes] <- memb
      offset              <- offset + max(lv_raw[[i]])
      lv_raw[[i]]         <- integer(0)  # スロットを残しつつデータ解放
    }
    rm(lv_raw, large_comp_nodes)
    gc(verbose = FALSE)
    write_log(paste0("  Louvain done. Total communities: ", offset))
  }

  V(graph)$community_id <- community_id

  # ---- [--dup-then-dedup] クラスタリング後に重複エッジを除去 ----
  if (dedup_after) {
    write_log("  [--dup-then-dedup] Simplifying graph after clustering...")
    graph <- simplify(graph,
                      remove.multiple = TRUE,
                      remove.loops    = TRUE,
                      edge.attr.comb  = "first")
    write_log(paste0("  Simplified: V=", vcount(graph), "  E=", ecount(graph)))
  }

  gc(verbose = FALSE)

  # ---- membership データフレーム構築 ----
  df_membership <- cbind(
    df$vertices,
    subgraph_id  = clusters,
    community_id = community_id
  )

  # ---- [suffix mode] node_type 整数列を追加 ----
  # ノード名末尾のサフィックスを抽出し NODE_TYPE_MAP で整数コードに変換（全ノード一括）
  write_log("  [suffix mode] Encoding node_type from name suffix (vectorized)...")
  t_sfx <- system.time({
    sfx <- sub("^.*\\.([^.]+)$", "\\1", df_membership$name)
    df_membership$node_type <- NODE_TYPE_MAP[sfx]
  })
  n_typed <- sum(!is.na(df_membership$node_type))
  write_log(paste0("  node_type encoded: ", n_typed, "/", nrow(df_membership),
                   " nodes mapped  (", round(t_sfx["elapsed"], 3), "s)"))

  n_community <- max(community_id, na.rm = TRUE)
  write_log(paste0("  Total communities: ", n_community))
  write_log("  Membership head:")
  write_log(capture.output(head(df_membership)))

  # ---- RDS 保存 ----
  saveRDS(graph,         file.path(save_path, paste0(name, "_02_graph.rds")))
  saveRDS(df_membership, file.path(save_path, paste0(name, "_02_membership.rds")))
  saveRDS(list(n_large = n_large, n_trivial = n_trivial, louvain_min_size = louvain_min_size),
          file.path(save_path, paste0(name, "_02_community.rds")))
  write_log(paste0("  Saved: ", name, "_02_graph.rds / _02_membership.rds / _02_community.rds"))

  # ---- テキスト出力 ----
  tsv_out <- file.path(save_path, paste0(name, "_02_membership.tsv"))
  fwrite(as.data.table(df_membership), tsv_out, sep = "\t", quote = FALSE)
  write_log(paste0("  Saved: ", tsv_out))

  # ---- サマリーテキスト ----
  summary_out <- file.path(save_path, paste0(name, "_02_summary.txt"))
  comm_table  <- sort(table(community_id), decreasing = TRUE)
  sink(summary_out)
  cat("=== Clustering Summary [suffix mode] ===\n")
  cat("Connected components :", n_comp,      "\n")
  cat("Total communities    :", n_community, "\n")
  cat("\nnode_type distribution:\n")
  print(table(df_membership$node_type, useNA = "ifany"))
  cat("\nCommunity size distribution (top 20):\n")
  print(head(comm_table, 20))
  cat("\nMembership head:\n")
  print(head(df_membership))
  sink()
  write_log(paste0("  Saved: ", summary_out))
  write_log(paste0("[sm_02_clustering] DONE: ", Sys.time()))

  invisible(list(graph = graph, membership = df_membership))
}

# =============================================================================
# スタンドアロン実行ブロック
# =============================================================================
if (!exists("IBMSEQ_SOURCED")) {
  suppressPackageStartupMessages({
    library(data.table)
    library(igraph)
    library(parallel)
  })

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) stop("Usage: Rscript sm_02_clustering.R <name> <save_path> [--cores=N] [--louvain-min=N] [--dedup-after]")
  name             <- args[1]
  save_path        <- args[2]
  num_cores        <- 1L
  louvain_min_size <- 3L
  dedup_after      <- FALSE

  for (a in args[-(1:2)]) {
    if (grepl("^--cores=[0-9]+$", a))       num_cores        <- as.integer(sub("^--cores=", "", a))
    if (grepl("^--louvain-min=[0-9]+$", a)) louvain_min_size <- as.integer(sub("^--louvain-min=", "", a))
    if (a == "--dedup-after")               dedup_after      <- TRUE
  }

  run_clustering(name, save_path, num_cores, louvain_min_size, dedup_after)
}
