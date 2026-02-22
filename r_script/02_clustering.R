# =============================================================================
# 02_clustering.R
# ルーベインクラスタリング（連結成分ごと）
#
# ★ ike.R の重要修正点を取り込み:
#   1. components() で連結成分を分割してから cluster_louvain() を適用
#   2. community_id をオフセットで一意管理（連結成分間で重複しない）
#   3. subgraph_id（連結成分）と community_id（Louvain結果）を分離して保持
#   4. ステップごとに RDS 保存
#
# Input : {save_path}/{name}_01_graph.rds
#
# Output: {save_path}/{name}_02_graph.rds          ← community_id属性付きgraph
#         {save_path}/{name}_02_membership.rds      ← 頂点属性テーブル
#         {save_path}/{name}_02_community.rds       ← louvain_results リスト
#         {save_path}/{name}_02_membership.tsv      ← テキスト出力（検証用）
#         {save_path}/{name}_02_summary.txt         ← クラスタリング統計
#
# 使い方（スタンドアロン）:
#   Rscript 02_clustering.R <name> <save_path>
# =============================================================================

run_clustering <- function(name, save_path, num_cores = 1L, louvain_min_size = 3L) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[02_clustering] START: ", Sys.time(),
                   "  num_cores=", num_cores,
                   "  louvain_min_size=", louvain_min_size))

  # ---- Step1 出力を読み込み ----
  graph_file <- file.path(save_path, paste0(name, "_01_graph.rds"))
  if (!file.exists(graph_file)) stop(paste("RDS not found:", graph_file))
  graph <- readRDS(graph_file)
  write_log(paste0("  Loaded graph: V=", vcount(graph), "  E=", ecount(graph)))

  # ---- 頂点属性テーブル構築（from→Target1, to→Target2 の出現数を集計）----
  # 各ノードがどのTarget種別に何回登場したかをカウントし、横持ちに変換する
  write_log("  Building vertex attribute table (Target type counts)...")
  df      <- igraph::as_data_frame(graph, "both")
  # from→Target1、to→Target2 の対応で (node, target_type) ペアを作る
  # 列名で参照してインデックスズレを防ぐ
  df_from <- data.frame(to = df$edges[["from"]], Target2 = df$edges[["Target1"]],
                        stringsAsFactors = FALSE)
  df_to   <- df$edges[, c("to", "Target2"), drop = FALSE]
  df_list <- rbind(df_from, df_to)

  # data.table の dcast を使い pivot_wider より高速・省メモリに変換
  test_dt   <- setDT(df_list)[, .(Count = .N), by = .(to, Target2)]
  test_wide <- dcast(test_dt, to ~ Target2, value.var = "Count", fill = 0L)
  df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                       by.x = "name", by.y = "to", all.x = TRUE)
  # 孤立頂点など結合できなかった列を 0 埋め
  for (col in setdiff(names(df$vertices), "name")) {
    df$vertices[[col]][is.na(df$vertices[[col]])] <- 0L
  }
  rm(df_from, df_to, df_list, test_dt, test_wide)
  gc(verbose = FALSE)

  # ---- 連結成分の取得 ----
  write_log("  Computing connected components...")
  comp     <- components(graph)
  clusters <- comp$membership     # 頂点ごとの連結成分ID
  n_comp   <- comp$no
  V(graph)$subgraph_id <- clusters
  write_log(paste0("  Connected components: ", n_comp))

  # ---- コンポーネントサイズ分類 ----
  # trivial（< louvain_min_size）: Louvain スキップ、連結成分IDをそのまま community_id に
  # large（>= louvain_min_size）: Louvain を並列実行
  comp_dt      <- data.table(node_idx = seq_len(vcount(graph)),
                              comp_id  = as.integer(clusters))
  comp_size_dt <- comp_dt[, .(size = .N), by = comp_id]
  large_ids    <- comp_size_dt[size >= louvain_min_size, comp_id]
  n_large      <- length(large_ids)
  n_trivial    <- n_comp - n_large
  write_log(paste0("  trivial(<", louvain_min_size, "): ", n_trivial,
                   "  Louvain(>=", louvain_min_size, "): ", n_large,
                   "  (num_cores=", num_cores, ")"))

  # trivial コンポーネントは連結成分ID(1..n_comp)をそのまま community_id に利用
  community_id <- as.integer(clusters)
  offset       <- as.integer(n_comp)  # large 用 ID はここから開始

  # ---- 大コンポーネントのみ Louvain（mclapply で並列）----
  if (n_large > 0) {
    write_log(paste0("  Running Louvain on ", n_large, " large components..."))
    # ノードリストを大コンポーネント分だけ生成（全コンポーネントの split は不要）
    large_comp_nodes <- comp_dt[comp_id %in% large_ids,
                                 .(nodes = list(node_idx)), by = comp_id][order(comp_id)]

    lv_raw <- parallel::mclapply(large_comp_nodes$nodes, function(nodes) {
      subg <- induced_subgraph(graph, nodes)
      lv   <- cluster_louvain(subg)
      rm(subg)
      as.integer(membership(lv))
    }, mc.cores = num_cores)

    # オフセットを逐次付与（ここは安価なので sequential で問題なし）
    for (i in seq_along(large_comp_nodes$nodes)) {
      nodes               <- large_comp_nodes$nodes[[i]]
      memb                <- lv_raw[[i]] + offset
      community_id[nodes] <- memb
      offset              <- offset + max(lv_raw[[i]])
      lv_raw[[i]]         <- integer(0)  # スロットを残しつつデータ解放（NULL だとリストが縮んでインデックスがずれる）
    }
    rm(lv_raw, large_comp_nodes)
    gc(verbose = FALSE)
    write_log(paste0("  Louvain done. Total communities: ", offset))
  }

  V(graph)$community_id <- community_id
  gc(verbose = FALSE)

  # ---- membership データフレーム構築 ----
  df_membership <- cbind(
    df$vertices,
    subgraph_id  = clusters,      # 連結成分ID
    community_id = community_id   # Louvainコミュニティ（一意）
    # NOTE: sum列 (rowSums of Target columns) は抗体数により列数が変わるため省略
    #       必要な場合は: df_membership$sum <- rowSums(df_membership[, 2:(ncol-2)], na.rm=TRUE)
  )

  n_community <- max(community_id, na.rm = TRUE)
  write_log(paste0("  Total communities: ", n_community))
  write_log("  Membership head:")
  write_log(capture.output(head(df_membership)))

  # ---- RDS 保存 ----
  saveRDS(graph,         file.path(save_path, paste0(name, "_02_graph.rds")))
  saveRDS(df_membership, file.path(save_path, paste0(name, "_02_membership.rds")))
  # _02_community.rds: Louvain オブジェクトはメモリ節約のため保持しない
  saveRDS(list(n_large = n_large, n_trivial = n_trivial, louvain_min_size = louvain_min_size),
          file.path(save_path, paste0(name, "_02_community.rds")))
  write_log(paste0("  Saved: ", name, "_02_graph.rds / _02_membership.rds / _02_community.rds"))

  # ---- テキスト出力（検証用）----
  tsv_out <- file.path(save_path, paste0(name, "_02_membership.tsv"))
  fwrite(as.data.table(df_membership), tsv_out, sep = "\t", quote = FALSE)
  write_log(paste0("  Saved: ", tsv_out))

  # ---- サマリーテキスト ----
  summary_out <- file.path(save_path, paste0(name, "_02_summary.txt"))
  comm_table  <- sort(table(community_id), decreasing = TRUE)
  sink(summary_out)
  cat("=== Clustering Summary ===\n")
  cat("Connected components :", n_comp,      "\n")
  cat("Total communities    :", n_community, "\n")
  cat("\nCommunity size distribution (top 20):\n")
  print(head(comm_table, 20))
  cat("\nMembership head:\n")
  print(head(df_membership))
  sink()
  write_log(paste0("  Saved: ", summary_out))
  write_log(paste0("[02_clustering] DONE: ", Sys.time()))

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
  if (length(args) < 2) stop("Usage: Rscript 02_clustering.R <name> <save_path> [--cores=N] [--louvain-min=N]")
  name             <- args[1]
  save_path        <- args[2]
  num_cores        <- 1L
  louvain_min_size <- 3L

  for (a in args[-(1:2)]) {
    if (grepl("^--cores=[0-9]+$", a))       num_cores        <- as.integer(sub("^--cores=", "", a))
    if (grepl("^--louvain-min=[0-9]+$", a)) louvain_min_size <- as.integer(sub("^--louvain-min=", "", a))
  }

  run_clustering(name, save_path, num_cores, louvain_min_size)
}
