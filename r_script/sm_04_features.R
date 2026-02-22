# =============================================================================
# sm_04_features.R  [suffix mode]
# その他特徴量計算: UMI/UEI数、Ego Size、Diameter
#
# 04_features.R との差分:
#   - run_umi_uei() のみ変更
#     旧: クラスターごと × サフィックスごとに grepl() ループ
#     新: _02_membership.rds の node_type 列を使い data.table group-by 一発でカウント
#   - run_ego_size(), run_diameter(), run_features() は 04_features.R と同一
#
# 前提:
#   sm_02_clustering.R で生成した _02_membership.rds（node_type 列あり）が必要
#
# NODE_TYPE_LABELS は sm_02_clustering.R の NODE_TYPE_MAP と対応させること:
#   1 = UMI1 (.m1)   2 = UMI2 (.t2)   3 = UEI1 (.e1)   4 = UEI2 (.e2)
#
# Input:
#   {save_path}/{name}_02_membership.rds  ← node_type 列を含む（sm_02_clustering 出力）
#   {save_path}/{name}_02_graph.rds
#   {save_path}/{name}_03_cluster_size.rds
#
# Output: 04_features.R と同一フォーマット
#   {name}_04_umi_uei.rds/.tsv
#   {name}_04_ego_size.rds/.tsv
#   {name}_04_diameter.rds/.tsv
#   {name}_04_features_summary.txt
#
# 使い方（スタンドアロン）:
#   Rscript sm_04_features.R <name> <save_path> [min_cluster_size] [--cores=N] [--metrics=A,B,...]
#   有効な指標名: umi_uei, ego_size, diameter
# =============================================================================

# node_type 整数コード → ラベル対応表
# sm_02_clustering.R の NODE_TYPE_MAP と一致させること
NODE_TYPE_LABELS <- c("1" = "UMI1", "2" = "UMI2", "3" = "UEI1", "4" = "UEI2")

# ---- 内部ヘルパー: 大クラスターのノードリストを返す（04_features.R と同一）----
.load_large_cluster_list <- function(name, save_path, min_cluster_size) {
  df_membership <- readRDS(file.path(save_path, paste0(name, "_02_membership.rds")))
  cluster_size  <- readRDS(file.path(save_path, paste0(name, "_03_cluster_size.rds")))
  large_ids     <- as.data.table(cluster_size)[total > min_cluster_size, community_id]
  if (length(large_ids) == 0) return(NULL)
  mem_dt <- as.data.table(df_membership)
  split(
    mem_dt[community_id %in% large_ids, name],
    mem_dt[community_id %in% large_ids, community_id]
  )
}

# =============================================================================
# run_umi_uei(): UMI/UEI 数カウント  [suffix mode: group-by 版]
# =============================================================================
run_umi_uei <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[sm_04_umi_uei] START: ", Sys.time(), "  mode=suffix"))
  write_log(paste0("  min_cluster_size=", min_cluster_size))

  # ---- membership 読み込み（node_type 列が必要）----
  mem_dt <- as.data.table(
    readRDS(file.path(save_path, paste0(name, "_02_membership.rds")))
  )
  if (!"node_type" %in% names(mem_dt)) {
    stop(paste(
      "node_type column not found in _02_membership.rds.",
      "Run sm_02_clustering.R (suffix mode) to generate it."
    ))
  }

  cluster_size <- readRDS(file.path(save_path, paste0(name, "_03_cluster_size.rds")))
  large_ids    <- as.data.table(cluster_size)[total > min_cluster_size, community_id]

  if (length(large_ids) == 0) {
    write_log(paste0("  WARNING: No clusters with > ", min_cluster_size, " nodes. Skipped."))
    empty <- data.table(community_id = integer(), variation = integer(), type = character())
    saveRDS(empty, file.path(save_path, paste0(name, "_04_umi_uei.rds")))
    fwrite(empty,  file.path(save_path, paste0(name, "_04_umi_uei.tsv")), sep = "\t")
    write_log(paste0("[sm_04_umi_uei] DONE (skipped): ", Sys.time()))
    return(invisible(empty))
  }

  write_log(paste0("  Counting UMI/UEI via group-by (", length(large_ids), " large clusters)..."))

  # ---- 1回の group-by で全タイプをカウント ----
  umi_uei_df <- mem_dt[
    community_id %in% large_ids & !is.na(node_type),
    .(variation = .N),
    by = .(community_id, node_type)
  ]

  if (nrow(umi_uei_df) == 0) {
    umi_uei_df <- data.table(community_id = integer(), variation = integer(), type = character())
  } else {
    # 整数コードをラベルに変換し 04_features.R と同一の出力フォーマットに合わせる
    umi_uei_df[, type := paste(name, NODE_TYPE_LABELS[as.character(node_type)], sep = "_")]
    umi_uei_df[, node_type := NULL]
    setcolorder(umi_uei_df, c("community_id", "variation", "type"))
  }

  saveRDS(umi_uei_df, file.path(save_path, paste0(name, "_04_umi_uei.rds")))
  fwrite(umi_uei_df,  file.path(save_path, paste0(name, "_04_umi_uei.tsv")), sep = "\t")
  write_log(capture.output(head(umi_uei_df)))
  write_log(paste0("  Saved: ", name, "_04_umi_uei.rds/.tsv"))
  write_log(paste0("[sm_04_umi_uei] DONE: ", Sys.time()))

  invisible(umi_uei_df)
}

# =============================================================================
# run_ego_size(): Ego Size 計算（04_features.R と同一）
# =============================================================================
run_ego_size <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[04_ego_size] START: ", Sys.time()))
  write_log(paste0("  min_cluster_size=", min_cluster_size, "  num_cores=", num_cores))

  cluster_list <- .load_large_cluster_list(name, save_path, min_cluster_size)
  if (is.null(cluster_list)) {
    write_log(paste0("  WARNING: No clusters with > ", min_cluster_size, " nodes. Skipped."))
    empty <- data.table(value = numeric(), community_id = character())
    saveRDS(empty, file.path(save_path, paste0(name, "_04_ego_size.rds")))
    fwrite(empty,  file.path(save_path, paste0(name, "_04_ego_size.tsv")), sep = "\t")
    write_log(paste0("[04_ego_size] DONE (skipped): ", Sys.time()))
    return(invisible(empty))
  }

  graph <- readRDS(file.path(save_path, paste0(name, "_02_graph.rds")))
  write_log(paste0("  Calculating ego sizes (order=3) for ", length(cluster_list), " cluster(s)..."))

  ego_list <- parallel::mclapply(names(cluster_list), function(cl) {
    cluster <- cluster_list[[cl]]
    if (length(cluster) == 0) return(NULL)
    subg <- induced_subgraph(graph, cluster)
    data.table(value = ego_size(subg, order = 3L), community_id = cl)
  }, mc.cores = num_cores)
  ego_size_df <- rbindlist(ego_list[!sapply(ego_list, is.null)])
  rm(ego_list); gc(verbose = FALSE)

  saveRDS(ego_size_df, file.path(save_path, paste0(name, "_04_ego_size.rds")))
  fwrite(ego_size_df,  file.path(save_path, paste0(name, "_04_ego_size.tsv")), sep = "\t")
  write_log(capture.output(head(ego_size_df)))
  write_log(paste0("  Saved: ", name, "_04_ego_size.rds/.tsv"))
  write_log(paste0("[04_ego_size] DONE: ", Sys.time()))

  invisible(ego_size_df)
}

# =============================================================================
# run_diameter(): Diameter 計算（04_features.R と同一）
# =============================================================================
run_diameter <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[04_diameter] START: ", Sys.time()))
  write_log(paste0("  min_cluster_size=", min_cluster_size, "  num_cores=", num_cores))

  cluster_list <- .load_large_cluster_list(name, save_path, min_cluster_size)
  if (is.null(cluster_list)) {
    write_log(paste0("  WARNING: No clusters with > ", min_cluster_size, " nodes. Skipped."))
    empty <- data.table(diameter = numeric(), community_id = integer(), library = character())
    saveRDS(empty, file.path(save_path, paste0(name, "_04_diameter.rds")))
    fwrite(empty,  file.path(save_path, paste0(name, "_04_diameter.tsv")), sep = "\t")
    write_log(paste0("[04_diameter] DONE (skipped): ", Sys.time()))
    return(invisible(empty))
  }

  graph <- readRDS(file.path(save_path, paste0(name, "_02_graph.rds")))
  write_log(paste0("  Calculating diameters for ", length(cluster_list), " cluster(s)..."))

  diam_list <- parallel::mclapply(names(cluster_list), function(cl) {
    cluster <- cluster_list[[cl]]
    data.table(
      diameter     = diameter(induced_subgraph(graph, cluster), weights = NA),
      community_id = as.integer(cl),
      library      = name
    )
  }, mc.cores = num_cores)
  diameter_df <- rbindlist(diam_list)

  saveRDS(diameter_df, file.path(save_path, paste0(name, "_04_diameter.rds")))
  fwrite(diameter_df,  file.path(save_path, paste0(name, "_04_diameter.tsv")), sep = "\t")
  write_log(capture.output(head(diameter_df)))
  write_log(paste0("  Saved: ", name, "_04_diameter.rds/.tsv"))
  write_log(paste0("[04_diameter] DONE: ", Sys.time()))

  invisible(diameter_df)
}

# =============================================================================
# run_features(): ラッパー（04_features.R と同一インターフェース）
# =============================================================================
run_features <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L,
                          metrics = c("umi_uei", "ego_size", "diameter")) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[sm_04_features] START: ", Sys.time(),
                   "  metrics=", paste(metrics, collapse = ",")))

  if ("umi_uei"  %in% metrics) run_umi_uei( name, save_path, min_cluster_size, num_cores)
  if ("ego_size" %in% metrics) run_ego_size(name, save_path, min_cluster_size, num_cores)
  if ("diameter" %in% metrics) run_diameter(name, save_path, min_cluster_size, num_cores)

  summary_out <- file.path(save_path, paste0(name, "_04_features_summary.txt"))
  sink(summary_out)
  cat("=== Features Summary [suffix mode] ===\n")
  cat("metrics computed:", paste(metrics, collapse = ", "), "\n")
  for (m in metrics) {
    rds <- file.path(save_path, paste0(name, "_04_", m, ".rds"))
    if (file.exists(rds)) {
      cat(paste0("\n[", m, "]:\n"))
      print(head(readRDS(rds)))
    }
  }
  sink()
  write_log(paste0("  Saved: ", summary_out))
  write_log(paste0("[sm_04_features] DONE: ", Sys.time()))
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
  if (length(args) < 2) {
    stop(paste(
      "Usage: Rscript sm_04_features.R <name> <save_path>",
      "[min_cluster_size] [--cores=N] [--metrics=A,B,...]",
      "\n  valid metrics: umi_uei, ego_size, diameter"
    ))
  }
  name      <- args[1]
  save_path <- args[2]

  min_cluster_size <- 1000L
  num_cores        <- 1L
  metrics          <- c("umi_uei", "ego_size", "diameter")

  for (a in args[-(1:2)]) {
    if (grepl("^--metrics=", a)) {
      metrics <- strsplit(sub("^--metrics=", "", a), ",")[[1]]
    } else if (grepl("^--cores=[0-9]+$", a)) {
      num_cores <- as.integer(sub("^--cores=", "", a))
    } else if (grepl("^[0-9]+$", a) && min_cluster_size == 1000L) {
      min_cluster_size <- as.integer(a)
    } else if (grepl("^[0-9]+$", a)) {
      num_cores <- as.integer(a)
    }
  }

  run_features(name, save_path, min_cluster_size, num_cores, metrics)
}
