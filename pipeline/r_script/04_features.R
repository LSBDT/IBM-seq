# =============================================================================
# 04_features.R
# その他特徴量計算: UMI/UEI数、Ego Size、Diameter
#
# 各指標は個別関数として独立しており、単独でも実行可能。
# run_features() はラッパーで --metrics で選択した指標のみ計算する。
#
# Input (各関数共通):
#   {save_path}/{name}_02_graph.rds
#   {save_path}/{name}_02_membership.rds
#   {save_path}/{name}_03_cluster_size.rds
#
# Output:
#   {name}_04_umi_uei.rds/.tsv     (run_umi_uei)
#   {name}_04_ego_size.rds/.tsv    (run_ego_size)
#   {name}_04_diameter.rds/.tsv    (run_diameter)
#   {name}_04_features_summary.txt (run_features のみ)
#
# 使い方（スタンドアロン）:
#   Rscript 04_features.R <name> <save_path> [min_cluster_size] [num_cores] [--metrics=A,B,...] [--no-rds]
#   有効な指標名: umi_uei, ego_size, diameter
#   デフォルト: 全指標, min_cluster_size=1000, num_cores=1, RDS を保存する
#
# 個別指標だけ実行する例:
#   Rscript 04_features.R V5P2_24aB_CTCF_2_3000 ./output 1000 4 --metrics=ego_size
#   Rscript 04_features.R V5P2_24aB_CTCF_2_3000 ./output 1000 4 --metrics=ego_size,diameter
# =============================================================================

# ---- 内部ヘルパー: 大クラスターのノードリストを返す ----
# _03_cluster_size.rds が存在しない場合（--no-rds 実行後）は TSV から読み込む
.load_large_cluster_list <- function(name, save_path, min_cluster_size) {
  df_membership <- readRDS(file.path(save_path, paste0(name, "_02_membership.rds")))
  rds_cs <- file.path(save_path, paste0(name, "_03_cluster_size.rds"))
  tsv_cs <- file.path(save_path, paste0(name, "_03_cluster_size.tsv"))
  if (file.exists(rds_cs)) {
    cluster_size <- readRDS(rds_cs)
  } else if (file.exists(tsv_cs)) {
    cluster_size <- fread(tsv_cs)
  } else {
    stop(paste("cluster_size file not found (checked .rds and .tsv):", rds_cs))
  }
  large_ids <- as.data.table(cluster_size)[total > min_cluster_size, community_id]
  if (length(large_ids) == 0) return(NULL)
  mem_dt <- as.data.table(df_membership)
  split(
    mem_dt[community_id %in% large_ids, name],
    mem_dt[community_id %in% large_ids, community_id]
  )
}

# =============================================================================
# run_umi_uei(): UMI/UEI 数カウント
# =============================================================================
run_umi_uei <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L,
                        save_rds = TRUE) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[04_umi_uei] START: ", Sys.time()))
  write_log(paste0("  min_cluster_size=", min_cluster_size))

  cluster_list <- .load_large_cluster_list(name, save_path, min_cluster_size)
  if (is.null(cluster_list)) {
    write_log(paste0("  WARNING: No clusters with > ", min_cluster_size, " nodes. Skipped."))
    empty <- data.table(community_id=integer(), variation=integer(), type=character())
    if (save_rds) saveRDS(empty, file.path(save_path, paste0(name, "_04_umi_uei.rds")))
    fwrite(empty,  file.path(save_path, paste0(name, "_04_umi_uei.tsv")), sep="\t")
    write_log(paste0("[04_umi_uei] DONE (skipped): ", Sys.time()))
    return(invisible(empty))
  }

  write_log(paste0("  Calculating UMI/UEI counts for ", length(cluster_list), " cluster(s)..."))
  # NOTE: suffixパターンはfastqの処理コードに合わせること
  #       現在: UMI1=.m1, UMI2=.t2, UEI1=.e1, UEI2=.e2
  #       変更が必要な場合は suffixes ベクトルを修正する
  suffixes <- c("UMI1" = "\\.m1$",
                "UMI2" = "\\.t2$",
                "UEI1" = "\\.e1$",
                "UEI2" = "\\.e2$")

  umi_uei_list <- lapply(names(suffixes), function(suffix_name) {
    pattern <- suffixes[[suffix_name]]
    res <- lapply(names(cluster_list), function(cl) {
      cnt <- sum(grepl(pattern, cluster_list[[cl]]))
      if (cnt > 0) {
        data.table(community_id = as.integer(cl),
                   variation    = cnt,
                   type         = paste(name, suffix_name, sep = "_"))
      } else NULL
    })
    rbindlist(res[!sapply(res, is.null)])
  })
  umi_uei_df <- rbindlist(umi_uei_list[lengths(umi_uei_list) > 0])
  if (nrow(umi_uei_df) == 0) {
    umi_uei_df <- data.table(community_id=integer(), variation=integer(), type=character())
  }

  if (save_rds) saveRDS(umi_uei_df, file.path(save_path, paste0(name, "_04_umi_uei.rds")))
  fwrite(umi_uei_df,  file.path(save_path, paste0(name, "_04_umi_uei.tsv")), sep="\t")
  write_log(capture.output(head(umi_uei_df)))
  write_log(paste0("  Saved: ", name, "_04_umi_uei",
                   if (save_rds) ".rds/.tsv" else ".tsv (--no-rds: RDS skipped)"))
  write_log(paste0("[04_umi_uei] DONE: ", Sys.time()))

  invisible(umi_uei_df)
}

# =============================================================================
# run_ego_size(): Ego Size 計算（order=3）
# =============================================================================
run_ego_size <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L,
                         save_rds = TRUE) {

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
    empty <- data.table(value=numeric(), community_id=character())
    if (save_rds) saveRDS(empty, file.path(save_path, paste0(name, "_04_ego_size.rds")))
    fwrite(empty,  file.path(save_path, paste0(name, "_04_ego_size.tsv")), sep="\t")
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

  if (save_rds) saveRDS(ego_size_df, file.path(save_path, paste0(name, "_04_ego_size.rds")))
  fwrite(ego_size_df,  file.path(save_path, paste0(name, "_04_ego_size.tsv")), sep="\t")
  write_log(capture.output(head(ego_size_df)))
  write_log(paste0("  Saved: ", name, "_04_ego_size",
                   if (save_rds) ".rds/.tsv" else ".tsv (--no-rds: RDS skipped)"))
  write_log(paste0("[04_ego_size] DONE: ", Sys.time()))

  invisible(ego_size_df)
}

# =============================================================================
# run_diameter(): Diameter 計算
# =============================================================================
run_diameter <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L,
                         save_rds = TRUE) {

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
    empty <- data.table(diameter=numeric(), community_id=integer(), library=character())
    if (save_rds) saveRDS(empty, file.path(save_path, paste0(name, "_04_diameter.rds")))
    fwrite(empty,  file.path(save_path, paste0(name, "_04_diameter.tsv")), sep="\t")
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

  if (save_rds) saveRDS(diameter_df, file.path(save_path, paste0(name, "_04_diameter.rds")))
  fwrite(diameter_df,  file.path(save_path, paste0(name, "_04_diameter.tsv")), sep="\t")
  write_log(capture.output(head(diameter_df)))
  write_log(paste0("  Saved: ", name, "_04_diameter",
                   if (save_rds) ".rds/.tsv" else ".tsv (--no-rds: RDS skipped)"))
  write_log(paste0("[04_diameter] DONE: ", Sys.time()))

  invisible(diameter_df)
}

# =============================================================================
# run_features(): ラッパー（metrics で指標を選択）
# =============================================================================
run_features <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L,
                          metrics = c("umi_uei", "ego_size", "diameter"),
                          save_rds = TRUE) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[04_features] START: ", Sys.time(),
                   "  metrics=", paste(metrics, collapse=",")))

  if ("umi_uei"  %in% metrics) run_umi_uei( name, save_path, min_cluster_size, num_cores, save_rds)
  if ("ego_size" %in% metrics) run_ego_size(name, save_path, min_cluster_size, num_cores, save_rds)
  if ("diameter" %in% metrics) run_diameter(name, save_path, min_cluster_size, num_cores, save_rds)

  # サマリーテキスト（RDS がなければ TSV から読む）
  summary_out <- file.path(save_path, paste0(name, "_04_features_summary.txt"))
  sink(summary_out)
  cat("=== Features Summary ===\n")
  cat("metrics computed:", paste(metrics, collapse=", "), "\n")
  if (!save_rds) cat("(--no-rds: RDS files not saved; TSV only)\n")
  for (m in metrics) {
    rds <- file.path(save_path, paste0(name, "_04_", m, ".rds"))
    tsv <- file.path(save_path, paste0(name, "_04_", m, ".tsv"))
    if (file.exists(rds)) {
      cat(paste0("\n[", m, "]:\n")); print(head(readRDS(rds)))
    } else if (file.exists(tsv)) {
      cat(paste0("\n[", m, "] (from TSV):\n")); print(head(fread(tsv)))
    }
  }
  sink()
  write_log(paste0("  Saved: ", summary_out))
  write_log(paste0("[04_features] DONE: ", Sys.time()))
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
      "Usage: Rscript 04_features.R <name> <save_path>",
      "[min_cluster_size] [num_cores] [--metrics=A,B,...]",
      "\n  valid metrics: umi_uei, ego_size, diameter"
    ))
  }
  name      <- args[1]
  save_path <- args[2]

  min_cluster_size <- 1000L
  num_cores        <- 1L
  metrics          <- c("umi_uei", "ego_size", "diameter")
  save_rds         <- TRUE

  for (a in args[-(1:2)]) {
    if (grepl("^--metrics=", a)) {
      metrics <- strsplit(sub("^--metrics=", "", a), ",")[[1]]
    } else if (grepl("^--cores=[0-9]+$", a)) {
      num_cores <- as.integer(sub("^--cores=", "", a))
    } else if (a == "--no-rds") {
      save_rds <- FALSE
    } else if (grepl("^[0-9]+$", a) && min_cluster_size == 1000L) {
      min_cluster_size <- as.integer(a)
    } else if (grepl("^[0-9]+$", a)) {
      num_cores <- as.integer(a)
    }
  }

  run_features(name, save_path, min_cluster_size, num_cores, metrics, save_rds)
}
