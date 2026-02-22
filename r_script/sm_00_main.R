#!/usr/bin/env Rscript
# =============================================================================
# sm_00_main.R  [suffix mode]
# IBMseq パイプライン メインスクリプト（suffix mode 版）
#
# 00_main.R との差分:
#   - 02_clustering: sm_02_clustering.R を使用（node_type 列を membership に追加）
#   - 04_features:   sm_04_features.R を使用（run_umi_uei が group-by ベースに高速化）
#   - 01_load_graph.R / 03_density.R / 05_plot.R は共通
#
# 使い方・オプションは 00_main.R と完全に同一:
#   Rscript sm_00_main.R <name> <read_path> <save_path> [オプション ...]
#
# オプション:
#   --no-dup         : 重複エッジを除去してからクラスタリング（デフォルト）
#   --with-dup       : 重複エッジを保持してクラスタリング
#   <整数>           : min_cluster_size（デフォルト: 1000）
#   --cores=N        : 並列計算コア数（デフォルト: 1）
#   --metrics=A,B,...: 計算する指標（edge_density, umi_uei, ego_size, diameter）
#   --from-tsv       : 作図ステップで TSV から読み込む
#
# 使用例:
#   Rscript sm_00_main.R V5P2_24aB_CTCF_2_3000 /data /output --no-dup 1000 --cores=9
#   Rscript sm_00_main.R V5P2_24aB_CTCF_2_3000 /data /output --metrics=umi_uei
# =============================================================================

# ---- 引数解析 ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript sm_00_main.R <name> <read_path> <save_path> [options]\nSee script header for full option list.")
}

name      <- args[1]
read_path <- args[2]
save_path <- args[3]

dup_mode         <- "--no-dup"
min_cluster_size <- 1000L
num_cores        <- 1L
metrics          <- c("edge_density", "umi_uei", "ego_size", "diameter")
from_tsv         <- FALSE

for (a in args[-(1:3)]) {
  if (a %in% c("--no-dup", "--with-dup")) {
    dup_mode <- a
  } else if (grepl("^--cores=[0-9]+$", a)) {
    num_cores <- as.integer(sub("^--cores=", "", a))
  } else if (grepl("^--metrics=", a)) {
    metrics <- strsplit(sub("^--metrics=", "", a), ",")[[1]]
  } else if (a == "--from-tsv") {
    from_tsv <- TRUE
  } else if (grepl("^[0-9]+$", a)) {
    min_cluster_size <- as.integer(a)
  }
}

# ---- 出力ディレクトリ作成 ----
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

# ---- ログ初期化 ----
log_file <- file.path(save_path, paste0(name, "_process.log"))
writeLines("", log_file)

cat(paste0("=== IBMseq Pipeline START [suffix mode]: ", Sys.time(), " ===\n"))
cat(paste0("  name             = ", name,                         "\n"))
cat(paste0("  read_path        = ", read_path,                    "\n"))
cat(paste0("  save_path        = ", save_path,                    "\n"))
cat(paste0("  dup_mode         = ", dup_mode,                     "\n"))
cat(paste0("  min_cluster_size = ", min_cluster_size,             "\n"))
cat(paste0("  num_cores        = ", num_cores,                    "\n"))
cat(paste0("  metrics          = ", paste(metrics, collapse = ","), "\n"))
cat(paste0("  from_tsv         = ", from_tsv,                     "\n\n"))

# ---- ライブラリ読み込み ----
suppressPackageStartupMessages({
  library(data.table)
  library(igraph)
  library(ggplot2)
  library(parallel)
})

# ---- サブスクリプトの source ----
IBMSEQ_SOURCED <- TRUE

script_args <- commandArgs(trailingOnly = FALSE)
script_path  <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir   <- if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()

source(file.path(script_dir, "01_load_graph.R"))
source(file.path(script_dir, "sm_02_clustering.R"))  # suffix mode 版
source(file.path(script_dir, "03_density.R"))
source(file.path(script_dir, "sm_04_features.R"))    # suffix mode 版
source(file.path(script_dir, "05_plot.R"))

# ============================================================
# Step 1: データ読み込み・グラフ構築
# ============================================================
cat("\n--- Step 1: Load Graph ---\n")
load_graph(name, read_path, save_path, dup_mode)

# ============================================================
# Step 2: Louvainクラスタリング + node_type エンコード [suffix mode]
# ============================================================
cat("\n--- Step 2: Clustering (suffix mode) ---\n")
run_clustering(name, save_path, num_cores = num_cores)

# ============================================================
# Step 3: 密度計算
# ============================================================
cat("\n--- Step 3: Density ---\n")
run_density(name, save_path, min_cluster_size, num_cores,
            compute_edge_density = "edge_density" %in% metrics)

# ============================================================
# Step 4: その他特徴量計算 [suffix mode: run_umi_uei が高速化]
# ============================================================
cat("\n--- Step 4: Features (suffix mode) ---\n")
run_features(name, save_path, min_cluster_size, num_cores,
             metrics = intersect(metrics, c("umi_uei", "ego_size", "diameter")))

# ============================================================
# Step 5: 作図
# ============================================================
cat("\n--- Step 5: Plot ---\n")
run_plots(name, save_path, from_tsv = from_tsv)

cat(paste0("\n=== IBMseq Pipeline DONE [suffix mode]: ", Sys.time(), " ===\n"))
cat(paste0("Output: ", save_path, "\n"))
