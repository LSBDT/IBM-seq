#!/usr/bin/env Rscript
# =============================================================================
# 00_main.R
# IBMseq パイプライン メインスクリプト
#
# 使い方:
#   Rscript 00_main.R <name> <read_path> <save_path> [オプション ...]
#
# 必須引数:
#   name             : サンプル名（例: V5P2_24aB_CTCF_2_3000）
#   read_path        : .link.gz が置かれているディレクトリ
#   save_path        : 出力ディレクトリ（なければ自動作成）
#
# オプション（順不同で指定可）:
#   --no-dup         : 重複エッジを除去してからクラスタリング（デフォルト）
#   --with-dup       : 重複エッジを保持してクラスタリング
#   <整数>           : min_cluster_size（デフォルト: 1000、テスト時は 3 など）
#   --cores=N        : 並列計算コア数（デフォルト: 1）
#                      72コア/512GBマシンなら --cores=9 程度が目安
#   --metrics=A,B,...: 計算する指標をカンマ区切りで指定（デフォルト: 全指標）
#                      有効値: edge_density, umi_uei, ego_size, diameter
#                      例: --metrics=ego_size,diameter
#   --from-tsv       : 作図ステップで RDS の代わりに TSV から読み込む
#                      計算を再実行せずに図だけ作り直す場合に使用
#   --no-rds         : Step 3/4 の指標 RDS ファイルを保存しない（TSV のみ出力）
#                      RDS の I/O を省略してパイプラインを高速化したい場合に使用
#                      ※ Step 1/2 の構造 RDS（graph.rds, membership.rds）は常に保存
#                      ※ --no-rds 指定時は自動的に --from-tsv が有効になる
#
# 使用例:
#   # 全指標を計算（通常実行）
#   Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --no-dup 1000 --cores=9
#
#   # ego_size と diameter だけ計算
#   Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --metrics=ego_size,diameter
#
#   # TSV から図だけ再作成
#   Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --from-tsv
#
#   # テスト（小データ、全指標、2コア）
#   Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --no-dup 3 --cores=2
#
#   # バックグラウンド実行
#   nohup Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output \
#     --no-dup 1000 --cores=9 > /output/run.log 2>&1 &
#
# ステップ構成:
#   01_load_graph.R  : データ読み込み・グラフ構築
#   02_clustering.R  : Louvainクラスタリング（連結成分ごと）
#   03_density.R     : クラスター規模・Edge Density計算
#   04_features.R    : UMI/UEI数・Ego Size・Diameter計算
#   05_plot.R        : 作図（PDF出力）
# =============================================================================

# ---- 引数解析 ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript 00_main.R <name> <read_path> <save_path> [options]\nSee script header for full option list.")
}

name      <- args[1]
read_path <- args[2]
save_path <- args[3]

# オプション引数を順不同で解析
dup_mode         <- "--no-dup"
min_cluster_size <- 1000L
num_cores        <- 1L
metrics          <- c("edge_density", "umi_uei", "ego_size", "diameter")
from_tsv         <- FALSE
no_rds           <- FALSE

for (a in args[-(1:3)]) {
  if (a %in% c("--no-dup", "--with-dup")) {
    dup_mode <- a
  } else if (grepl("^--cores=[0-9]+$", a)) {
    num_cores <- as.integer(sub("^--cores=", "", a))
  } else if (grepl("^--metrics=", a)) {
    metrics <- strsplit(sub("^--metrics=", "", a), ",")[[1]]
  } else if (a == "--from-tsv") {
    from_tsv <- TRUE
  } else if (a == "--no-rds") {
    no_rds   <- TRUE
    from_tsv <- TRUE  # --no-rds では作図も TSV から読む
  } else if (grepl("^[0-9]+$", a)) {
    min_cluster_size <- as.integer(a)
  }
}

# ---- 出力ディレクトリ作成 ----
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

# ---- ログ初期化 ----
log_file <- file.path(save_path, paste0(name, "_process.log"))
writeLines("", log_file)  # 上書き初期化

cat(paste0("=== IBMseq Pipeline START: ", Sys.time(), " ===\n"))
cat(paste0("  name             = ", name,                        "\n"))
cat(paste0("  read_path        = ", read_path,                   "\n"))
cat(paste0("  save_path        = ", save_path,                   "\n"))
cat(paste0("  dup_mode         = ", dup_mode,                    "\n"))
cat(paste0("  min_cluster_size = ", min_cluster_size,            "\n"))
cat(paste0("  num_cores        = ", num_cores,                   "\n"))
cat(paste0("  metrics          = ", paste(metrics, collapse=","), "\n"))
cat(paste0("  from_tsv         = ", from_tsv,                    "\n"))
cat(paste0("  no_rds           = ", no_rds,                      "\n\n"))

# ---- ライブラリ読み込み ----
suppressPackageStartupMessages({
  library(data.table)
  library(igraph)
  library(ggplot2)
  library(parallel)
})

# ---- サブスクリプトのパス取得とsource ----
# IBMSEQ_SOURCED を TRUE にしてから source することで、
# 各サブスクリプトのスタンドアロン実行ブロックが動かないようにする
IBMSEQ_SOURCED <- TRUE

script_args <- commandArgs(trailingOnly = FALSE)
script_path  <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir   <- if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()

source(file.path(script_dir, "01_load_graph.R"))
source(file.path(script_dir, "02_clustering.R"))
source(file.path(script_dir, "03_density.R"))
source(file.path(script_dir, "04_features.R"))
source(file.path(script_dir, "05_plot.R"))

# ============================================================
# Step 1: データ読み込み・グラフ構築
# ============================================================
cat("\n--- Step 1: Load Graph ---\n")
load_graph(name, read_path, save_path, dup_mode)

# ============================================================
# Step 2: Louvainクラスタリング（連結成分ごと）
# ============================================================
cat("\n--- Step 2: Clustering ---\n")
run_clustering(name, save_path, num_cores = num_cores)

# ============================================================
# Step 3: 密度計算
# ============================================================
cat("\n--- Step 3: Density ---\n")
run_density(name, save_path, min_cluster_size, num_cores,
            compute_edge_density = "edge_density" %in% metrics,
            save_rds = !no_rds)

# ============================================================
# Step 4: その他特徴量計算
# ============================================================
cat("\n--- Step 4: Features ---\n")
run_features(name, save_path, min_cluster_size, num_cores,
             metrics  = intersect(metrics, c("umi_uei", "ego_size", "diameter")),
             save_rds = !no_rds)

# ============================================================
# Step 5: 作図
# ============================================================
cat("\n--- Step 5: Plot ---\n")
run_plots(name, save_path, from_tsv = from_tsv)

cat(paste0("\n=== IBMseq Pipeline DONE: ", Sys.time(), " ===\n"))
cat(paste0("Output: ", save_path, "\n"))
