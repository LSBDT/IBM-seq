#!/usr/bin/env Rscript
# =============================================================================
# test_compare_with_then.R
# --with-dup と --dup-then-dedup のクラスター数・分布を比較するテストスクリプト
#
# Usage:
#   Rscript test_compare_with_then.R <name> <data_dir> <out_base> [min_cluster_size]
#
# 引数:
#   name             : サンプル名（例: V5P2_24aB_CTCF_2_3000）
#   data_dir         : .link.gz が置かれているディレクトリ
#   out_base         : 出力ベースディレクトリ（自動的にサブディレクトリを作成）
#   min_cluster_size : クラスターサイズ閾値（デフォルト: 3）
#
# 出力:
#   {out_base}/with_dup/     : --with-dup モードの中間ファイル
#   {out_base}/dup_dedup/    : --dup-then-dedup モードの中間ファイル
#   {out_base}/compare_with_then.pdf  : 比較グラフ
#   {out_base}/compare_with_then_summary.tsv : 数値サマリー
#
# 例:
#   Rscript test_compare_with_then.R \
#     V5P2_24aB_CTCF_2_3000 data data/output_test_compare 3
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(igraph)
  library(ggplot2)
  library(parallel)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript test_compare_with_then.R <name> <data_dir> <out_base> [min_cluster_size] [num_cores]")
}

name             <- args[1]
data_dir         <- args[2]
out_base         <- args[3]
min_cluster_size <- if (length(args) >= 4) as.integer(args[4]) else 3L
num_cores        <- if (length(args) >= 5) as.integer(args[5]) else 1L

out_with_dup  <- file.path(out_base, "with_dup")
out_dup_dedup <- file.path(out_base, "dup_dedup")
out_pdf       <- file.path(out_base, "compare_with_then.pdf")
out_tsv       <- file.path(out_base, "compare_with_then_summary.tsv")

dir.create(out_with_dup,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dup_dedup, recursive = TRUE, showWarnings = FALSE)

cat("=== test_compare_with_then START ===\n")
cat(paste0("  name             = ", name,             "\n"))
cat(paste0("  data_dir         = ", data_dir,         "\n"))
cat(paste0("  out_with_dup     = ", out_with_dup,     "\n"))
cat(paste0("  out_dup_dedup    = ", out_dup_dedup,    "\n"))
cat(paste0("  min_cluster_size = ", min_cluster_size, "\n"))
cat(paste0("  num_cores        = ", num_cores,        "\n\n"))

# ---- サブスクリプトの source ----
IBMSEQ_SOURCED <- TRUE
script_args <- commandArgs(trailingOnly = FALSE)
script_path  <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir   <- if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()

# r_script ディレクトリへのパスを取得 (dev/validation -> r_script)
r_script_dir <- file.path(dirname(dirname(script_dir)), "r_script")

source(file.path(r_script_dir, "01_load_graph.R"))
source(file.path(r_script_dir, "02_clustering.R"))
source(file.path(r_script_dir, "03_density.R"))

# ============================================================
# モード 1: --with-dup (重複エッジを含んだまま全処理)
# ============================================================
cat("\n--- [Mode 1/2] --with-dup ---\n")
writeLines("", file.path(out_with_dup, paste0(name, "_process.log")))

load_graph(name, data_dir, out_with_dup, dup_mode = "--with-dup")
run_clustering(name, out_with_dup, num_cores = num_cores, dedup_after = FALSE)
run_density(name, out_with_dup, min_cluster_size, num_cores = num_cores,
            compute_edge_density = FALSE, save_rds = FALSE)

# ============================================================
# モード 2: --dup-then-dedup (クラスタリング後に重複除去)
# ============================================================
cat("\n--- [Mode 2/2] --dup-then-dedup ---\n")
writeLines("", file.path(out_dup_dedup, paste0(name, "_process.log")))

load_graph(name, data_dir, out_dup_dedup, dup_mode = "--with-dup")
run_clustering(name, out_dup_dedup, num_cores = num_cores, dedup_after = TRUE)
run_density(name, out_dup_dedup, min_cluster_size, num_cores = num_cores,
            compute_edge_density = FALSE, save_rds = FALSE)

# ============================================================
# 結果読み込み
# ============================================================
cat("\n--- Loading results ---\n")

load_cluster_size <- function(dir, mode_label) {
  path <- file.path(dir, paste0(name, "_03_cluster_size.tsv"))
  if (!file.exists(path)) stop(paste("Not found:", path))
  dt <- fread(path)
  dt[, mode := mode_label]
  dt
}

dt_with_dup  <- load_cluster_size(out_with_dup,  "--with-dup")
dt_dup_dedup <- load_cluster_size(out_dup_dedup, "--dup-then-dedup")
combined <- rbindlist(list(dt_with_dup, dt_dup_dedup))
combined[, mode := factor(mode, levels = c("--with-dup", "--dup-then-dedup"))]

# ============================================================
# 数値サマリー
# ============================================================
cat("\n--- Summary ---\n")

summary_dt <- combined[, .(
  n_clusters       = .N,
  n_clusters_large = sum(total > min_cluster_size),
  median_size      = median(total),
  mean_size        = round(mean(total), 1),
  max_size         = max(total),
  total_nodes      = sum(total)
), by = mode]

print(summary_dt)
fwrite(summary_dt, out_tsv, sep = "\t")
cat(paste0("  Saved summary: ", out_tsv, "\n"))

# ============================================================
# 作図
# ============================================================
cat("\n--- Plotting ---\n")

MODE_COLORS <- c("--with-dup" = "#4DAF4A", "--dup-then-dedup" = "#E41A1C")

my_theme <- theme_bw() +
  theme(
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(color = "black", linewidth = 1),
    axis.text.x       = element_text(color = "black", size = 13),
    axis.text.y       = element_text(color = "black", size = 13),
    axis.title        = element_text(color = "black", size = 15),
    plot.title        = element_text(size = 14, face = "bold"),
    plot.subtitle     = element_text(size = 11, color = "gray30"),
    legend.position   = "none"
  )

pdf(out_pdf, width = 10, height = 8)

# ---- 表紙 ----
plot.new()
n_with_dup  <- summary_dt[mode == "--with-dup",      n_clusters]
n_dup_dedup <- summary_dt[mode == "--dup-then-dedup", n_clusters]
text(0.5, 0.75,
     paste0("--with-dup vs --dup-then-dedup\nCluster Count Comparison\n", name),
     cex = 1.4, font = 2, adj = 0.5)
text(0.5, 0.52,
     paste0("--with-dup       : ", n_with_dup,  " clusters\n",
            "--dup-then-dedup : ", n_dup_dedup, " clusters\n",
            "Difference       : ", n_dup_dedup - n_with_dup,
            " (", round((n_dup_dedup - n_with_dup) / n_with_dup * 100, 1), "%)"),
     cex = 1.1, adj = 0.5)
text(0.5, 0.30,
     paste0("min_cluster_size = ", min_cluster_size, "\n",
            "Both modes cluster with duplicates.\n",
            "--dup-then-dedup removes duplicates after clustering."),
     cex = 0.9, col = "gray30", adj = 0.5)

# ---- 1. クラスター総数の棒グラフ ----
p1 <- ggplot(summary_dt, aes(x = mode, y = n_clusters, fill = mode)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = n_clusters), vjust = -0.4, size = 5, fontface = "bold") +
  scale_fill_manual(values = MODE_COLORS) +
  labs(title   = "1. Total Number of Clusters",
       subtitle = "(trivial + Louvain communities)",
       x = "Mode", y = "Number of Clusters") +
  my_theme
print(p1)

# ---- 2. 閾値以上のクラスター数の棒グラフ ----
p2 <- ggplot(summary_dt, aes(x = mode, y = n_clusters_large, fill = mode)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = n_clusters_large), vjust = -0.4, size = 5, fontface = "bold") +
  scale_fill_manual(values = MODE_COLORS) +
  labs(title   = paste0("2. Clusters > ", min_cluster_size, " Nodes"),
       subtitle = paste0("(clusters exceeding min_cluster_size = ", min_cluster_size, ")"),
       x = "Mode", y = "Number of Large Clusters") +
  my_theme
print(p2)

# ---- 3. クラスターサイズ分布（violin + boxplot） ----
if (nrow(combined) > 0) {
  p3 <- ggplot(combined, aes(x = mode, y = total, fill = mode)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA) +
    scale_fill_manual(values = MODE_COLORS) +
    scale_y_log10() +
    labs(title   = "3. Cluster Size Distribution (log10)",
         subtitle = "(all clusters, violin + boxplot)",
         x = "Mode", y = "Nodes per cluster (log10)") +
    my_theme
  print(p3)
}

# ---- 4. クラスターサイズの累積分布（ECDF） ----
if (nrow(combined) > 0) {
  p4 <- ggplot(combined, aes(x = total, color = mode)) +
    stat_ecdf(linewidth = 1) +
    scale_color_manual(values = MODE_COLORS) +
    scale_x_log10() +
    labs(title   = "4. Cumulative Distribution of Cluster Size",
         subtitle = "(ECDF, all clusters)",
         x = "Nodes per cluster (log10)", y = "Cumulative Proportion") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", linewidth = 1),
      axis.text        = element_text(color = "black", size = 13),
      axis.title       = element_text(color = "black", size = 15),
      plot.title       = element_text(size = 14, face = "bold"),
      plot.subtitle    = element_text(size = 11, color = "gray30"),
      legend.position  = "right",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 11)
    )
  print(p4)
}

# ---- 5. クラスターサイズの頻度ヒストグラム（サイズ段階別） ----
if (nrow(combined) > 0) {
  size_breaks <- c(1, 2, 3, 5, 10, 20, 50, Inf)
  size_labels <- c("1", "2", "3-4", "5-9", "10-19", "20-49", "50+")
  combined[, size_bin := cut(total, breaks = size_breaks, labels = size_labels,
                              right = FALSE, include.lowest = TRUE)]
  bin_count <- combined[, .N, by = .(mode, size_bin)]
  bin_count[, size_bin := factor(size_bin, levels = size_labels)]

  p5 <- ggplot(bin_count, aes(x = size_bin, y = N, fill = mode)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = N), position = position_dodge(width = 0.7),
              vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = MODE_COLORS) +
    labs(title   = "5. Cluster Count by Size Bin",
         subtitle = "(number of clusters per size category)",
         x = "Cluster Size (nodes)", y = "Number of Clusters",
         fill = "Mode") +
    theme_bw() +
    theme(
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(color = "black", linewidth = 1),
      axis.text         = element_text(color = "black", size = 13),
      axis.title        = element_text(color = "black", size = 15),
      plot.title        = element_text(size = 14, face = "bold"),
      plot.subtitle     = element_text(size = 11, color = "gray30"),
      legend.position   = "right",
      legend.text       = element_text(size = 11),
      legend.title      = element_text(size = 12)
    )
  print(p5)
}

dev.off()

cat(paste0("\n=== test_compare_with_then DONE: ", Sys.time(), " ===\n"))
cat(paste0("  PDF    : ", out_pdf, "\n"))
cat(paste0("  Summary: ", out_tsv, "\n"))
