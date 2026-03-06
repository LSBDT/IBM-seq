#!/usr/bin/env Rscript
# =============================================================================
# compare_modes.R
# 3モード（original / modified / sm_modified）の指標分布を比較する図を作成
#
# Usage:
#   Rscript compare_modes.R <name> <orig_dir> <mod_dir> <sm_dir> <out_pdf> [min_cluster_size]
#
# 入力（各ディレクトリから TSV を読み込む）:
#   {name}_03_cluster_size.tsv
#   {name}_03_edge_density.tsv
#   {name}_04_umi_uei.tsv
#   {name}_04_ego_size.tsv
#   {name}_04_diameter.tsv
#
# 出力:
#   <out_pdf>               比較図（violin + boxplot、5指標）
#   <out_pdf>.summary.tsv   数値サマリー（中央値・平均・件数）
#
# 注意:
#   Louvain はランダム初期化のため、モード間でクラスター割り当ては一致しない。
#   比較は分布（violin/boxplot）と total カウントで行う。
#   umi_uei の total カウントは modified vs sm_modified で一致するはず（実装検証）。
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("Usage: Rscript compare_modes.R <name> <orig_dir> <mod_dir> <sm_dir> <out_pdf> [min_cluster_size]")
}
name             <- args[1]
orig_dir         <- args[2]
mod_dir          <- args[3]
sm_dir           <- args[4]
out_pdf          <- args[5]
min_cluster_size <- if (length(args) >= 6) as.integer(args[6]) else 1000L

MODE_LEVELS <- c("original", "modified", "sm_modified")
MODE_COLORS <- c("original"    = "#E41A1C",
                 "modified"    = "#377EB8",
                 "sm_modified" = "#4DAF4A")

# ---- ヘルパー ----
load_tsv <- function(dir, stem) {
  path <- file.path(dir, paste0(name, "_", stem, ".tsv"))
  if (!file.exists(path)) {
    cat(paste0("  SKIP (not found): ", basename(path), " in ", dir, "\n"))
    return(NULL)
  }
  fread(path)
}

my_theme <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", linewidth = 1),
    axis.text.x      = element_text(color = "black", size = 13, angle = 30, hjust = 1),
    axis.text.y      = element_text(color = "black", size = 13),
    axis.title       = element_text(color = "black", size = 15),
    plot.title       = element_text(size = 15, face = "bold"),
    legend.position  = "none"
  )

cat("=== compare_modes START ===\n")
cat(paste0("  name     = ", name, "\n"))
cat(paste0("  orig_dir = ", orig_dir, "\n"))
cat(paste0("  mod_dir  = ", mod_dir,  "\n"))
cat(paste0("  sm_dir   = ", sm_dir,   "\n"))
cat(paste0("  out_pdf  = ", out_pdf,  "\n\n"))

summary_rows <- list()

pdf(out_pdf, width = 10, height = 7)

# ---- 表紙 ----
plot.new()
text(0.5, 0.7, paste0("IBMseq Mode Comparison\n", name),
     cex = 1.4, font = 2, adj = 0.5)
text(0.5, 0.5,
     paste0("original   : ike.R equivalent (no dedup, sequential Louvain)\n",
            "modified   : 00_main.R (--no-dup, parallel Louvain + trivial skip)\n",
            "sm_modified: sm_00_main.R (--no-dup, suffix mode umi_uei)\n\n",
            "Louvain is stochastic: cluster assignments differ between runs.\n",
            "Comparison is distributional (violin/boxplot) + total counts.\n",
            "umi_uei totals: modified vs sm_modified should match exactly."),
     cex = 0.9, adj = 0.5)

# ============================================================
# 1. Cluster Size
# ============================================================
cat("  [1/5] cluster_size\n")
dt_list <- mapply(function(d, m) {
  x <- load_tsv(d, "03_cluster_size")
  if (!is.null(x)) x[, mode := m]
  x
}, list(orig_dir, mod_dir, sm_dir), MODE_LEVELS, SIMPLIFY = FALSE)

combined <- rbindlist(dt_list[!sapply(dt_list, is.null)])
if (nrow(combined) > 0) {
  combined[, mode := factor(mode, levels = MODE_LEVELS)]
  p <- ggplot(combined, aes(x = mode, y = total, fill = mode)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    scale_fill_manual(values = MODE_COLORS) +
    scale_y_log10() +
    labs(title = "1. Cluster Size (nodes per cluster)",
         x = "Mode", y = "Nodes per cluster (log10)") +
    my_theme
  print(p)
  s <- combined[, .(metric="cluster_size", n=.N,
                    median=median(total), mean=round(mean(total),1),
                    sd=round(sd(total),1), max=max(total)), by=mode]
  cat("  Summary:\n"); print(s)
  summary_rows[[length(summary_rows)+1]] <- s
}

# ============================================================
# 2. Edge Density
# ============================================================
cat("  [2/5] edge_density\n")
dt_list <- mapply(function(d, m) {
  x <- load_tsv(d, "03_edge_density")
  if (!is.null(x)) x[, mode := m]
  x
}, list(orig_dir, mod_dir, sm_dir), MODE_LEVELS, SIMPLIFY = FALSE)

combined <- rbindlist(dt_list[!sapply(dt_list, is.null)])
if (nrow(combined) > 0) {
  combined[, mode := factor(mode, levels = MODE_LEVELS)]
  p <- ggplot(combined, aes(x = mode, y = ED, fill = mode)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    scale_fill_manual(values = MODE_COLORS) +
    labs(title = "2. Edge Density per cluster",
         x = "Mode", y = "Edge Density") +
    my_theme
  print(p)
  s <- combined[, .(metric="edge_density", n=.N,
                    median=round(median(ED),4), mean=round(mean(ED),4),
                    sd=round(sd(ED),4)), by=mode]
  cat("  Summary:\n"); print(s)
  summary_rows[[length(summary_rows)+1]] <- s
}

# ============================================================
# 3. UMI/UEI Count
# ============================================================
cat("  [3/5] umi_uei\n")
dt_list <- mapply(function(d, m) {
  x <- load_tsv(d, "04_umi_uei")
  if (!is.null(x)) x[, mode := m]
  x
}, list(orig_dir, mod_dir, sm_dir), MODE_LEVELS, SIMPLIFY = FALSE)

combined <- rbindlist(dt_list[!sapply(dt_list, is.null)])
if (nrow(combined) > 0) {
  combined[, mode := factor(mode, levels = MODE_LEVELS)]
  combined[, type_short := sub("^.*_", "", type)]

  p <- ggplot(combined, aes(x = mode, y = variation, fill = mode)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    scale_fill_manual(values = MODE_COLORS) +
    scale_y_log10() +
    facet_wrap(~ type_short, nrow = 1) +
    labs(title = "3. UMI/UEI count per cluster",
         x = "Mode", y = "Count per cluster (log10)") +
    my_theme + theme(strip.text = element_text(size = 12))
  print(p)

  # Total counts (modified vs sm_modified should match)
  tot <- combined[, .(total = sum(variation)), by = .(mode, type_short)][order(mode, type_short)]
  cat("  Total counts by type (modified vs sm_modified should match):\n")
  print(dcast(tot, type_short ~ mode, value.var = "total"))

  s <- combined[, .(metric=paste0("umi_uei_", type_short[1]), n=.N,
                    median=median(variation), mean=round(mean(variation),1),
                    total=sum(variation)), by=.(mode, type_short)]
  summary_rows[[length(summary_rows)+1]] <- s[, .(mode, metric=paste0("umi_uei_",type_short),
                                                   n, median, mean, total)]
}

# ============================================================
# 4. Ego Size
# ============================================================
cat("  [4/5] ego_size\n")
dt_list <- mapply(function(d, m) {
  x <- load_tsv(d, "04_ego_size")
  if (!is.null(x)) x[, mode := m]
  x
}, list(orig_dir, mod_dir, sm_dir), MODE_LEVELS, SIMPLIFY = FALSE)

combined <- rbindlist(dt_list[!sapply(dt_list, is.null)])
if (nrow(combined) > 0) {
  combined[, mode := factor(mode, levels = MODE_LEVELS)]
  p <- ggplot(combined, aes(x = value, color = mode)) +
    geom_density(linewidth = 0.9) +
    scale_color_manual(values = MODE_COLORS) +
    scale_x_log10(limits = c(4, 2000)) +
    ylim(0, 2.0) +
    labs(title = "4. Ego Size distribution (order=3)",
         x = "Ego Size", y = "Density") +
    theme_bw() +
    theme(panel.grid  = element_blank(),
          axis.text   = element_text(size = 13, color = "black"),
          axis.title  = element_text(size = 15, color = "black"),
          plot.title  = element_text(size = 15, face = "bold"),
          legend.position = "right",
          legend.title    = element_blank())
  print(p)
  s <- combined[, .(metric="ego_size", n=.N,
                    median=round(median(value),1), mean=round(mean(value),1),
                    sd=round(sd(value),1)), by=mode]
  cat("  Summary:\n"); print(s)
  summary_rows[[length(summary_rows)+1]] <- s
}

# ============================================================
# 5. Diameter
# ============================================================
cat("  [5/5] diameter\n")
dt_list <- mapply(function(d, m) {
  x <- load_tsv(d, "04_diameter")
  if (!is.null(x)) x[, mode := m]
  x
}, list(orig_dir, mod_dir, sm_dir), MODE_LEVELS, SIMPLIFY = FALSE)

combined <- rbindlist(dt_list[!sapply(dt_list, is.null)])
if (nrow(combined) > 0) {
  combined[, mode := factor(mode, levels = MODE_LEVELS)]
  p <- ggplot(combined, aes(x = mode, y = diameter, fill = mode)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    scale_fill_manual(values = MODE_COLORS) +
    labs(title = "5. Diameter per cluster",
         x = "Mode", y = "Diameter") +
    my_theme
  print(p)
  s <- combined[, .(metric="diameter", n=.N,
                    median=median(diameter), mean=round(mean(diameter),2),
                    sd=round(sd(diameter),2)), by=mode]
  cat("  Summary:\n"); print(s)
  summary_rows[[length(summary_rows)+1]] <- s
}

dev.off()

# ---- 数値サマリーを TSV 出力 ----
if (length(summary_rows) > 0) {
  summary_all <- rbindlist(summary_rows, fill = TRUE)
  tsv_out <- paste0(out_pdf, ".summary.tsv")
  fwrite(summary_all, tsv_out, sep = "\t")
  cat(paste0("\n  Summary TSV: ", tsv_out, "\n"))
}

cat(paste0("\n=== compare_modes DONE: ", Sys.time(), " ===\n"))
cat(paste0("Output PDF: ", out_pdf, "\n"))
