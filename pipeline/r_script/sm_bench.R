#!/usr/bin/env Rscript
# =============================================================================
# sm_bench.R
# suffix mode の正確性検証・速度比較・全指標可視化
#
# 【設計方針】
#   Louvain はランダムなので、異なるパイプライン実行で比較すると
#   クラスター割り当てが変わり、指標の合計値が必ず異なる。
#   このスクリプトでは sm_save の「同一クラスタリング結果」を
#   両実装（orig=grepl / sfx=group-by）で共有し公平に比較する。
#
#   umi_uei は両実装で計算し直接比較（完全一致すべき）。
#   他の指標（cluster_size, edge_density, ego_size, diameter）は
#   両モード共通実装なので sm_save の TSV をそのまま表示。
#
# 前提:
#   sm_save に sm_00_main.R（または sm_02/03/04）の出力が揃っていること:
#     {name}_02_membership.rds   ← node_type 列あり
#     {name}_03_cluster_size.rds/.tsv
#     {name}_03_edge_density.tsv
#     {name}_04_ego_size.tsv
#     {name}_04_diameter.tsv
#
# Usage:
#   Rscript sm_bench.R <name> <sm_save> [min_cluster_size] [n_reps] [out_pdf]
#
# 例:
#   Rscript sm_bench.R V5P2_24aB_CTCF_2_3000 tmp/sm 3 10 tmp/sm_bench.pdf
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript sm_bench.R <name> <sm_save> [min_cluster_size] [n_reps] [out_pdf]")
}
name             <- args[1]
sm_save          <- args[2]
min_cluster_size <- if (length(args) >= 3) as.integer(args[3]) else 1000L
n_reps           <- if (length(args) >= 4) as.integer(args[4]) else 5L
out_pdf          <- if (length(args) >= 5) args[5] else file.path(sm_save, paste0(name, "_sm_bench.pdf"))

cat("=== sm_bench START ===\n")
cat(paste0("  name             = ", name, "\n"))
cat(paste0("  sm_save          = ", sm_save, "\n"))
cat(paste0("  min_cluster_size = ", min_cluster_size, "\n"))
cat(paste0("  n_reps           = ", n_reps, "\n"))
cat(paste0("  out_pdf          = ", out_pdf, "\n\n"))

# =============================================================================
# 共有データ読み込み
# =============================================================================
mem_path  <- file.path(sm_save, paste0(name, "_02_membership.rds"))
size_path <- file.path(sm_save, paste0(name, "_03_cluster_size.rds"))
if (!file.exists(mem_path))  stop(paste("Not found:", mem_path))
if (!file.exists(size_path)) stop(paste("Not found:", size_path))

mem_dt       <- as.data.table(readRDS(mem_path))
cluster_size <- readRDS(size_path)
large_ids    <- as.data.table(cluster_size)[total > min_cluster_size, community_id]

cat(paste0("Membership rows : ", nrow(mem_dt), "\n"))
cat(paste0("Large clusters  : ", length(large_ids), " (> ", min_cluster_size, " nodes)\n"))

if (!"node_type" %in% names(mem_dt)) {
  stop("node_type column not found. Run sm_02_clustering.R (suffix mode) first.")
}
cat("\nnode_type distribution:\n")
print(table(mem_dt$node_type, useNA = "ifany"))
cat("\n")

# =============================================================================
# umi_uei の2実装定義
# =============================================================================
NODE_TYPE_LABELS <- c("1" = "UMI1", "2" = "UMI2", "3" = "UEI1", "4" = "UEI2")
suffixes <- c("UMI1" = "\\.m1$", "UMI2" = "\\.t2$", "UEI1" = "\\.e1$", "UEI2" = "\\.e2$")

run_umi_uei_orig <- function() {
  if (length(large_ids) == 0) return(data.table())
  cluster_list <- split(
    mem_dt[community_id %in% large_ids, name],
    mem_dt[community_id %in% large_ids, community_id]
  )
  umi_uei_list <- lapply(names(suffixes), function(sn) {
    pat <- suffixes[[sn]]
    res <- lapply(names(cluster_list), function(cl) {
      cnt <- sum(grepl(pat, cluster_list[[cl]]))
      if (cnt > 0) data.table(community_id = as.integer(cl), variation = cnt,
                               type = paste(name, sn, sep = "_"))
      else NULL
    })
    rbindlist(res[!sapply(res, is.null)])
  })
  rbindlist(umi_uei_list[lengths(umi_uei_list) > 0])
}

run_umi_uei_sfx <- function() {
  if (length(large_ids) == 0) return(data.table())
  dt <- mem_dt[community_id %in% large_ids & !is.na(node_type),
               .(variation = .N), by = .(community_id, node_type)]
  if (nrow(dt) > 0) {
    dt[, type := paste(name, NODE_TYPE_LABELS[as.character(node_type)], sep = "_")]
    dt[, node_type := NULL]
  }
  dt
}

# =============================================================================
# 正確性検証
# =============================================================================
cat("=== Correctness check ===\n")
res_orig <- run_umi_uei_orig()
res_sfx  <- run_umi_uei_sfx()

extract_totals <- function(dt) {
  if (nrow(dt) == 0) return(data.table(type_short = character(), total = integer()))
  dt[, .(total = sum(variation)), by = .(type_short = sub("^.*_", "", type))][order(type_short)]
}
tot_orig <- extract_totals(res_orig)
tot_sfx  <- extract_totals(res_sfx)

merged <- merge(tot_orig, tot_sfx, by = "type_short", suffixes = c("_orig", "_sfx"), all = TRUE)
merged[is.na(merged)] <- 0L
merged[, match := total_orig == total_sfx]
merged[, diff  := total_orig - total_sfx]
cat("  Total counts per type (same clustering → should match exactly):\n")
print(merged)

pass <- all(merged$match, na.rm = TRUE)
cat(paste0("\n  Result: [", if (pass) "PASS" else "FAIL", "]\n\n"))

# =============================================================================
# 速度比較
# =============================================================================
cat(paste0("=== Speed comparison (", n_reps, " reps) ===\n"))
times_orig <- numeric(n_reps)
times_sfx  <- numeric(n_reps)
for (i in seq_len(n_reps)) {
  times_orig[i] <- system.time(run_umi_uei_orig())["elapsed"]
  cat(sprintf("  [orig] rep %2d: %.4f s\n", i, times_orig[i]))
}
cat("\n")
for (i in seq_len(n_reps)) {
  times_sfx[i] <- system.time(run_umi_uei_sfx())["elapsed"]
  cat(sprintf("  [sfx]  rep %2d: %.4f s\n", i, times_sfx[i]))
}
cat(sprintf("\n  orig  mean=%.4f s  median=%.4f s  min=%.4f s\n",
            mean(times_orig), median(times_orig), min(times_orig)))
cat(sprintf("  sfx   mean=%.4f s  median=%.4f s  min=%.4f s\n",
            mean(times_sfx),  median(times_sfx),  min(times_sfx)))
cat(sprintf("  Speedup (mean): %.2fx   (median): %.2fx\n\n",
            mean(times_orig)/mean(times_sfx), median(times_orig)/median(times_sfx)))

# =============================================================================
# 図作成
# =============================================================================
load_tsv <- function(stem) {
  p <- file.path(sm_save, paste0(name, "_", stem, ".tsv"))
  if (!file.exists(p)) { cat(paste0("  SKIP (not found): ", basename(p), "\n")); return(NULL) }
  fread(p)
}

my_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border     = element_rect(color = "black", linewidth = 1),
        axis.text        = element_text(color = "black", size = 13),
        axis.title       = element_text(color = "black", size = 15),
        plot.title       = element_text(size = 14, face = "bold"),
        legend.position  = "right", legend.title = element_blank())

IMPL_COLORS <- c("orig (grepl)"  = "#E41A1C",
                 "sfx (groupby)" = "#377EB8")

cat(paste0("=== Generating figures → ", out_pdf, " ===\n"))
pdf(out_pdf, width = 10, height = 7)

# ---- 表紙 ----
plot.new()
result_str <- if (pass) "[PASS] Counts match" else "[FAIL] Counts differ!"
text(0.5, 0.75,
     paste0("sm_bench: suffix mode verification\n", name),
     cex = 1.4, font = 2, adj = 0.5)
text(0.5, 0.55, result_str, cex = 1.6, col = if (pass) "darkgreen" else "red", adj = 0.5)
text(0.5, 0.38,
     paste0("Correctness: orig (grepl) vs sfx (group-by)\n",
            "on SAME clustering data (sm_save)\n\n",
            "Speedup (mean): ", round(mean(times_orig)/mean(times_sfx), 2), "x\n",
            "min_cluster_size = ", min_cluster_size, "   large_clusters = ", length(large_ids)),
     cex = 1.0, adj = 0.5)

# ---- 1. umi_uei: orig vs sfx の分布比較（同一データ → 一致するはず）----
cat("  [fig 1] umi_uei orig vs sfx\n")
if (nrow(res_orig) > 0 || nrow(res_sfx) > 0) {
  res_orig[, impl := "orig (grepl)"]
  res_sfx[ , impl := "sfx (groupby)"]
  combined_uu <- rbindlist(list(res_orig, res_sfx), fill = TRUE)
  combined_uu[, type_short := sub("^.*_", "", type)]
  combined_uu[, impl := factor(impl, levels = names(IMPL_COLORS))]

  p <- ggplot(combined_uu, aes(x = impl, y = variation, fill = impl)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.15, outlier.shape = NA, notch = FALSE) +
    scale_fill_manual(values = IMPL_COLORS) +
    scale_y_log10() +
    facet_wrap(~ type_short, nrow = 1) +
    labs(title = "1. umi_uei: orig (grepl) vs sfx (group-by)  [same clustering]",
         subtitle = if (pass) "PASS: distributions should overlap perfectly"
                    else      "FAIL: distributions differ!",
         x = "Implementation", y = "Count per cluster (log10)") +
    my_theme +
    theme(strip.text = element_text(size = 12),
          plot.subtitle = element_text(color = if (pass) "darkgreen" else "red"))
  print(p)

  # total count bar chart
  tot_long <- rbind(
    tot_orig[, .(type_short, total, impl = "orig (grepl)")],
    tot_sfx[ , .(type_short, total, impl = "sfx (groupby)")]
  )
  tot_long[, impl := factor(impl, levels = names(IMPL_COLORS))]
  p2 <- ggplot(tot_long, aes(x = type_short, y = total, fill = impl)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = IMPL_COLORS) +
    labs(title = "1b. umi_uei total counts  [same clustering, should be identical]",
         x = "Type", y = "Total count across all large clusters") +
    my_theme
  print(p2)
}

# ---- 2. Cluster Size ----
cat("  [fig 2] cluster_size\n")
cs <- load_tsv("03_cluster_size")
if (!is.null(cs) && nrow(cs) > 0) {
  cs[, library := name]
  p <- ggplot(cs, aes(x = library, y = total)) +
    geom_violin(fill = "#999999", alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    scale_y_log10() +
    labs(title = "2. Cluster Size (all clusters)",
         x = "", y = "Nodes per cluster (log10)") +
    my_theme
  print(p)
  cat(paste0("  cluster_size: n=", nrow(cs), "  median=", median(cs$total),
             "  max=", max(cs$total), "\n"))
}

# ---- 3. Edge Density ----
cat("  [fig 3] edge_density\n")
ed <- load_tsv("03_edge_density")
if (!is.null(ed) && nrow(ed) > 0) {
  ed[, library := name]
  p <- ggplot(ed, aes(x = library, y = ED)) +
    geom_violin(fill = "#999999", alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    labs(title = paste0("3. Edge Density  (", nrow(ed), " large clusters)"),
         x = "", y = "Edge Density") +
    my_theme
  print(p)
  cat(paste0("  edge_density: n=", nrow(ed), "  median=", round(median(ed$ED), 4),
             "  mean=", round(mean(ed$ED), 4), "\n"))
}

# ---- 4. Ego Size ----
cat("  [fig 4] ego_size\n")
es <- load_tsv("04_ego_size")
if (!is.null(es) && nrow(es) > 0) {
  p <- ggplot(es, aes(x = value)) +
    geom_density(aes(color = community_id), show.legend = FALSE, linewidth = 0.5) +
    scale_x_log10(limits = c(4, 2000)) +
    ylim(0, 2.0) +
    labs(title = paste0("4. Ego Size (order=3)  [", length(unique(es$community_id)), " clusters]"),
         x = "Ego Size", y = "Density") +
    my_theme + theme(legend.position = "none")
  print(p)
  cat(paste0("  ego_size: n=", nrow(es), "  median=", round(median(es$value), 1),
             "  mean=", round(mean(es$value), 1), "\n"))
}

# ---- 5. Diameter ----
cat("  [fig 5] diameter\n")
dm <- load_tsv("04_diameter")
if (!is.null(dm) && nrow(dm) > 0) {
  dm[, library := name]
  p <- ggplot(dm, aes(x = library, y = diameter)) +
    geom_violin(fill = "#999999", alpha = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, notch = FALSE) +
    labs(title = paste0("5. Diameter  (", nrow(dm), " large clusters)"),
         x = "", y = "Diameter") +
    my_theme
  print(p)
  cat(paste0("  diameter: n=", nrow(dm), "  median=", median(dm$diameter),
             "  mean=", round(mean(dm$diameter), 2), "\n"))
}

dev.off()
cat(paste0("\n=== sm_bench DONE: ", out_pdf, " ===\n"))
