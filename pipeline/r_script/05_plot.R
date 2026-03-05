# =============================================================================
# 05_plot.R
# 作図（全ステップの結果から PDF を生成）
#
# Input (RDS または TSV、--from-tsv 指定時は TSV を使用):
#   {save_path}/{name}_03_cluster_size.rds/.tsv
#   {save_path}/{name}_03_edge_density.rds/.tsv
#   {save_path}/{name}_04_umi_uei.rds/.tsv
#   {save_path}/{name}_04_ego_size.rds/.tsv
#   {save_path}/{name}_04_diameter.rds/.tsv
#
# Output:
#   {save_path}/{name}_05_cluster_size.pdf
#   {save_path}/{name}_05_edge_density.pdf
#   {save_path}/{name}_05_umi_uei.pdf
#   {save_path}/{name}_05_ego_size.pdf
#   {save_path}/{name}_05_diameter.pdf
#
# 使い方（スタンドアロン）:
#   Rscript 05_plot.R <name> <save_path> [--from-tsv]
#
#   --from-tsv: RDS の代わりに TSV ファイルから読み込む
#               計算ステップを再実行せずに図だけ作り直す場合に使用
# =============================================================================

run_plots <- function(name, save_path, from_tsv = FALSE) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[05_plot] START: ", Sys.time(),
                   "  from_tsv=", from_tsv))

  # ---- テーマ設定 ----
  my_plot2 <- theme_bw() +
    theme(
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(color = "black", linewidth = 1),
      axis.text.x       = element_text(color = "black", size = 16, angle = 45, hjust = 1),
      axis.text.y       = element_text(color = "black", size = 16),
      axis.title        = element_text(color = "black", size = 20),
      legend.background = element_blank(),
      legend.text       = element_text(size = 8),
      legend.title      = element_blank()
    )

  # ---- Helper: RDS または TSV を存在確認付きで読み込む ----
  load_data <- function(stem) {
    if (from_tsv) {
      path <- file.path(save_path, paste0(name, "_", stem, ".tsv"))
      if (!file.exists(path)) {
        write_log(paste0("  SKIP (TSV not found): ", basename(path)))
        return(NULL)
      }
      fread(path)
    } else {
      path <- file.path(save_path, paste0(name, "_", stem, ".rds"))
      if (!file.exists(path)) {
        write_log(paste0("  SKIP (RDS not found): ", basename(path)))
        return(NULL)
      }
      readRDS(path)
    }
  }

  # ============================================================
  # 1. クラスター規模（violin + boxplot）
  # ============================================================
  ms <- load_data("03_cluster_size")
  if (!is.null(ms) && nrow(ms) > 0) {
    ms$library <- name
    p <- ggplot(ms, aes(x = library, y = total)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = (nrow(ms) > 5)) +
      labs(x = "Library", y = "Total Number of Nodes per Cluster") +
      scale_y_log10() +
      my_plot2
    pdf_out <- file.path(save_path, paste0(name, "_05_cluster_size.pdf"))
    pdf(pdf_out); print(p); dev.off()
    write_log(paste0("  Saved: ", basename(pdf_out)))
  }

  # ============================================================
  # 2. Edge Density（violin + boxplot）
  # ============================================================
  ed <- load_data("03_edge_density")
  if (!is.null(ed) && nrow(ed) > 0) {
    p <- ggplot(ed, aes(x = library, y = ED)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = (nrow(ed) > 5)) +
      labs(x = "Library", y = "Edge Density") +
      my_plot2
    pdf_out <- file.path(save_path, paste0(name, "_05_edge_density.pdf"))
    pdf(pdf_out); print(p); dev.off()
    write_log(paste0("  Saved: ", basename(pdf_out)))
  }

  # ============================================================
  # 3. UMI/UEI 数（violin + boxplot）
  # ============================================================
  uu <- load_data("04_umi_uei")
  if (!is.null(uu) && nrow(uu) > 0) {
    p <- ggplot(uu, aes(x = type, y = variation)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = (nrow(uu) > 5)) +
      labs(x = "Type", y = "Count per cluster") +
      scale_y_log10() +
      my_plot2
    pdf_out <- file.path(save_path, paste0(name, "_05_umi_uei.pdf"))
    pdf(pdf_out); print(p); dev.off()
    write_log(paste0("  Saved: ", basename(pdf_out)))
  }

  # ============================================================
  # 4. Ego Size（density plot、クラスターごとに色分け）
  # ============================================================
  es <- load_data("04_ego_size")
  if (!is.null(es) && nrow(es) > 0) {
    p <- ggplot(es, aes(x = value)) +
      geom_density(aes(color = community_id), show.legend = FALSE) +
      labs(x = paste0(name, "_egoSize")) +
      scale_x_log10(limits = c(4, 2000)) +
      ylim(c(0.0, 2.0)) +
      my_plot2
    pdf_out <- file.path(save_path, paste0(name, "_05_ego_size.pdf"))
    pdf(pdf_out); print(p); dev.off()
    write_log(paste0("  Saved: ", basename(pdf_out)))
  }

  # ============================================================
  # 5. Diameter（violin + boxplot）
  # ============================================================
  dm <- load_data("04_diameter")
  if (!is.null(dm) && nrow(dm) > 0) {
    p <- ggplot(dm, aes(x = library, y = diameter)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = (nrow(dm) > 5)) +
      labs(x = "Library", y = "Diameter") +
      my_plot2
    pdf_out <- file.path(save_path, paste0(name, "_05_diameter.pdf"))
    pdf(pdf_out); print(p); dev.off()
    write_log(paste0("  Saved: ", basename(pdf_out)))
  }

  write_log(paste0("[05_plot] DONE: ", Sys.time()))
}

# =============================================================================
# スタンドアロン実行ブロック
# =============================================================================
if (!exists("IBMSEQ_SOURCED")) {
  suppressPackageStartupMessages({
    library(ggplot2)
    library(data.table)
  })

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("Usage: Rscript 05_plot.R <name> <save_path> [--from-tsv]")
  }
  name      <- args[1]
  save_path <- args[2]
  from_tsv  <- "--from-tsv" %in% args

  run_plots(name, save_path, from_tsv)
}
