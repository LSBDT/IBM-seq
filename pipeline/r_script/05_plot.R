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
#   Rscript 05_plot.R <name> <save_path> [--from-tsv] [--ego-xlim=min,max]
#
#   --from-tsv: RDS の代わりに TSV ファイルから読み込む
#               計算ステップを再実行せずに図だけ作り直す場合に使用
#   --ego-xlim=min,max: Ego Size プロットの x軸範囲（デフォルト: 4,2000）
#                       例: --ego-xlim=4,1000
# =============================================================================

run_plots <- function(name, save_path, from_tsv = FALSE, ego_xlim = c(4, 2000)) {

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
      scale_x_log10(limits = ego_xlim) +
      ylim(c(0.0, 2.0)) +
      my_plot2
    pdf_out <- file.path(save_path, paste0(name, "_05_ego_size.pdf"))
    pdf(pdf_out); print(p); dev.off()
    write_log(paste0("  Saved: ", basename(pdf_out), " (xlim=", paste(ego_xlim, collapse=","), ")"))
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

  # ============================================================
  # 6. 抗体別カウント（Mix データのみ）
  # ============================================================
  # membership から抗体列を抽出し、クラスターごとに抗体別の合計値を集計
  membership_file <- file.path(save_path, paste0(name, "_02_membership.rds"))
  if (file.exists(membership_file)) {
    write_log("  Checking for antibody columns (Mix data)...")
    membership <- readRDS(membership_file)

    # 除外する列（抗体以外のメタデータ列）
    exclude_cols <- c("name", "UEI1", "UEI2", "subgraph_id", "community_id", "node_type")
    all_cols <- names(membership)
    antibody_cols <- setdiff(all_cols, exclude_cols)

    # 抗体列が存在するかチェック（Mix データの場合のみ）
    if (length(antibody_cols) > 0) {
      write_log(paste0("  Found ", length(antibody_cols), " antibody columns: ",
                       paste(head(antibody_cols, 5), collapse = ", "),
                       if (length(antibody_cols) > 5) "..." else ""))

      # 抗体名を抽出（.t1, .t2 を除去してグループ化）
      # 例: CTCF.t1, CTCF.t2 → CTCF
      antibody_names <- unique(gsub("\\.(t1|t2|m1)$", "", antibody_cols))

      # 各クラスターごとに抗体別の合計を集計
      antibody_counts_list <- lapply(antibody_names, function(ab) {
        # この抗体に関連する列（例: CTCF.t1, CTCF.t2）
        ab_cols <- grep(paste0("^", ab, "\\.(t1|t2|m1)$"), antibody_cols, value = TRUE)
        if (length(ab_cols) == 0) return(NULL)

        # クラスターごとに合計
        membership_dt <- as.data.table(membership)
        agg <- membership_dt[, .(count = sum(.SD, na.rm = TRUE)),
                             by = community_id,
                             .SDcols = ab_cols]
        agg[, antibody := ab]
        agg[, library := name]
        agg[count > 0]  # カウント0のクラスターは除外
      })

      antibody_counts <- rbindlist(antibody_counts_list[!sapply(antibody_counts_list, is.null)])

      if (!is.null(antibody_counts) && nrow(antibody_counts) > 0) {
        write_log(paste0("  Aggregated antibody counts: ", nrow(antibody_counts), " rows"))

        # violin + boxplot（抗体別）
        p <- ggplot(antibody_counts, aes(x = antibody, y = count)) +
          geom_violin() +
          geom_boxplot(width = 0.1, notch = (nrow(antibody_counts) > 5)) +
          labs(x = "Antibody", y = "Count per Cluster") +
          scale_y_log10() +
          my_plot2

        pdf_out <- file.path(save_path, paste0(name, "_05_antibody_counts.pdf"))
        pdf(pdf_out); print(p); dev.off()
        write_log(paste0("  Saved: ", basename(pdf_out)))

        # TSV出力（オプション）
        tsv_out <- file.path(save_path, paste0(name, "_05_antibody_counts.tsv"))
        fwrite(antibody_counts, tsv_out, sep = "\t")
        write_log(paste0("  Saved: ", basename(tsv_out)))
      } else {
        write_log("  No antibody counts to plot (all zero)")
      }
    } else {
      write_log("  No antibody columns found (Single data, skipping antibody plot)")
    }
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
    stop("Usage: Rscript 05_plot.R <name> <save_path> [--from-tsv] [--ego-xlim=min,max]")
  }
  name      <- args[1]
  save_path <- args[2]
  from_tsv  <- "--from-tsv" %in% args
  ego_xlim  <- c(4, 2000)  # デフォルト

  # --ego-xlim=min,max オプション解析
  for (a in args[-(1:2)]) {
    if (grepl("^--ego-xlim=", a)) {
      vals <- as.numeric(strsplit(sub("^--ego-xlim=", "", a), ",")[[1]])
      if (length(vals) == 2 && all(!is.na(vals))) {
        ego_xlim <- vals
      } else {
        stop("--ego-xlim requires two numeric values: --ego-xlim=min,max")
      }
    }
  }

  run_plots(name, save_path, from_tsv, ego_xlim)
}
