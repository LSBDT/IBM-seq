# =============================================================================
# 06_combine_plot.R
# 複数サンプルの RDS/TSV を一括読み込みし、サンプル比較図（PDF）を生成
#
# 使い方:
#   # 同一ディレクトリ内の複数サンプル
#   Rscript 06_combine_plot.R <rds_dir> <name1,name2,...> [オプション ...]
#
#   # 異なるディレクトリのサンプル
#   Rscript 06_combine_plot.R --out=<out_dir> <dir1>:<name1> [<dir2>:<name2> ...] [オプション ...]
#
# オプション:
#   --out=path       : 出力ディレクトリ（省略時は rds_dir）
#   --prefix=str     : 出力ファイルのプレフィックス（デフォルト: combined）
#   --from-tsv       : RDS の代わりに TSV ファイルから読み込む
#   --min-size=N     : cluster_size プロットの最小クラスターサイズ閾値（デフォルト: 0 = 全クラスター）
#
# 出力:
#   {out_dir}/{prefix}_06_cluster_size.pdf
#   {out_dir}/{prefix}_06_edge_density.pdf
#   {out_dir}/{prefix}_06_umi_uei.pdf
#   {out_dir}/{prefix}_06_ego_size.pdf
#   {out_dir}/{prefix}_06_diameter.pdf
#   {out_dir}/{prefix}_06_combine.log
#
# 例:
#   Rscript 06_combine_plot.R /output sampleA,sampleB,sampleC
#   Rscript 06_combine_plot.R /output sampleA,sampleB --out=/output/figs --prefix=exp1 --min-size=1000
#   Rscript 06_combine_plot.R --out=/figs /output/A:sampleA /output/B:sampleB
# =============================================================================

run_combine_plots <- function(entries, out_dir, prefix = "combined",
                              from_tsv = FALSE, min_size = 0L) {
  # entries: list of list(dir = ..., name = ...)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(out_dir, paste0(prefix, "_06_combine.log"))
  writeLines("", log_file)  # 上書き初期化
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[06_combine_plot] START: ", Sys.time()))
  write_log(paste0("  entries: ",
    paste(sapply(entries, function(e) paste0(e$name, " @ ", e$dir)), collapse = "  |  ")))
  write_log(paste0("  out_dir=", out_dir, "  prefix=", prefix,
                   "  from_tsv=", from_tsv, "  min_size=", min_size))

  # ---- テーマ ----
  my_plot2 <- theme_bw() + theme(
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

  names_ordered <- sapply(entries, function(e) e$name)

  # ---- Helper: 1サンプル分の1指標を読み込む ----
  load_one <- function(dir, name, stem) {
    ext  <- if (from_tsv) ".tsv" else ".rds"
    path <- file.path(dir, paste0(name, "_", stem, ext))
    if (!file.exists(path) && !from_tsv) {
      # RDS が見つからなければ TSV にフォールバック
      path <- file.path(dir, paste0(name, "_", stem, ".tsv"))
    }
    if (!file.exists(path)) {
      write_log(paste0("  SKIP (not found): ", path))
      return(NULL)
    }
    if (grepl("\\.rds$", path)) readRDS(path) else fread(path)
  }

  # ---- Helper: 全サンプルを結合し library 列をサンプル名で統一する ----
  load_combine <- function(stem) {
    dfs <- lapply(entries, function(e) {
      df <- load_one(e$dir, e$name, stem)
      if (is.null(df)) return(NULL)
      dt <- as.data.table(df)
      if (nrow(dt) == 0) return(NULL)
      dt[, library := e$name]  # ファイル内の library 列があっても上書き
      dt
    })
    dfs <- dfs[!sapply(dfs, is.null)]
    if (length(dfs) == 0) return(NULL)
    rbindlist(dfs, fill = TRUE)
  }

  # ---- Helper: PDF を保存 ----
  save_pdf <- function(p, stem, pdf_width = 7) {
    out <- file.path(out_dir, paste0(prefix, "_06_", stem, ".pdf"))
    pdf(out, width = pdf_width)
    print(p)
    dev.off()
    write_log(paste0("  Saved: ", basename(out)))
  }

  n <- length(entries)

  # notch は十分なデータ数がある場合のみ有効にする（グループあたり 20 点以上）
  use_notch <- function(dt, group_col) {
    min_n <- min(dt[, .N, by = group_col]$N)
    min_n >= 20
  }

  # ============================================================
  # 1. クラスター規模（violin + boxplot）
  # ============================================================
  ms <- load_combine("03_cluster_size")
  if (!is.null(ms) && nrow(ms) > 0) {
    if (min_size > 0) ms <- ms[total > min_size]
    if (nrow(ms) > 0) {
      ms[, library := factor(library, levels = names_ordered)]
      p <- ggplot(ms, aes(x = library, y = total)) +
        geom_violin() +
        geom_boxplot(width = 0.1, notch = use_notch(ms, "library")) +
        labs(x = "Library", y = "Total Number of Nodes per Cluster") +
        scale_y_log10() +
        my_plot2
      save_pdf(p, "cluster_size", pdf_width = max(7, 2 * n))
    }
  }

  # ============================================================
  # 2. Edge Density（violin + boxplot）
  # ============================================================
  ed <- load_combine("03_edge_density")
  if (!is.null(ed) && nrow(ed) > 0) {
    ed[, library := factor(library, levels = names_ordered)]
    p <- ggplot(ed, aes(x = library, y = ED)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = use_notch(ed, "library")) +
      labs(x = "Library", y = "Edge Density") +
      my_plot2
    save_pdf(p, "edge_density", pdf_width = max(7, 2 * n))
  }

  # ============================================================
  # 3. UMI/UEI（violin + boxplot、type = "{name}_UMI1" 形式）
  # ============================================================
  uu <- load_combine("04_umi_uei")
  if (!is.null(uu) && nrow(uu) > 0) {
    # サンプル順を保ったラベル順を構築
    type_levels <- unlist(lapply(names_ordered, function(nm) {
      paste(nm, c("UMI1", "UMI2", "UEI1", "UEI2"), sep = "_")
    }))
    uu[, type := factor(type, levels = intersect(type_levels, unique(type)))]
    p <- ggplot(uu, aes(x = type, y = variation)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = use_notch(uu, "type")) +
      labs(x = "Type", y = "Count per cluster") +
      scale_y_log10() +
      my_plot2
    save_pdf(p, "umi_uei", pdf_width = max(7, 2 * n))
  }

  # ============================================================
  # 4. Ego Size（density plot、サンプルごとに facet）
  # データ範囲に合わせて x 軸を動的に設定する
  # ============================================================
  es <- load_combine("04_ego_size")
  if (!is.null(es) && nrow(es) > 0) {
    es[, library := factor(library, levels = names_ordered)]
    pos_vals <- es$value[es$value > 0]
    if (length(pos_vals) > 0) {
      x_lo <- floor(log10(max(1, min(pos_vals)))) |> (\(e) 10^e)()
      x_hi <- ceiling(log10(max(pos_vals) * 2))   |> (\(e) 10^e)()
      p <- ggplot(es, aes(x = value)) +
        geom_density(aes(color = as.factor(community_id)), show.legend = FALSE) +
        facet_wrap(~ library, ncol = n) +
        labs(x = "Ego Size (order = 3)") +
        scale_x_log10(limits = c(x_lo, x_hi)) +
        my_plot2
      save_pdf(p, "ego_size", pdf_width = max(7, 4 * n))
    } else {
      write_log("  SKIP ego_size: no positive values to plot")
    }
  }

  # ============================================================
  # 5. Diameter（violin + boxplot）
  # ============================================================
  dm <- load_combine("04_diameter")
  if (!is.null(dm) && nrow(dm) > 0) {
    dm[, library := factor(library, levels = names_ordered)]
    p <- ggplot(dm, aes(x = library, y = diameter)) +
      geom_violin() +
      geom_boxplot(width = 0.1, notch = use_notch(dm, "library")) +
      labs(x = "Library", y = "Diameter") +
      my_plot2
    save_pdf(p, "diameter", pdf_width = max(7, 2 * n))
  }

  write_log(paste0("[06_combine_plot] DONE: ", Sys.time()))
  invisible(NULL)
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
  if (length(args) == 0) {
    cat(paste(
      "Usage (同一ディレクトリ):",
      "  Rscript 06_combine_plot.R <rds_dir> <name1,name2,...> [--out=dir] [--prefix=str] [--from-tsv] [--min-size=N]",
      "",
      "Usage (異なるディレクトリ):",
      "  Rscript 06_combine_plot.R --out=<out_dir> <dir1>:<name1> [<dir2>:<name2> ...] [--prefix=str] [--from-tsv]",
      "",
      "例:",
      "  Rscript 06_combine_plot.R /output sampleA,sampleB,sampleC",
      "  Rscript 06_combine_plot.R /output sampleA,sampleB --out=/output/figs --prefix=exp1",
      "  Rscript 06_combine_plot.R --out=/figs /output/A:sampleA /output/B:sampleB",
      sep = "\n"
    ), file = stderr())
    quit(status = 1)
  }

  prefix     <- "combined"
  from_tsv   <- FALSE
  out_dir    <- NULL
  min_size   <- 0L
  entries    <- list()
  positional <- character(0)

  for (a in args) {
    if (grepl("^--out=", a)) {
      out_dir <- sub("^--out=", "", a)
    } else if (grepl("^--prefix=", a)) {
      prefix <- sub("^--prefix=", "", a)
    } else if (a == "--from-tsv") {
      from_tsv <- TRUE
    } else if (grepl("^--min-size=[0-9]+$", a)) {
      min_size <- as.integer(sub("^--min-size=", "", a))
    } else {
      positional <- c(positional, a)
    }
  }

  if (length(positional) == 0) stop("サンプル指定がありません。")

  # 1つ目の positional が "dir:name" 形式かどうかで分岐
  if (grepl(":", positional[1], fixed = TRUE)) {
    # --- 異なるディレクトリモード ---
    if (is.null(out_dir)) {
      stop("dir:name 形式の場合は --out=<out_dir> を指定してください。")
    }
    for (a in positional) {
      colon <- regexpr(":", a, fixed = TRUE)[1]
      d <- substring(a, 1, colon - 1)
      n <- substring(a, colon + 1)
      entries <- c(entries, list(list(dir = d, name = n)))
    }
  } else {
    # --- 同一ディレクトリモード ---
    if (length(positional) < 2) {
      stop("Usage: Rscript 06_combine_plot.R <rds_dir> <name1,name2,...>")
    }
    rds_dir <- positional[1]
    if (is.null(out_dir)) out_dir <- rds_dir
    nms <- unlist(strsplit(positional[2], ","))
    for (nm in nms) entries <- c(entries, list(list(dir = rds_dir, name = nm)))
    # 3つ目以降は追加サンプル名として扱う
    for (a in positional[-(1:2)]) {
      entries <- c(entries, list(list(dir = rds_dir, name = a)))
    }
  }

  if (length(entries) < 2) {
    stop("比較には 2 サンプル以上を指定してください。")
  }

  run_combine_plots(entries, out_dir, prefix, from_tsv, min_size)
}
