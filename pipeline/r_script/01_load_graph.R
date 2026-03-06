# =============================================================================
# 01_load_graph.R
# データ読み込み・グラフ構築
#
# Input : {read_path}/{name}.link.gz
#         カラム: from, to, Target1, Target2, count
#
# Output: {save_path}/{name}_01_graph.rds
#         {save_path}/{name}_01_summary.txt
#
# 使い方（スタンドアロン）:
#   Rscript 01_load_graph.R <name> <read_path> <save_path> [--no-dup|--with-dup]
#   デフォルト: --no-dup（重複エッジを除去）
# =============================================================================

load_graph <- function(name, read_path, save_path, dup_mode = "--no-dup") {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[01_load_graph] START: ", Sys.time()))
  write_log(paste0("  name=", name, "  dup_mode=", dup_mode))

  # ---- ファイル存在確認 ----
  links_file <- file.path(read_path, paste0(name, ".link.gz"))
  if (!file.exists(links_file)) {
    stop(paste("File not found:", links_file))
  }

  # ---- 読み込み ----
  write_log("  Reading link file...")
  links_data <- fread(links_file, header = FALSE)
  colnames(links_data) <- c("from", "to", "Target1", "Target2", "count")
  write_log(paste0("  Rows loaded: ", nrow(links_data)))
  write_log(capture.output(head(links_data, 3)))

  # ---- グラフ構築 ----
  write_log("  Building igraph object...")
  graph <- graph_from_data_frame(d = links_data, directed = FALSE)

  # ---- 重複エッジ処理（ike.R取り込み: .link.gz対応 + dup_modeオプション）----
  # NOTE: edge.attr.comb="first" を指定しないと Target1/Target2 等の属性が
  #       simplify() によってデフォルトで "ignore"（削除）される
  if (dup_mode == "--no-dup") {
    write_log("  Simplifying graph (--no-dup: remove duplicate edges & loops)...")
    graph <- simplify(graph,
                      remove.multiple = TRUE,
                      remove.loops    = TRUE,
                      edge.attr.comb  = "first")
  } else {
    write_log("  Keeping duplicate edges (--with-dup)...")
  }

  # ---- メモリ解放 ----
  rm(links_data)
  gc(verbose = FALSE)

  # ---- グラフ情報 ----
  n_v <- vcount(graph)
  n_e <- ecount(graph)
  n_comp <- components(graph)$no
  write_log(paste0("  Vertices: ", n_v, "  Edges: ", n_e, "  Components: ", n_comp))

  # ---- RDS保存 ----
  rds_out <- file.path(save_path, paste0(name, "_01_graph.rds"))
  saveRDS(graph, rds_out)
  write_log(paste0("  Saved: ", rds_out))

  # ---- テキスト出力（検証用）----
  txt_out <- file.path(save_path, paste0(name, "_01_summary.txt"))
  sink(txt_out)
  cat("=== Graph Summary ===\n")
  print(graph)
  cat("\nVertices       :", n_v, "\n")
  cat("Edges          :", n_e, "\n")
  cat("Is connected   :", is_connected(graph), "\n")
  cat("Components     :", n_comp, "\n")
  cat("dup_mode       :", dup_mode, "\n")
  sink()
  write_log(paste0("  Saved: ", txt_out))
  write_log(paste0("[01_load_graph] DONE: ", Sys.time()))

  invisible(graph)
}

# =============================================================================
# スタンドアロン実行ブロック（00_main.R からsourceされるときは実行しない）
# =============================================================================
if (!exists("IBMSEQ_SOURCED")) {
  suppressPackageStartupMessages({
    library(data.table)
    library(igraph)
  })

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    stop("Usage: Rscript 01_load_graph.R <name> <read_path> <save_path> [--no-dup|--with-dup|--dup-then-dedup]")
  }
  name      <- args[1]
  read_path <- args[2]
  save_path <- args[3]
  # --dup-then-dedup はStep2でdedupするため、Step1では --with-dup と同じ扱い
  dup_mode  <- if (length(args) >= 4 && args[4] %in% c("--no-dup", "--with-dup", "--dup-then-dedup")) {
    if (args[4] == "--dup-then-dedup") "--with-dup" else args[4]
  } else "--no-dup"

  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  load_graph(name, read_path, save_path, dup_mode)
}
