# =============================================================================
# 03_density.R
# クラスター規模集計 & Edge Density 計算
#
# Input : {save_path}/{name}_02_graph.rds
#         {save_path}/{name}_02_membership.rds
#
# Output: {save_path}/{name}_03_cluster_size.rds/.tsv  (--no-rds 時は .tsv のみ)
#         {save_path}/{name}_03_cluster_size_large.tsv    ← 閾値以上のみ
#         {save_path}/{name}_03_edge_density.rds/.tsv  (--no-rds 時は .tsv のみ)
#         {save_path}/{name}_03_density_summary.txt
#
# 使い方（スタンドアロン）:
#   Rscript 03_density.R <name> <save_path> [min_cluster_size] [num_cores] [--no-edge-density] [--no-rds]
#   デフォルト: min_cluster_size=1000, num_cores=1, edge_density を計算する, RDS を保存する
# =============================================================================

run_density <- function(name, save_path, min_cluster_size = 1000L, num_cores = 1L,
                        compute_edge_density = TRUE, save_rds = TRUE) {

  log_file <- file.path(save_path, paste0(name, "_process.log"))
  write_log <- function(msg) {
    cat(paste0(msg, "\n"), file = log_file, append = TRUE)
    cat(paste0(msg, "\n"))
  }

  write_log(paste0("[03_density] START: ", Sys.time()))
  write_log(paste0("  min_cluster_size=", min_cluster_size,
                   "  num_cores=", num_cores,
                   "  compute_edge_density=", compute_edge_density,
                   "  save_rds=", save_rds))

  # ---- RDS 読み込み ----
  graph        <- readRDS(file.path(save_path, paste0(name, "_02_graph.rds")))
  df_membership <- readRDS(file.path(save_path, paste0(name, "_02_membership.rds")))
  write_log(paste0("  Loaded: V=", vcount(graph), "  communities=",
                   max(df_membership$community_id, na.rm = TRUE)))

  # ---- クラスター規模集計（data.table で高速集計）----
  write_log("  Calculating cluster sizes...")
  membership_dt        <- as.data.table(df_membership)
  membership_sum       <- membership_dt[, .(total = .N), by = community_id][order(-total)]
  membership_sum_large <- membership_sum[total > min_cluster_size][, library := name]

  n_large <- nrow(membership_sum_large)
  write_log(paste0("  Clusters > ", min_cluster_size, " nodes: ", n_large))
  write_log(capture.output(print(membership_sum_large)))

  # ---- RDS + テキスト保存 ----
  if (save_rds)
    saveRDS(membership_sum, file.path(save_path, paste0(name, "_03_cluster_size.rds")))
  fwrite(as.data.table(membership_sum),
         file.path(save_path, paste0(name, "_03_cluster_size.tsv")), sep = "\t")
  fwrite(as.data.table(membership_sum_large),
         file.path(save_path, paste0(name, "_03_cluster_size_large.tsv")), sep = "\t")
  write_log(paste0("  Saved: ", name, "_03_cluster_size",
                   if (save_rds) ".rds/.tsv" else ".tsv (--no-rds: RDS skipped)"))

  # ---- Edge Density 計算（compute_edge_density=TRUE の場合のみ）----
  if (!compute_edge_density) {
    write_log("  Edge density: skipped (compute_edge_density=FALSE)")
    edge_density_df <- data.table(ED=numeric(0), community_id=integer(0), library=character(0))
  } else if (n_large == 0) {
    write_log(paste0("  WARNING: No clusters with > ", min_cluster_size,
                     " nodes. Edge density skipped."))
    edge_density_df <- data.table(ED=numeric(0), community_id=integer(0), library=character(0))
  } else {
    write_log("  Calculating edge density for large clusters...")
    large_ids    <- membership_sum_large$community_id
    cluster_list <- split(
      membership_dt[community_id %in% large_ids, name],
      membership_dt[community_id %in% large_ids, community_id]
    )
    # mclapply で並列計算（num_cores=1 のときは実質 lapply と同等）
    ed_list <- parallel::mclapply(cluster_list, function(nodes) {
      tryCatch(edge_density(induced_subgraph(graph, nodes)), error = function(e) NA_real_)
    }, mc.cores = num_cores)
    ed_vec <- setNames(unlist(ed_list), names(cluster_list))
    valid  <- !is.na(ed_vec)
    edge_density_df <- data.table(
      ED           = ed_vec[valid],
      community_id = as.integer(names(ed_vec)[valid]),
      library      = name
    )
  }

  if (save_rds)
    saveRDS(edge_density_df, file.path(save_path, paste0(name, "_03_edge_density.rds")))
  fwrite(edge_density_df,  file.path(save_path, paste0(name, "_03_edge_density.tsv")), sep = "\t")
  write_log(capture.output(head(edge_density_df)))
  write_log(paste0("  Saved: ", name, "_03_edge_density",
                   if (save_rds) ".rds/.tsv" else ".tsv (--no-rds: RDS skipped)"))

  # ---- サマリーテキスト ----
  summary_out <- file.path(save_path, paste0(name, "_03_density_summary.txt"))
  sink(summary_out)
  cat("=== Density Summary ===\n")
  cat("min_cluster_size :", min_cluster_size, "\n")
  cat("Total communities:", nrow(membership_sum), "\n")
  cat("Large clusters   :", n_large, "\n\n")
  cat("Cluster size distribution (all):\n")
  print(as.data.frame(membership_sum))
  cat("\nEdge density (large clusters):\n")
  print(edge_density_df)
  sink()
  write_log(paste0("  Saved: ", summary_out))
  write_log(paste0("[03_density] DONE: ", Sys.time()))

  invisible(list(cluster_size = membership_sum, edge_density = edge_density_df))
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
    stop("Usage: Rscript 03_density.R <name> <save_path> [min_cluster_size] [num_cores] [--no-edge-density] [--no-rds]")
  }
  name                 <- args[1]
  save_path            <- args[2]
  min_cluster_size     <- 1000L
  num_cores            <- 1L
  compute_edge_density <- TRUE
  save_rds             <- TRUE

  for (a in args[-(1:2)]) {
    if (a == "--no-edge-density") {
      compute_edge_density <- FALSE
    } else if (a == "--no-rds") {
      save_rds <- FALSE
    } else if (grepl("^--cores=[0-9]+$", a)) {
      num_cores <- as.integer(sub("^--cores=", "", a))
    } else if (grepl("^[0-9]+$", a) && min_cluster_size == 1000L) {
      min_cluster_size <- as.integer(a)
    } else if (grepl("^[0-9]+$", a)) {
      num_cores <- as.integer(a)
    }
  }

  run_density(name, save_path, min_cluster_size, num_cores, compute_edge_density, save_rds)
}
