#!/usr/bin/env Rscript
# =============================================================================
# run_orig_metrics.R
# ike.R 相当の処理を実行し、新スクリプトの指標計算（03/04）を適用して TSV を出力する
#
# ike.R との対応:
#   - graph_from_data_frame()  で読み込み（simplify なし = ike.R の挙動と同等）
#   - 全連結成分を逐次 Louvain（ike.R の community_id 管理ロジックを再現）
#   - 頂点属性テーブルは data.table で構築（dplyr 不要）
#   - 03_density.R / 04_features.R を流用して指標を TSV 出力
#
# Usage:
#   Rscript run_orig_metrics.R <name> <read_path> <save_path> [min_cluster_size] [num_cores]
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(igraph)
  library(parallel)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript run_orig_metrics.R <name> <read_path> <save_path> [min_cluster_size] [num_cores]")
}

name             <- args[1]
read_path        <- args[2]
save_path        <- args[3]
min_cluster_size <- if (length(args) >= 4) as.integer(args[4]) else 1000L
num_cores        <- if (length(args) >= 5) as.integer(args[5]) else 1L

dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(save_path, paste0(name, "_process.log"))
writeLines("", log_file)
write_log <- function(msg) {
  cat(paste0(msg, "\n"), file = log_file, append = TRUE)
  cat(paste0(msg, "\n"))
}

cat(paste0("=== orig (ike.R equivalent) START: ", Sys.time(), " ===\n"))
cat(paste0("  name=", name, "  min_cluster_size=", min_cluster_size,
           "  num_cores=", num_cores, "\n\n"))

# ============================================================
# Step 1: データ読み込み（simplify なし = ike.R 相当）
# ============================================================
write_log(paste0("[orig_step1] START: ", Sys.time()))

links_file <- file.path(read_path, paste0(name, ".link.gz"))
if (!file.exists(links_file)) stop(paste("Not found:", links_file))

links_data <- fread(links_file, header = FALSE)
setnames(links_data, c("from", "to", "Target1", "Target2", "count"))
write_log(paste0("  Rows loaded: ", nrow(links_data)))

# graph_from_data_frame で構築（ike.R の graph.data.frame と等価）
# simplify() しない → 重複エッジを保持（ike.R 相当）
graph <- graph_from_data_frame(
  d = links_data[, .(from, to, Target1, Target2, count)],
  directed = FALSE
)
write_log(paste0("  V=", vcount(graph), "  E=", ecount(graph), "  (no simplify)"))
rm(links_data); gc(verbose = FALSE)

# 頂点属性テーブル構築（ike.R の dplyr 処理を data.table で代替）
df      <- igraph::as_data_frame(graph, "both")
df_from <- data.frame(to = df$edges[["from"]], Target2 = df$edges[["Target1"]],
                      stringsAsFactors = FALSE)
df_to   <- df$edges[, c("to", "Target2"), drop = FALSE]
df_list <- rbind(df_from, df_to)

test_dt   <- setDT(df_list)[, .(Count = .N), by = .(to, Target2)]
test_wide <- dcast(test_dt, to ~ Target2, value.var = "Count", fill = 0L)
df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                     by.x = "name", by.y = "to", all.x = TRUE)
for (col in setdiff(names(df$vertices), "name")) {
  df$vertices[[col]][is.na(df$vertices[[col]])] <- 0L
}
rm(df_from, df_to, df_list, test_dt, test_wide); gc(verbose = FALSE)

write_log(paste0("[orig_step1] DONE: ", Sys.time()))

# ============================================================
# Step 2: Louvain クラスタリング（ike.R 相当: 全コンポーネント逐次）
# ============================================================
write_log(paste0("[orig_step2] START: ", Sys.time()))

comp     <- components(graph)
clusters <- comp$membership
n_comp   <- comp$no
V(graph)$subgraph_id <- clusters
write_log(paste0("  Connected components: ", n_comp))

subgraph_list <- split(seq_along(clusters), clusters)
community_id  <- rep(NA_integer_, vcount(graph))
offset        <- 0L

for (i in seq_along(subgraph_list)) {
  nodes <- subgraph_list[[i]]
  subg  <- induced_subgraph(graph, nodes)
  lv    <- cluster_louvain(subg)
  rm(subg)
  comms               <- as.integer(membership(lv)) + offset
  offset              <- offset + max(as.integer(membership(lv)))
  community_id[nodes] <- comms
  if (i %% 500 == 0) {
    write_log(paste0("    Progress: ", i, "/", n_comp))
    gc(verbose = FALSE)
  }
}
V(graph)$community_id <- community_id

df_membership <- cbind(df$vertices, subgraph_id = clusters, community_id = community_id)
n_comm <- max(community_id, na.rm = TRUE)
write_log(paste0("  Total communities: ", n_comm))
write_log(capture.output(head(df_membership)))

# 03_density.R / 04_features.R が読む命名規則で保存
saveRDS(graph,         file.path(save_path, paste0(name, "_02_graph.rds")))
saveRDS(df_membership, file.path(save_path, paste0(name, "_02_membership.rds")))
saveRDS(list(mode = "orig_ike_equivalent", n_comp = n_comp, n_comm = n_comm),
        file.path(save_path, paste0(name, "_02_community.rds")))
fwrite(as.data.table(df_membership),
       file.path(save_path, paste0(name, "_02_membership.tsv")), sep = "\t")

write_log(paste0("[orig_step2] DONE: ", Sys.time()))

# ============================================================
# Step 3 + 4: 新スクリプトの指標計算を流用
# ============================================================
IBMSEQ_SOURCED <- TRUE
script_args <- commandArgs(trailingOnly = FALSE)
script_path  <- sub("--file=", "", script_args[grep("--file=", script_args)])
script_dir   <- if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()

source(file.path(script_dir, "03_density.R"))
source(file.path(script_dir, "04_features.R"))

cat("\n--- Step 3: Density ---\n")
run_density(name, save_path, min_cluster_size, num_cores)

cat("\n--- Step 4: Features ---\n")
run_features(name, save_path, min_cluster_size, num_cores)

cat(paste0("\n=== orig (ike.R equivalent) DONE: ", Sys.time(), " ===\n"))
cat(paste0("Output: ", save_path, "\n"))
