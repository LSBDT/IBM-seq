library(data.table)
library(dplyr)
library(tidyverse)
library(igraph)
library(ggplot2)
library(reshape2)
sessionInfo()

args <- commandArgs(trailingOnly = TRUE)
name <- args[1]
read_path <- args[2]
save_path <- args[3]

write_log <- function(message) {
    cat(message, file = paste0(name,"_process_log.txt"), append = TRUE, sep = "\n") #名前
  }
  # データを処理する関数
 # process_data <- function(name) {
    # データの読み込み/link.gz fileの読み込みにする
links_file <- file.path(read_path,paste0(name, ".link.gz"))
if (!file.exists(links_file)) {
      stop(paste("File", links_file, "does not exist or is non-readable."))
  }
links_data <- fread(links_file, header = FALSE)
colnames(links_data) <- c("from", "to", "Target1", "Target2", "count")
    
# グラフの作成
graph <- graph.data.frame(d = links_data, directed = FALSE)
    
    # デバッグ: 読み込んだデータとグラフの情報を表示
    cat("Processing:", name, "\n")
    cat("Links data:\n")
    print(head(links_data))
    cat("Graph summary:\n")
    print(graph)
    
    # デバッグ: 読み込んだデータとグラフの情報を表示
    write_log(paste("Processing:", name))
    write_log("Links data:")
    write_log(capture.output(head(links_data)))
    write_log("Graph summary:")
    write_log(capture.output(graph))
    
    # データフレーム変換と処理
    df <- igraph::as_data_frame(graph, "both")
    df_list <- df$edges[, c(1, 3)]
    colnames(df_list) <- c("to", "Target2")
    df_list <- rbind(df_list, df$edges[, c(2, 4)])
    
    write_log("Check for NA in df_list:")
    write_log(capture.output(summary(df_list)))
    # 各頂点にテーブルを追加
    test <- setDT(df_list)[, .(Count = .N), by = .(to, Target2)]
    write_log(paste("Processing2:", name))
    # データを横持ちに変換
    test_wide <- test %>% pivot_wider(names_from = "Target2", values_from = "Count")
    test_wide[is.na(test_wide)] <- 0
    write_log(paste("Processing3:", name))
    # データフレームを更新
    df$vertices <- df$vertices %>%
      left_join(test_wide, by = c("name" = "to"))
    write_log(paste("Processing4:", name))
    ##subgraph/community part
    # 連結成分を取得
    clusters <- components(graph)$membership
    V(graph)$subgraph_id <- clusters
    
    # 連結成分ごとにサブグラフを作成
    subgraph_list <- split(seq_along(clusters), clusters)
    subgraphs <- lapply(subgraph_list, function(nodes) induced_subgraph(graph, nodes))
    
    # 各サブグラフに対して Louvain 法を適用
    louvain_results <- lapply(subgraphs, cluster_louvain)
    community_list <- lapply(louvain_results, membership) 
    community_id <- rep(NA, vcount(graph))
    offset <- 0
    for (i in seq_along(community_list)) {
      nodes <- subgraph_list[[i]]
      comms <- community_list[[i]]+offset
      offset <- offset + max(community_list[[i]])
      community_id[nodes] <- comms
    }
    V(graph)$community_id <- community_id
    
    df_membership <- cbind(df$vertices,subgraph_id = clusters, community_id= community_id)
    #df_membership$sum <- rowSums(df_membership[, 2:5], na.rm = TRUE) #抗体の数で変わる。6種類x6種類なら2:13
    
    # デバッグ: クラスタリング結果の表示
    print(head(df_membership))
    write_log("Membership data:")
    write_log(capture.output(head(df_membership)))
    
    # 保存
    saveRDS(df_membership, file = file.path(save_path, paste0(name, "_data_membership.rds")))
    saveRDS(graph,         file = file.path(save_path, paste0(name, "_data_graph.rds")))
    saveRDS(louvain_results, file = file.path(save_path, paste0(name, "_data_community.rds")))
    
    write_log("finish")
  

