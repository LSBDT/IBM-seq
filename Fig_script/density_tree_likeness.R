rm(list = ls())
library(igraph)
library(ggplot2)
library(dplyr)
sessionInfo()
#updata 2026/3/04

#########DATA#######
# ディレクトリの指定
args <- commandArgs(trailingOnly = TRUE)
#file name
names <- unlist(strsplit(args[1], ","))
#threshold
threshold <- as.numeric(args[2])
#path
read_path <- args[3]
#save_path
save_path <- args[4]

for (name in names) {
  # name と path の生成
  rds_graph_name <- sprintf("%s_02_graph.rds", name)
  rds_membership_name <- sprintf("%s_02_membership.rds", name)
  rds_graph_path <- file.path(read_path, rds_graph_name)
  rds_membership_path<-file.path(read_path, rds_membership_name)
  
  if (file.exists(rds_graph_path) && file.exists(rds_membership_path)) {
    # rdsファイルの読み込み
    rds_graph <- readRDS(rds_graph_path)
    rds_membership<-readRDS(rds_membership_path)
    
    #community内ノード数でcutする
    rds_membership_cut <- rds_membership %>%
      count(community_id) %>% 
      filter(n > threshold) %>%
      select(community_id) %>% 
      inner_join(rds_membership,by = "community_id") %>%
      select(subgraph_id, community_id)
    
    # 結果保存用データフレーム
    density_result_percom <- data.frame(
      subgraph_id = character(),
      community_id = character(),
      n_nodes = numeric(),
      density = numeric(),
      tree_theoretical_density=numeric()
    )
    
    
    # 各コミュニティに対して処理
    for (i in unique(rds_membership_cut$community_id)) {
      
      #subgraph_id
      sub_id<-rds_membership_cut %>% filter(community_id == i) %>% select(subgraph_id) %>% unique()
      
      # community_id に対応するノードを選択
      vid <- V(rds_graph)[V(rds_graph)$community_id == i]
      
      #node数
      n_nodes <- length(vid)
      
      # サブグラフを作成
      subgraph <- induced_subgraph(rds_graph, vids = vid)
      # 多重辺（＋自己ループも必要なら）を除去
      subgraph <- simplify(
			   subgraph,
			   remove.multiple = TRUE,
			   remove.loops = TRUE
	   )		   
      
      # density
      density <-edge_density(graph = subgraph)
      
      # diameter
      tree_theoretical_density<-2 / n_nodes

      
      # 結果をデータフレームに追加
      density_result_percom <- rbind(
        density_result_percom,
        data.frame(
          subgraph_id =sub_id,
          community_id = i,
          n_nodes=n_nodes,
          density = density,
          tree_theoretical_density=tree_theoretical_density
        )
      )
    }
    
    # RDSファイルとして保存
    saveRDS(density_result_percom, file = file.path(save_path, paste0(name, "_density_tree_likeness.rds")))
    
    
  } else {
    warning(paste("File not found:", rds_graph_path, "or", rds_membership_path))
  }
}
