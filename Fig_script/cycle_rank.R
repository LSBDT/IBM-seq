rm(list = ls())
library(igraph)
library(ggplot2)
library(dplyr)
sessionInfo()
#updata 2026/3/4

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
      edge_count=numeric(),
      n_components=numeric(),
      cycle_rank=numeric()
    )
    
    
    # 各コミュニティに対して処理
    for (i in unique(rds_membership_cut$community_id)) {
      
      #subgraph_id
      sub_id<-rds_membership_cut %>% filter(community_id == i) %>% select(subgraph_id) %>% unique()
      
      vid <- V(rds_graph)[V(rds_graph)$community_id == i]
      n_nodes <- length(vid)
      
      subgraph <- simplify(subgraph0, remove.multiple = TRUE, remove.loops = TRUE)
      m_edge <- ecount(subgraph)
      
      c_comp <- components(subgraph)$no
      cycle_rank <- m_edge - n_nodes + c_comp
      
      
      # 結果をデータフレームに追加
      density_result_percom <- rbind(
        density_result_percom,
        data.frame(
          subgraph_id =sub_id,
          community_id = i,
          n_nodes = n_nodes,
          edge_count = m_edge,
          n_components = c_comp,
          cycle_rank = cycle_rank
        )
      )
    }
    
    # RDSファイルとして保存
    saveRDS(density_result_percom, file = file.path(save_path, paste0(name, "_cycle_rank.rds")))
    
    
  } else {
    warning(paste("File not found:", rds_graph_path, "or", rds_membership_path))
  }
}
