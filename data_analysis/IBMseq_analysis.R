library(igraph)
library(dplyr)
##update 2026/03/04

#############################
# 1. subgraph内ノード数が4より多いsubgraphとコミュニティ情報の抽出
get_cut_subgraph <- function(rds) {
  rds %>%
    count(subgraph_id) %>% 
    filter(n > 4) %>%
    select(subgraph_id) %>% 
    inner_join(rds,by = "subgraph_id") %>%
    select(subgraph_id, community_id)
}

# 2. コミュニティ内ノード数をカウントする関数 
get_community_node_count <- function(cut_subgraph) {
  cut_subgraph %>%
    count(subgraph_id, community_id)
}

# 3. 各サブグラフごとのコミュニティ数をカウントする関数
get_community_per_subgraph <- function(community_node_count) {
  community_node_count %>%
    group_by(subgraph_id) %>%
    summarise(num_community = n(), .groups = "drop") 
}

# 4. サブグラフの数をカウントする関数
get_total_subgraph <- function(rds) {
  total_subgraph <- n_distinct(rds$subgraph_id)
  
  total_subgraph_cut4 <- rds %>%
    count(subgraph_id) %>%
    filter(n > 4) %>%
    nrow()
  
  tibble(
    total_subgraph = total_subgraph,
    total_subgraph_cut4 = total_subgraph_cut4
  )
}

##閾値を設定した際のcluster数/subgraph数をカウントする関数
count_communities_by_threshold <- function(df, start, end, by) {
  
  thresholds <- seq(start, end, by = by)
  
  lapply(thresholds, function(n_threshold) {
    df %>%
      dplyr::group_by(library) %>%
      dplyr::filter(.data$n > n_threshold) %>%
      dplyr::summarise(
        communitynumber = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(n_threshold = n_threshold)
  }) %>%
    dplyr::bind_rows()
}


count_subgraphs_by_threshold <- function(df, start, end, by) {
  
  thresholds <- seq(start, end, by = by)
  
  lapply(thresholds, function(n_threshold) {
    df %>%
      dplyr::group_by(library) %>%
      dplyr::filter(.data$n > n_threshold) %>%
      dplyr::summarise(
        subgraphnumber = dplyr::n_distinct(subgraph_id),
        .groups = "drop"
      ) %>%
      dplyr::mutate(n_threshold = n_threshold)
  }) %>%
    dplyr::bind_rows()
}
  
