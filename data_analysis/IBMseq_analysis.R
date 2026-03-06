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


cycle_rank_per_community <- function(graph, membership, threshold) {
  membership_cut <- membership %>%
    count(community_id) %>%
    filter(n > threshold) %>%
    select(community_id) %>%
    inner_join(membership, by = "community_id") %>%
    select(subgraph_id, community_id)
  
  res_list <- lapply(unique(membership_cut$community_id), function(i) {
    sub_id <- membership_cut %>%
      filter(community_id == i) %>%
      distinct(subgraph_id) %>%
      pull(subgraph_id)
    
    vid <- V(graph)[V(graph)$community_id == i]
    n_nodes <- length(vid)
    
    subgraph <- induced_subgraph(graph, vids = vid)
    
    m_edge <- ecount(subgraph)
    c_comp <- components(subgraph)$no
    cycle_rank <- m_edge - n_nodes + c_comp
    
    data.frame(
      subgraph_id = if (length(sub_id) > 0) sub_id[1] else NA,
      community_id = i,
      n_nodes = n_nodes,
      edge_count = m_edge,
      n_components = c_comp,
      cycle_rank = cycle_rank
    )
  })
  
  bind_rows(res_list)
}

tree_likeness_per_community <- function(graph, membership, threshold) {
  
  membership_cut <- membership %>%
    count(community_id) %>%
    filter(n > threshold) %>%
    select(community_id) %>%
    inner_join(membership, by = "community_id") %>%
    select(subgraph_id, community_id)
  
  res_list <- lapply(unique(membership_cut$community_id), function(i) {
    
    sub_id <- membership_cut %>%
      filter(community_id == i) %>%
      distinct(subgraph_id) %>%
      pull(subgraph_id)
    
    vid <- V(graph)[V(graph)$community_id == i]
    n_nodes <- length(vid)
    
    subgraph <- induced_subgraph(graph, vids = vid) %>%
      simplify(remove.multiple = TRUE, remove.loops = TRUE)
    
    c_comp <- components(subgraph)$no
    
    if (c_comp > 1) {
      density <- NA_real_
      tree_theoretical_density <- NA_real_
      ratio <- NA_real_
    } else {
      density <- edge_density(subgraph)
      tree_theoretical_density <- if (n_nodes > 0) 2 / n_nodes else NA_real_
      ratio <- tree_theoretical_density / density
    }
    
    data.frame(
      subgraph_id = dplyr::first(sub_id, default = NA),
      community_id = i,
      n_nodes = n_nodes,
      n_components = c_comp,
      density = density,
      tree_theoretical_density = tree_theoretical_density,
      tree_likeness_ratio = ratio
    )
  })
  
  bind_rows(res_list)
}


